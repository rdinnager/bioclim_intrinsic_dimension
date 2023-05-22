library(ENMTools)
library(terra)
library(disdat)
library(tidyverse)
library(sf)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

regions <- c("AWT", "NSW", "CAN", "NZ", "SA", "SWI")

pca_rast <- rast("data/bioclim_pca.tif")[[1:10]]
bioclim_scaled_rast <- rast("data/bioclim_scaled.tif")
bioman_rast <- c(rast("data/bioclim_manifold_1.tif"),
                 rast("data/bioclim_manifold_2.tif"),
                 rast("data/bioclim_manifold_3.tif"),
                 rast("data/bioclim_manifold_4.tif"),
                 rast("data/bioclim_manifold_5.tif"))

POs <- map(regions, disPo)
BGs <- map(regions, disBg)
borders <- map(regions, disBorder)
CRSs <- map(regions, ~ crs(disBorder(.x)))

PO <- POs[[1]]
BG <- BGs[[1]]
border <- borders[[1]]
CRS <- CRSs[[1]]

run_SDM <- function(PO, BG, border, CRS) {

  border_proj <- st_transform(border, crs(rast))
  env_bioclim <- crop(bioclim_scaled_rast, border_proj)
  env_bioclim <- project(env_bioclim, CRS)
  env_pca <- crop(pca_rast, border_proj)
  env_pca <- project(env_pca, CRS)
  env_bioman <- crop(bioman_rast, border_proj)
  env_bioman <- project(env_bioman, CRS)

  extract_model_evals <- function(mod_fun, enm_ob, test_prop = 0.1, num_iter = 10) {
    evals <- list()
    for(i in seq_len(num_iter)) {
      bc <- mod_fun(enm_ob, env_bioclim, test.prop = test_prop)
      pc <- mod_fun(enm_ob, env_pca, test.prop = test_prop)
      bm <- mod_fun(enm_ob, env_bioman, test.prop = test_prop)
      ev <- list(
        bc = list(test = bc$test.evaluation, train = bc$training.evaluation,
                  env_test = bc$env.test.evaluation, env_train = bc$env.training.evaluation),
        pc = list(test = pc$test.evaluation, train = pc$training.evaluation,
                  env_test = pc$env.test.evaluation, env_train = pc$env.training.evaluation),
        bm = list(test = bm$test.evaluation, train = bm$training.evaluation,
                  env_test = bm$env.test.evaluation, env_train = bm$env.training.evaluation)
      )
      evals[[i]] <- ev
    }

    evals

  }

  get_models <- function(spec) {

    enm_ob <- enmtools.species(rast(vect(border)),
                             PO %>% filter(spid == spec) %>% select(lon = x, lat = y) %>%
                               vect(crs = CRS),
                             BG %>% select(lon = x, lat = y) %>%
                               vect(crs = CRS),
                             spec)

    mod_rf <- extract_model_evals(enmtools.rf.ranger, enm_ob)
    mod_glm <- extract_model_evals(enmtools.glm, enm_ob)
    mod_gam <- extract_model_evals(enmtools.gam, enm_ob)
    mod_bc <- extract_model_evals(enmtools.bc, enm_ob)

  }

  specs <- unique(PO$spid)

}




