#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pc_file
#' @param borders
#' @param CRSs
crop_envs <- function(file, borders, CRSs) {

  if(length(file) > 1) {
    rast1 <- map(file, rast) %>%
      do.call(c, .)
  } else {
    rast1 <- rast(file)
  }
  border_proj <- st_transform(borders, crs(rast1))
  env <- crop(rast1, border_proj)
  env <- project(env, CRSs)
  wrap(env)

}


get_models <- function(dat, spec) {
    #browser()
    c(po_df, border, bg, crs, env_pc, env_bc, env_bm, spec) %<-% dat

    env_pc <- unwrap(env_pc)
    env_bc <- unwrap(env_bc)
    env_bm <- unwrap(env_bm)

    enm_ob <- enmtools.species(rast(vect(border)),
                             po_df %>%
                               vect(crs = crs),
                             bg %>% dplyr::select(lon = x, lat = y) %>%
                               vect(crs = crs),
                             spec)

    mod_rf <- extract_model_evals(enmtools.rf.ranger, enm_ob, env_bc, env_pc, env_bm)
    mod_glm <- extract_model_evals(enmtools.glm, enm_ob, env_bc, env_pc, env_bm)
    #mod_gam <- extract_model_evals(enmtools.gam, enm_ob, env_bc, env_pc, env_bm)

    mod_bc <- extract_model_evals(enmtools.bc, enm_ob, env_bc, env_pc, env_bm)

    list(rf = mod_rf, glm = mod_glm, bc = mod_bc)

}

get_models_block <- function(dat, spec) {

    c(po_df, border, bg, crs, env_pc, env_bc, env_bm, spec) %<-% dat

    env_pc <- unwrap(env_pc)
    env_bc <- unwrap(env_bc)
    env_bm <- unwrap(env_bm)

    enm_ob <- enmtools.species(rast(vect(border)),
                             po_df %>%
                               vect(crs = crs),
                             bg %>% dplyr::select(lon = x, lat = y) %>%
                               vect(crs = crs),
                             spec)

    mod_rf <- extract_model_evals_block(enmtools.rf.ranger, enm_ob, env_bc, env_pc, env_bm)
    mod_glm <- extract_model_evals_block(enmtools.glm, enm_ob, env_bc, env_pc, env_bm)
    #mod_gam <- extract_model_evals(enmtools.gam, enm_ob, env_bc, env_pc, env_bm)

    mod_bc <- extract_model_evals_block(enmtools.bc, enm_ob, env_bc, env_pc, env_bm)

    list(rf = mod_rf, glm = mod_glm, bc = mod_bc)

}

extract_model_evals <- function(mod_fun, enm_ob, env_bc, env_pc, env_bm, test_prop = 0.1, num_iter = 10) {

  evals <- list()
    for(i in seq_len(num_iter)) {
      bc <- mod_fun(enm_ob, env_bc, test.prop = test_prop)
      pc <- mod_fun(enm_ob, env_pc, test.prop = test_prop)
      bm <- mod_fun(enm_ob, env_bm, test.prop = test_prop)
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

extract_model_evals_block <- function(mod_fun, enm_ob, env_bc, env_pc, env_bm, num_iter = 10) {

    evals <- list()
    for(i in seq_len(num_iter)) {
      bc <- mod_fun(enm_ob, env_bc, test.prop = "block")
      pc <- mod_fun(enm_ob, env_pc, test.prop = "block")
      bm <- mod_fun(enm_ob, env_bm, test.prop = "block")
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
