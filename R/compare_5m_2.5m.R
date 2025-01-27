library(tidyverse)
library(terra)
library(guildai)
library(alignProMises)

assignInNamespace("find_guild", guildai:::find_r_guildai_guild, ns = "guildai")

Sys.setenv(GUILD_HOME = "/blue/rdinnage.fiu/rdinnage.fiu/.guild") #"/home/rdinnage.fiu/.guild") #"/blue/rdinnage.fiu/rdinnage.fiu/.guild")

runs <- runs_info() |>
  filter(status == "completed",
         tags == "ident_lat_lon_weighted")

runs_5 <- runs_info() |>
  filter(status == "completed",
         tags == "ident_lat_lon_weighted",
         scalars$active_dims == 5)
dirs <- runs_5$dir
files <- file.path(dirs, "output/vae_manifold_vars.csv")

bioman_2.5_run_dat <- map(files, read_csv)

bioman_2.5_all_5 <- bioman_2.5_run_dat[[1]] |>
  rename_with(~paste0(., "_run1"), c(-x, -y))

for(i in 2:length(bioman_2.5_run_dat)) {
  bioman_2.5_all_5 <- bioman_2.5_all_5 |>
    left_join(bioman_2.5_run_dat[[i]] |>
                rename_with(~paste0(., "_run", i),
                            c(-x, -y)),
              by = c("x", "y"))
}

samp <- sample.int(nrow(bioman_2.5_run_dat[[1]]), 10000)
plot(abs(bioman_2.5_run_dat[[1]]$y[samp]), bioman_2.5_run_dat[[1]]$manifold_1[samp])
plot(abs(bioman_2.5_run_dat[[1]]$y[samp]), bioman_2.5_run_dat[[1]]$manifold_2[samp])
plot(abs(bioman_2.5_run_dat[[1]]$y[samp]), bioman_2.5_run_dat[[1]]$manifold_3[samp])
plot(abs(bioman_2.5_run_dat[[1]]$y[samp]), bioman_2.5_run_dat[[1]]$manifold_4[samp])
plot(abs(bioman_2.5_run_dat[[1]]$y[samp]), bioman_2.5_run_dat[[1]]$manifold_5[samp])

plot(bioman_2.5_run_dat[[3]]$y[samp], bioman_2.5_run_dat[[3]]$manifold_1[samp])
plot(bioman_2.5_run_dat[[3]]$y[samp], bioman_2.5_run_dat[[3]]$manifold_2[samp])
plot(bioman_2.5_run_dat[[3]]$y[samp], bioman_2.5_run_dat[[3]]$manifold_3[samp])
plot(bioman_2.5_run_dat[[3]]$y[samp], bioman_2.5_run_dat[[3]]$manifold_4[samp])
plot(bioman_2.5_run_dat[[3]]$y[samp], bioman_2.5_run_dat[[3]]$manifold_5[samp])

plot(abs(bioman_2.5_run_dat[[4]]$y[samp]), bioman_2.5_run_dat[[4]]$manifold_5[samp])

runs_i <- 1:length(files)
bioman_mats <- list()

for(i in runs_i) {
  bioman_mats[[i]] <- bioman_2.5_all_5 |>
    select(ends_with(paste0("_run", i))) |>
    as.matrix()
}

combs <- expand.grid(bioman1 = 1:5, bioman2 = 1:5)
cors <- apply(combs, 1, function(x) cor(bioman_mats[[1]][ , x[1]], bioman_mats[[5]][ , x[2]]))
combs <- combs |>
  mutate(corr = cors)

cors <- apply(combs, 1, function(x) cor(bioman_mats[[1]][ , x[1]], bioman_mats[[4]][ , x[2]]))
combs <- combs |>
  mutate(corr = cors)

cors <- apply(combs, 1, function(x) cor(bioman_mats[[1]][ , x[1]], bioman_mats[[6]][ , x[2]]))
combs <- combs |>
  mutate(corr = cors)

samp <- sample.int(nrow(bioman_mats[[1]]), 1000)
plot(bioman_mats[[1]][samp, 5], bioman_mats[[1]][samp, 1])

cor_1_2 <- outer(bioman_mats[[1]], bioman_mats[[2]],
                 cor)

bioman_2.5_aligned_1_2 <- ProMisesModel(bioman_mats[c(2, 1)],
                                    maxIt = 100,
                                    t = 1/1000,
                                    k = 1)
cors <- apply(combs, 1, function(x) cor(bioman_2.5_aligned_1_2$Xest[ , x[1], 1], bioman_2.5_aligned_1_2$Xest[ , x[2], 2]))
combs <- combs |>
  mutate(corr = cors)

bioman_2.5_aligned <- ProMisesModel(bioman_mats,
                                    maxIt = 100,
                                    t = 1/1000,
                                    k = 1)

plot(bioman_2.5_aligned$dist, type = 'l')
plot(bioman_2.5_aligned$dist, type = 'l', ylim = c(0, 100))
plot(bioman_2.5_aligned$dist, type = 'l', ylim = c(0, 10))
plot(bioman_2.5_aligned$dist, type = 'l', ylim = c(0, 1))

bioman_2.5_aligned$dist[bioman_2.5_aligned$count]

cor(bioman_2.5_aligned$Xest[ , 1, 1], bioman_2.5_aligned$Xest[ , 1, 3])
cor(bioman_2.5_aligned$Xest[ , 2, 1], bioman_2.5_aligned$Xest[ , 2, 3])
cor(bioman_2.5_aligned$Xest[ , 3, 1], bioman_2.5_aligned$Xest[ , 3, 3])
cor(bioman_2.5_aligned$Xest[ , 4, 1], bioman_2.5_aligned$Xest[ , 4, 3])
cor(bioman_2.5_aligned$Xest[ , 5, 1], bioman_2.5_aligned$Xest[ , 5, 3])

cor(bioman_2.5_aligned$Xest[ , 1, 1], bioman_2.5_aligned$Xest[ , 1, 2])
cor(bioman_2.5_aligned$Xest[ , 2, 1], bioman_2.5_aligned$Xest[ , 2, 2])
cor(bioman_2.5_aligned$Xest[ , 3, 1], bioman_2.5_aligned$Xest[ , 3, 2])
cor(bioman_2.5_aligned$Xest[ , 4, 1], bioman_2.5_aligned$Xest[ , 4, 2])
cor(bioman_2.5_aligned$Xest[ , 5, 1], bioman_2.5_aligned$Xest[ , 5, 2])

cor(bioman_2.5_aligned$Xest[ , 1, 1], bioman_2.5_aligned$Xest[ , 1, 4])
cor(bioman_2.5_aligned$Xest[ , 2, 1], bioman_2.5_aligned$Xest[ , 2, 4])
cor(bioman_2.5_aligned$Xest[ , 3, 1], bioman_2.5_aligned$Xest[ , 3, 4])
cor(bioman_2.5_aligned$Xest[ , 4, 1], bioman_2.5_aligned$Xest[ , 4, 4])
cor(bioman_2.5_aligned$Xest[ , 5, 1], bioman_2.5_aligned$Xest[ , 5, 4])

cor(bioman_2.5_aligned$Xest[ , 1, 2], bioman_2.5_aligned$Xest[ , 1, 3])
cor(bioman_2.5_aligned$Xest[ , 2, 2], bioman_2.5_aligned$Xest[ , 2, 3])
cor(bioman_2.5_aligned$Xest[ , 3, 2], bioman_2.5_aligned$Xest[ , 3, 3])
cor(bioman_2.5_aligned$Xest[ , 4, 2], bioman_2.5_aligned$Xest[ , 4, 3])
cor(bioman_2.5_aligned$Xest[ , 5, 2], bioman_2.5_aligned$Xest[ , 5, 3])

cor(bioman_2.5_aligned$Xest[ , 1, 2], bioman_2.5_aligned$Xest[ , 1, 4])
cor(bioman_2.5_aligned$Xest[ , 2, 2], bioman_2.5_aligned$Xest[ , 2, 4])
cor(bioman_2.5_aligned$Xest[ , 3, 2], bioman_2.5_aligned$Xest[ , 3, 4])
cor(bioman_2.5_aligned$Xest[ , 4, 2], bioman_2.5_aligned$Xest[ , 4, 4])
cor(bioman_2.5_aligned$Xest[ , 5, 2], bioman_2.5_aligned$Xest[ , 5, 4])

samp <- sample.int(nrow(bioman_2.5_aligned$Xest[ , , 1]), 100000)
plot(bioman_2.5_aligned$Xest[samp, 4, 2], bioman_2.5_aligned$Xest[samp, 4, 4])
plot(bioman_2.5_aligned$Xest[samp, 5, 1], bioman_2.5_aligned$Xest[samp, 5, 2])


bioclim_scaled_rast_5m <- rast("data/bioclim_manifold_all_data_5.tif")[[1:5]]
bioclim_scaled_rast_2.5m <- rast("data/bioclim_manifold_all_data.tif")[[1:5]]

bioclim_downscaled <- terra::resample(bioclim_scaled_rast_2.5m, bioclim_scaled_rast_5m,
                                      threads = 5)

bioclim_5m <- as.data.frame(bioclim_scaled_rast_5m, xy = TRUE, na.rm = TRUE)
bioclim_2.5m <- as.data.frame(bioclim_downscaled, xy = TRUE, na.rm = TRUE)

colnames(bioclim_5m)[3:7] <- paste0(colnames(bioclim_5m)[3:7], "_5m")
colnames(bioclim_2.5m)[3:7] <- paste0(colnames(bioclim_2.5m)[3:7], "_2.5m")

bioclim_combined <- bioclim_5m |>
  left_join(bioclim_2.5m, by = c("x", "y"))


############## Do 4 dims ################

runs_4 <- runs_info() |>
  filter(status == "completed",
         tags == "ident_lat_lon_weighted",
         scalars$active_dims == 4)
dirs <- runs_4$dir
files <- file.path(dirs, "output/vae_manifold_vars.csv")

bioman_2.5_run_dat <- map(files, read_csv)

bioman_2.5_all_4 <- bioman_2.5_run_dat[[1]] |>
  rename_with(~paste0(., "_run1"), c(-x, -y))

for(i in 2:length(bioman_2.5_run_dat)) {
  bioman_2.5_all_4 <- bioman_2.5_all_4 |>
    left_join(bioman_2.5_run_dat[[i]] |>
                rename_with(~paste0(., "_run", i),
                            c(-x, -y)),
              by = c("x", "y"))
}

runs_i <- 1:length(files)
bioman_mats_4 <- list()

for(i in runs_i) {
  bioman_mats_4[[i]] <- bioman_2.5_all_4 |>
    select(ends_with(paste0("_run", i))) |>
    as.matrix()
}

combs <- expand.grid(bioman1 = 1:4, bioman2 = 1:4)
cors <- apply(combs, 1, function(x) cor(bioman_mats_4[[1]][ , x[1]], bioman_mats_4[[2]][ , x[2]]))
combs <- combs |>
  mutate(corr = cors)

cors <- apply(combs, 1, function(x) cor(bioman_mats[[1]][ , x[1]], bioman_mats[[4]][ , x[2]]))
combs <- combs |>
  mutate(corr = cors)

#### 6 dims ###########

runs_6 <- runs_info() |>
  filter(status == "completed",
         tags == "ident_lat_lon_weighted",
         scalars$active_dims == 6)
dirs <- runs_6$dir
files <- file.path(dirs, "output/vae_manifold_vars.csv")

bioman_2.5_run_dat <- map(files, read_csv)

bioman_2.5_all_6 <- bioman_2.5_run_dat[[1]] |>
  rename_with(~paste0(., "_run1"), c(-x, -y))

for(i in 2:length(bioman_2.5_run_dat)) {
  bioman_2.5_all_6 <- bioman_2.5_all_6 |>
    left_join(bioman_2.5_run_dat[[i]] |>
                rename_with(~paste0(., "_run", i),
                            c(-x, -y)),
              by = c("x", "y"))
}

runs_i <- 1:length(files)
bioman_mats_6 <- list()

for(i in runs_i) {
  bioman_mats_6[[i]] <- bioman_2.5_all_6 |>
    select(ends_with(paste0("_run", i))) |>
    as.matrix()
}

combs <- expand.grid(bioman1 = 1:6, bioman2 = 1:6)
cors <- apply(combs, 1, function(x) cor(bioman_mats_6[[1]][ , x[1]], bioman_mats_6[[2]][ , x[2]]))
combs <- combs |>
  mutate(corr = cors)

cors <- apply(combs, 1, function(x) cor(bioman_mats[[1]][ , x[1]], bioman_mats[[4]][ , x[2]]))
combs <- combs |>
  mutate(corr = cors)


########### res = 10 ##########
runs <- runs_info() |>
  filter(status == "completed",
         tags == "ident_lat_lon_weighted_10")

runs_5 <- runs_info() |>
  filter(status == "completed",
         tags == "ident_lat_lon_weighted_10",
         scalars$active_dims == 5)
dirs <- runs_5$dir
files <- file.path(dirs, "output/vae_manifold_vars.csv")

bioman_2.5_run_dat <- map(files, read_csv)

bioman_2.5_all_5 <- bioman_2.5_run_dat[[1]] |>
  rename_with(~paste0(., "_run1"), c(-x, -y))

for(i in 2:length(bioman_2.5_run_dat)) {
  bioman_2.5_all_5 <- bioman_2.5_all_5 |>
    left_join(bioman_2.5_run_dat[[i]] |>
                rename_with(~paste0(., "_run", i),
                            c(-x, -y)),
              by = c("x", "y"))
}

runs_i <- 1:length(files)
bioman_mats_5 <- list()

for(i in runs_i) {
  bioman_mats_5[[i]] <- bioman_2.5_all_5 |>
    select(ends_with(paste0("_run", i))) |>
    as.matrix()
}

combs <- expand.grid(bioman1 = 1:5, bioman2 = 1:5)
cors <- apply(combs, 1, function(x) cor(bioman_mats_5[[1]][ , x[1]], bioman_mats_5[[2]][ , x[2]]))
combs <- combs |>
  mutate(corr = cors)


########### res = 5 ##########
runs <- runs_info() |>
  filter(status == "completed",
         tags == "ident_lat_lon_weighted_5")

runs_5 <- runs_info() |>
  filter(status == "completed",
         tags == "ident_lat_lon_weighted_5",
         scalars$active_dims == 5)
dirs <- runs_5$dir
files <- file.path(dirs, "output/vae_manifold_vars.csv")

bioman_2.5_run_dat <- map(files, read_csv)

bioman_2.5_all_5 <- bioman_2.5_run_dat[[1]] |>
  rename_with(~paste0(., "_run1"), c(-x, -y))

for(i in 2:length(bioman_2.5_run_dat)) {
  bioman_2.5_all_5 <- bioman_2.5_all_5 |>
    left_join(bioman_2.5_run_dat[[i]] |>
                rename_with(~paste0(., "_run", i),
                            c(-x, -y)),
              by = c("x", "y"))
}

runs_i <- 1:length(files)
bioman_mats_5 <- list()

for(i in runs_i) {
  bioman_mats_5[[i]] <- bioman_2.5_all_5 |>
    select(ends_with(paste0("_run", i))) |>
    as.matrix()
}

combs <- expand.grid(bioman1 = 1:5, bioman2 = 1:5)
cors <- apply(combs, 1, function(x) cor(bioman_mats_5[[1]][ , x[1]], bioman_mats_5[[3]][ , x[2]]))
combs <- combs |>
  mutate(corr = cors)
