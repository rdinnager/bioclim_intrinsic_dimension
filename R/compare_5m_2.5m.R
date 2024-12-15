library(tidyverse)
library(terra)
library(guildai)
library(alignProMises)

runs <- runs_info() |>
  filter(status == "completed",
         tags == "ident_ecoregion")
dirs <- runs$dir
files <- file.path(dirs, "output/vae_manifold_vars.csv")

bioman_2.5_run_dat <- map(files, read_csv)

bioman_2.5_all <- bioman_2.5_run_dat[[1]] |>
  rename_with(~paste0(., "_run1"), c(-x, -y))

for(i in 2:length(bioman_2.5_run_dat)) {
  bioman_2.5_all <- bioman_2.5_all |>
    left_join(bioman_2.5_run_dat[[i]] |>
                rename_with(~paste0(., "_run", i),
                            c(-x, -y)),
              by = c("x", "y"))
}

runs <- 1:length(files)
bioman_mats <- list()

for(i in runs) {
  bioman_mats[[i]] <- bioman_2.5_all |>
    select(ends_with(paste0("_run", i))) |>
    as.matrix()
}

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

