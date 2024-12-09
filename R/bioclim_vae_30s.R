library(torch)
library(terra)
library(tidyverse)
library(dagnn)
library(zeallot)
library(SpaDES.tools)

bioclim_files <- list.files("data/bioclim_30s", full.names = TRUE)
bioclim <- map(bioclim_files, terra::rast)
bioclim <- do.call(c, bioclim)

dir <- "data/bioclim_30s_tiles/"

#split_rast <- splitRaster(bioclim, 400, 200, path = dir)

# m <- global(bioclim, "mean", na.rm = TRUE)
# s <- global(bioclim, "sd", na.rm = TRUE)
# scf <- cbind(m, s)
#
# scoff(bioclim) <- scf
#
# #bioclim_scaled <- scale(bioclim)

## scaling parameters
m <- global(bioclim, "mean", na.rm = TRUE)
s <- global(bioclim, "sd", na.rm = TRUE)
scaling <- list(means = m |> pull(mean),
                sds = s |> pull(sd))

write_rds(scaling, "data/bioclim_scaling_30s.rds")

scaling <- read_rds("data/bioclim_scaling_30s.rds")

tile_files <- list.files(dir, full.names = TRUE)
tile_names <- strsplit(basename(tile_files), "_") |>
  map_chr(~ .x[5]) %>%
  gsub(pattern = ".tif", replacement = "", .)

tile_df <- tibble(tile = tile_names,
                  file = tile_files) |>
  group_by(tile) |>
  summarise(file = list(file),
            .groups = "drop")

test <- rast(tile_df$file[[5500]])
df <- test |>
  as.data.frame(na.rm = TRUE, xy = TRUE)

bioclim_dataset <- dataset(name = "bioclim_ds",
                           initialize = function(tiles, scaling) {
                             self$tiles <- tiles
                             self$scaling <- scaling
                           },
                           .getbatch = function(i) {
                             loaded <- map(self$tiles[i],
                                           ~ terra::rast(.x) |>
                                             terra::as.data.frame(na.rm = TRUE)) |>
                               list_rbind() |>
                               as.matrix()
                             loaded <- t((t(loaded) - self$scaling$means) / self$scaling$sds)
                             torch_tensor(loaded)
                           },
                           .length = function() {
                             length(self$tiles)
                           })

train_ds <- bioclim_dataset(tile_df$file, scaling)
train_dl <- dataloader(train_ds, 100, shuffle = TRUE)

test_iter <- train_dl$.iter()
dim(test_iter$.next())
dim(test_iter$.next())
dim(test_iter$.next())

## note that this module includes the ability to provide data to condition on, making
## it a 'conditional VAE', however, for this study I do not use any conditioning information
## In this case we just provide a tensor of zeroes for the conditioning data, effectively turning
## it into an unconditional VAE. The conditioning ability is for future extensions.
vae_mod <- nn_module("CVAE",
                 initialize = function(input_dim, c_dim, latent_dim, loggamma_init = 0) {
                   self$latent_dim <- latent_dim
                   self$input_dim <- input_dim
                   self$encoder <- nndag(i_1 = ~ input_dim,
                                         c = ~ c_dim,
                                         c_1 = c ~ c_dim,
                                         e_1 = i_1 + c_1 ~ latent_dim * 2,
                                         e_2 = e_1 + c_1 ~ latent_dim * 2,
                                         e_3 = e_2 + c_1 ~ latent_dim * 2,
                                         means = e_3 ~ latent_dim,
                                         logvars = e_3 ~ latent_dim,
                                         .act = list(nn_relu,
                                                     logvars = nn_identity,
                                                     means = nn_identity))

                   self$decoder <- nndag(i_1 = ~ latent_dim,
                                         c = ~ c_dim,
                                         c_1 = c ~ c_dim,
                                         d_1 = i_1 + c_1 ~ latent_dim * 2,
                                         d_2 = d_1 + c_1 ~ latent_dim * 2,
                                         d_3 = d_2 + c_1 ~ latent_dim * 2,
                                         out = d_3 ~ input_dim,
                                         .act = list(nn_relu,
                                                     out = nn_identity))

                   self$loggamma <- nn_parameter(torch_tensor(loggamma_init))

                 },
                 reparameterize = function(mean, logvar) {
                   std <- torch_exp(torch_tensor(0.5, device = "cuda") * logvar)
                   eps <- torch_randn_like(std)
                   eps * std + mean
                 },
                 loss_function = function(reconstruction, input, mean, log_var) {
                   kl <- torch_sum(torch_exp(log_var) + torch_square(mean) - log_var, dim = 2L) - latent_dim
                   recon1 <- torch_sum(torch_square(input - reconstruction), dim = 2L) / torch_exp(self$loggamma)
                   recon2 <- self$input_dim * self$loggamma + torch_log(torch_tensor(2 * pi, device = "cuda")) * self$input_dim
                   loss <- torch_mean(recon1 + recon2 + kl)
                   list(loss, torch_mean(recon1*torch_exp(self$loggamma)), torch_mean(kl))
                 },
                 forward = function(x, c) {
                   c(means, log_vars) %<-% self$encoder(c, x)
                   z <- self$reparameterize(means, log_vars)
                   list(self$decoder(c, z), x, means, log_vars)
                 })

input_dim <- 19
c_dim <- 1
latent_dim <- 16

vae <- vae_mod(input_dim, c_dim, latent_dim, loggamma_init = -3)
vae <- vae$cuda()

num_epochs <- 1000

lr <- 0.005
optimizer <- optim_adam(vae$parameters, lr = lr)
scheduler <- lr_one_cycle(optimizer, max_lr = lr,
                          epochs = num_epochs, steps_per_epoch = length(train_dl),
                          cycle_momentum = FALSE)


for (epoch in 1:num_epochs) {

    batchnum <- 0
    coro::loop(for (b in train_dl) {

        batchnum <- batchnum + 1
        input <- b$to(device = "cuda")
        optimizer$zero_grad()
        c_null <- torch_zeros(input$size()[[1]], 1, device = "cuda") ## null conditioning data
        c(reconstruction, input, mean, log_var) %<-% vae(input, c_null)
        c(loss, reconstruction_loss, kl_loss) %<-% vae$loss_function(reconstruction, input, mean, log_var)


        if(batchnum %% 10 == 0) {

            cat("Epoch: ", epoch,
                "    batch: ", batchnum,
                "    loss: ", as.numeric(loss$cpu()),
                "    recon loss: ", as.numeric(reconstruction_loss$cpu()),
                "    KL loss: ", as.numeric(kl_loss$cpu()),
                "    loggamma: ", as.numeric(vae$loggamma$cpu()),
                #"    loggamma: ", loggamma,
                "    active dims: ", as.numeric((torch_exp(log_var)$mean(dim = 1L) < 0.5)$sum()$cpu()),
                "\n")

        }
        loss$backward()
        optimizer$step()
        scheduler$step()
    })
}

############### figures for VAE #####################

vae <- torch_load("data/trained_vae_1.to")
vae <- vae$cuda()

post_dl <- dataloader(train_ds, 500000, shuffle = FALSE)
post_it <- as_iterator(post_dl)

all_mean_var <- list()
it <- 0
loop(for(i in post_dl) {
  it <- it + 1
  with_no_grad({
    c_null <- torch_zeros(i$size()[[1]], 1, device = "cuda")
    all_mean_var[[it]] <- vae$encoder(c_null, i$cuda())
  })
  print(it)
})

all_means <- map(all_mean_var, 1) %>%
  map(~ as.matrix(.x$cpu())) %>%
  do.call(rbind, .)

all_vars <- map(all_mean_var, 2) %>%
  map(~ as.matrix(torch_exp(.x)$cpu())) %>%
  do.call(rbind, .)

vars <- apply(all_vars, 2, mean)

vars_df <- tibble(`Mean Variance` = vars) %>%
  mutate(Status = ifelse(`Mean Variance` < 0.5, "Keep", "Toss"))

ggplot(vars_df, aes(`Mean Variance`)) +
  geom_histogram(aes(fill = Status)) +
  ylab("Count") +
  theme_minimal() +
  theme(legend.position = "none")

manifold_dims <- vars < 0.5

non_manifold_dim_nums <- which(!manifold_dims)
write_rds(non_manifold_dim_nums, "data/non_manifold_dim_nums.rds")

## run decoder using only manifold dimensions and calculate reconstruction
recon_loss <- list()
it <- 0
loop(for(i in post_dl) {
  it <- it + 1
  with_no_grad({
    c_null <- torch_zeros(i$size()[[1]], 1, device = "cuda")
    all_mean_var <- vae$encoder(c_null, i$cuda())
    z <- all_mean_var$means$clone()
    z[ , non_manifold_dim_nums] <- 0
    recon <- vae$decoder(c_null, z)
    recon_loss[[it]] <- torch_mean((i$cuda() - recon)^2)

  })
  print(it)
})
mse <- mean(unlist(map(recon_loss, ~ as.numeric(.x$cpu()))))

## MSE = 0.0036

mean_variances <- apply(all_means, 2, var)

bioclim_df <- as.data.frame(bioclim, na.rm = TRUE, xy = TRUE) %>%
  select(x, y) %>%
  bind_cols(as.data.frame(all_means[ , manifold_dims]))

colnames(bioclim_df) <- c("x", "y", paste("manifold", 1:5, sep = "_"))

write_csv(bioclim_df, "data/vae_manifold_vars.csv")

ggplot(bioclim_df, aes(x, y)) +
  geom_raster(aes(fill = manifold_1)) +
  scale_fill_viridis_c("inferno") +
  coord_equal() +
  theme_minimal()

vae_rast <- rast(bioclim_df, type = "xyz", crs = crs(bioclim), extent = ext(c(min(bioclim_df$x),
                                                                              max(bioclim_df$x),
                                                                              min(bioclim_df$y),
                                                                              max(bioclim_df$y))))

vae_rast2 <- project(vae_rast, bioclim)
vae_rast <- vae_rast2

names(vae_rast) <- c("BIOMAN1", "BIOMAN2", "BIOMAN3","BIOMAN4", "BIOMAN5")
writeRaster(vae_rast, c("data/bioclim_manifold_1.tif",
                        "data/bioclim_manifold_2.tif",
                        "data/bioclim_manifold_3.tif",
                        "data/bioclim_manifold_4.tif",
                        "data/bioclim_manifold_5.tif"),
            overwrite = TRUE)


############# PCA analysis ####################
pca <- prcomp(bioclim_mat[complete.cases(bioclim_mat),])

pca_df <- as.data.frame(bioclim, na.rm = TRUE, xy = TRUE) %>%
  select(x, y) %>%
  bind_cols(as.data.frame(pca$x))

pca_rast <- rast(pca_df, type = "xyz", crs = crs(bioclim), extent = ext(c(min(pca_df$x),
                                                                          max(pca_df$x),
                                                                          min(pca_df$y),
                                                                          max(pca_df$y))))

pca_rast <- project(pca_rast, bioclim)

eigenvec <- pca$sdev
vec_cumsum <- cumsum(eigenvec) / sum(eigenvec)
var_exp <- tibble(`Cumulative Variance Explained` = vec_cumsum,
                  Variable = factor(paste("PCA", 1:length(vec_cumsum)))) %>%
  mutate(Variable = reorder(Variable, `Cumulative Variance Explained`),
         Status = ifelse(`Cumulative Variance Explained` < 0.96, "Keep", "Toss"))

ggplot(var_exp, aes(Variable, `Cumulative Variance Explained`)) +
  geom_bar(aes(fill = Status), stat = "identity") +
  annotate("line", y = c(0.95, 0.95), x = c(1, 19.5)) +
  annotate("text", x = 21, y = 0.95, label = "95%") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 0))

torch_save(vae, "data/trained_vae_1.to")

############# ecoregions ##############
library(sf)
library(fasterize)
ecoreg <- st_read("data/ecoregions")
ecoreg_rast <- fasterize(ecoreg, raster::raster(vae_rast), "BIOME_NUM")
ecoreg_rast2 <- rast(ecoreg_rast)

bioclim_scaled_df <- as.data.frame(bioclim, na.rm = TRUE, xy = TRUE) %>%
  select(x, y) %>%
  bind_cols(as.data.frame(bioclim_mat))

bioclim_scaled_rast <- rast(bioclim_scaled_df, type = "xyz", crs = crs(bioclim),
                            extent = ext(c(min(bioclim_scaled_df$x),
                                           max(bioclim_scaled_df$x),
                                           min(bioclim_scaled_df$y),
                                           max(bioclim_scaled_df$y))))

bioclim_scaled_rast <- project(bioclim_scaled_rast, bioclim)

mani_ecoreg <- c(vae_rast, ecoreg_rast2, pca_rast, bioclim)

writeRaster(mani_ecoreg, "data/bioclim_manifold_all_data.tif")
writeRaster(bioclim_scaled_rast, "data/bioclim_scaled.tif")

ecoreg_names <- ecoreg %>%
  as_tibble() %>%
  group_by(BIOME_NUM) %>%
  summarise(BIOME_NAME = BIOME_NAME[1])

write_csv(ecoreg_names, "data/ecoreg_names.csv")

########### visualize manifold dimensions ##################

bioclim_cats <-
  tribble(~Variable, ~Description,                                                 ~Type,
          "BIO1",    "Annual Mean Temperature",                                    "Temperature Value",
          "BIO2",    "Mean Diurnal Range (Mean of monthly (max temp - min temp))", "Temperature Variation",
          "BIO3",    "Isothermality (BIO2/BIO7) (×100)",                           "Temperature Variation",
          "BIO4",    "Temperature Seasonality (standard deviation ×100)",          "Temperature Variation",
          "BIO5",    "Max Temperature of Warmest Month",                           "Temperature Value",
          "BIO6",    "Min Temperature of Coldest Month",                           "Temperature Value",
          "BIO7",    "Temperature Annual Range (BIO5-BIO6)",                       "Temperature Variation",
          "BIO8",    "Mean Temperature of Wettest Quarter",                        "Temperature Precipitation Interaction",
          "BIO9",    "Mean Temperature of Driest Quarter",                         "Temperature Precipitation Interaction",
          "BIO10",   "Mean Temperature of Warmest Quarter",                        "Temperature Value",
          "BIO11",   "Mean Temperature of Coldest Quarter",                        "Temperature Value",
          "BIO12",   "Annual Precipitation",                                       "Precipitation Value",
          "BIO13",   "Precipitation of Wettest Month",                             "Precipitation Value",
          "BIO14",   "Precipitation of Driest Month",                              "Precipitation Value",
          "BIO15",   "Precipitation Seasonality (Coefficient of Variation)",       "Precipitation Variation",
          "BIO16",   "Precipitation of Wettest Quarter",                           "Precipitation Value",
          "BIO17",   "Precipitation of Driest Quarter",                            "Precipitation Value",
          "BIO18",   "Precipitation of Warmest Quarter",                           "Temperature Precipitation Interaction",
          "BIO19",   "Precipitation of Coldest Quarter",                           "Temperature Precipitation Interaction"
)


