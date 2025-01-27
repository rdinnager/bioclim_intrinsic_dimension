#| requires:
#|     - file: data
#|       target-type: link
#|     - file: output

library(torch)
library(terra)
library(tidyverse)
library(dagnn)
library(zeallot)

options(torch.serialization_version = 2)

source("R/angle_embedding.R")

checkpoint <- "none"
res <- "2.5"

bioclim_files <- list.files(paste0("data/bioclim_", res), full.names = TRUE)
bioclim <- map(bioclim_files, terra::rast)
bioclim <- do.call(c, bioclim)

#ecoregions <- rast("data/bioclim_manifold_all_data.tif")[["layer"]]

#bioclim <- c(bioclim, ecoregions)

bioclim_mat <- as.data.frame(bioclim, na.rm = TRUE, xy = TRUE) %>%
  as.matrix()

world_embedding <- cbind(create_angle_embedding(bioclim_mat[ , 2]),
                         create_angle_embedding(bioclim_mat[ , 1]))

bioclim_mat <- bioclim_mat[ , -c(1, 2)]

bioclim_mat <- bioclim_mat |>
  scale()

scaling <- list(means = attr(bioclim_mat, "scaled:center"),
                sds = attr(bioclim_mat, "scaled:scale"))

write_rds(scaling, "output/bioclim_scaling_ident.rds")

bioclim_dataset <- dataset(name = "bioclim_ds",
                           initialize = function(rr, lat) {
                             self$bioclim <- torch_tensor(rr)
                             self$lat <- torch_tensor(lat)
                           },
                           .getbatch = function(i) {
                             list(self$bioclim[i, ], self$lat[i, , drop = FALSE])
                           },
                           .length = function() {
                             self$bioclim$size()[[1]]
                           })

train_ds <- bioclim_dataset(bioclim_mat, world_embedding)
train_dl <- dataloader(train_ds, 1300000, shuffle = TRUE)
batch_num <- length(train_dl)

## clear space
rm(bioclim_mat, world_embedding)
gc()

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
                                         #c_1 = c ~ c_dim,
                                         e_1 = i_1 + c ~ latent_dim * 2,
                                         e_2 = e_1 + c ~ latent_dim * 2,
                                         e_3 = e_2 + c ~ latent_dim * 2,
                                         means = e_3 ~ latent_dim,
                                         logvars = e_3 ~ latent_dim,
                                         .act = list(nn_relu,
                                                     logvars = nn_identity,
                                                     means = nn_identity))

                   self$decoder <- nndag(i_1 = ~ latent_dim,
                                         #c = ~ c_dim,
                                         #c_1 = c ~ c_dim,
                                         d_1 = i_1 ~ latent_dim * 2,
                                         d_2 = d_1 ~ latent_dim * 2,
                                         d_3 = d_2 ~ latent_dim * 2,
                                         out = d_3 ~ input_dim,
                                         .act = list(nn_relu,
                                                     out = nn_identity))

                   self$prior_net <- nndag(c = ~ c_dim,  # latitude input
                                           p_1 = c ~ latent_dim * 2,
                                           p_2 = p_1 ~ latent_dim * 2,
                                           p_3 = p_2 ~ latent_dim * 2,
                                           prior_means = p_3 ~ latent_dim,
                                           prior_logvars = p_3 ~ latent_dim,
                                           .act = list(nn_relu,
                                                       prior_means = nn_identity,
                                                       prior_logvars = nn_identity))

                   self$loggamma <- nn_parameter(torch_tensor(loggamma_init))

                 },
                 reparameterize = function(mean, logvar) {
                   std <- torch_exp(torch_tensor(0.5, device = "cuda") * logvar)
                   eps <- torch_randn_like(std)
                   eps * std + mean
                 },
                 loss_function = function(reconstruction, input, mean, log_var, prior_means, prior_logvars, hloss_prior = 0.1) {

                   # KL between posterior and conditional prior
                   kl_posterior <- torch_sum(torch_exp(log_var - prior_logvars) +
                                               torch_square(mean - prior_means)/torch_exp(prior_logvars) -
                                               1 + prior_logvars - log_var,
                                             dim = 2L)

                   # KL between conditional prior and standard normal
                   kl_prior <- torch_sum(torch_exp(prior_logvars) +
                                           torch_square(prior_means) -
                                           prior_logvars,
                                         dim = 2L)

                   recon1 <- torch_sum(torch_square(input - reconstruction), dim = 2L) / torch_exp(self$loggamma)
                   recon2 <- self$input_dim * self$loggamma + torch_log(torch_tensor(2 * pi, device = "cuda")) * self$input_dim

                   loss <- torch_mean(recon1 + recon2 + kl_posterior + hloss_prior * kl_prior)
                   list(loss, torch_mean(recon1*torch_exp(self$loggamma)), torch_mean(kl_posterior), torch_mean(kl_prior))
                 },
                 forward = function(x, c) {
                   c(means, log_vars) %<-% self$encoder(x, c)
                   z <- self$reparameterize(means, log_vars)
                   c(prior_means, prior_logvars) %<-% self$prior_net(c)
                   list(self$decoder(z), x, means, log_vars, prior_means, prior_logvars)
                 })

input_dim <- 19
c_dim <- 64
latent_dim <- 16

loggamma_init <- -3
if(checkpoint != "none") {
  vae <- torch_load(checkpoint)
} else {
  vae <- vae_mod(input_dim, c_dim, latent_dim, loggamma_init = loggamma_init)
}
vae <- vae$cuda()

num_epochs <- 2000

lr <- 0.005
optimizer <- optim_adam(vae$parameters, lr = lr)
scheduler <- lr_one_cycle(optimizer, max_lr = lr,
                          epochs = num_epochs, steps_per_epoch = length(train_dl),
                          cycle_momentum = FALSE)


for (epoch in 1:num_epochs) {

    batchnum <- 0
    coro::loop(for (b in train_dl) {

        batchnum <- batchnum + 1
        optimizer$zero_grad()
        input <- b[[1]]$cuda()
        #c_null <- torch_zeros(input$size()[[1]], 1, device = "cuda") ## null conditioning data
        c(reconstruction, input, mean, log_var, prior_mean, prior_logvar) %<-% vae(input, b[[2]]$cuda())
        c(loss, reconstruction_loss, kl_loss, kl_hloss) %<-% vae$loss_function(reconstruction, input, mean, log_var, prior_mean, prior_logvar)

        if(batchnum == batch_num) {

            cat("step: ", epoch, "\n",
                "batch: ", batchnum, "\n",
                "loss: ", as.numeric(loss$cpu()), "\n",
                "recon_loss: ", as.numeric(reconstruction_loss$cpu()), "\n",
                "KL_loss: ", as.numeric(kl_loss$cpu()), "\n",
                "KL_Hloss: ", as.numeric(kl_hloss$cpu()), "\n",
                "loggamma: ", as.numeric(vae$loggamma$cpu()), "\n",
                #"    loggamma: ", loggamma,
                "active_dims: ", as.numeric((torch_exp(log_var)$mean(dim = 1L) < 0.5)$sum()$cpu()),
                "\n",
                sep = "")

        }
        loss$backward()
        optimizer$step()
        scheduler$step()
    })
}

############### figures for VAE #####################

options(torch.serialization_version = 2)
torch_save(vae, "output/trained_ivae.to")

post_dl <- dataloader(train_ds, 500000, shuffle = FALSE)
post_it <- as_iterator(post_dl)

all_mean_var <- list()
it <- 0
loop(for(i in post_dl) {
  it <- it + 1
  with_no_grad({
    c_null <- i[[2]]$cuda()
    all_mean_var[[it]] <- vae$encoder(i[[1]]$cuda(), c_null)
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

p <- ggplot(vars_df, aes(`Mean Variance`)) +
  geom_histogram(aes(fill = Status)) +
  ylab("Count") +
  theme_minimal() +
  theme(legend.position = "none")
plot(p)

manifold_dims <- vars < 0.5

non_manifold_dim_nums <- which(!manifold_dims)
write_rds(non_manifold_dim_nums, "output/non_manifold_dim_nums.rds")

## run decoder using only manifold dimensions and calculate reconstruction
recon_loss <- list()
it <- 0
loop(for(i in post_dl) {
  it <- it + 1
  with_no_grad({
    c_null <- i[[2]]$cuda()
    input <- i[[1]]$cuda()
    all_mean_var <- vae$encoder(input, c_null)
    z <- all_mean_var$means$clone()
    z[ , non_manifold_dim_nums] <- 0
    recon <- vae$decoder(z)
    recon_loss[[it]] <- torch_mean((input - recon)^2)

  })
  print(it)
})
mse <- mean(unlist(map(recon_loss, ~ as.numeric(.x$cpu()))))

cat("MSE:", mse, "/n")

## MSE = 0.0036

mean_variances <- apply(all_means, 2, var)

bioclim_df <- as.data.frame(bioclim, na.rm = TRUE, xy = TRUE) %>%
  select(x, y) %>%
  bind_cols(as.data.frame(all_means[ , manifold_dims]))

n_dims <- sum(manifold_dims)
colnames(bioclim_df) <- c("x", "y", paste("manifold", 1:n_dims, sep = "_"))

write_csv(bioclim_df, "output/vae_manifold_vars.csv")

p <- ggplot(bioclim_df, aes(x, y)) +
  geom_raster(aes(fill = manifold_1)) +
  scale_fill_viridis_c("inferno") +
  coord_equal() +
  theme_minimal()

plot(p)

vae_rast <- rast(bioclim_df, type = "xyz", crs = crs(bioclim), extent = ext(c(min(bioclim_df$x),
                                                                              max(bioclim_df$x),
                                                                              min(bioclim_df$y),
                                                                              max(bioclim_df$y))))

vae_rast2 <- project(vae_rast, bioclim)
vae_rast <- vae_rast2

names(vae_rast) <- paste0("BIOMAN", 1:n_dims)
filenames <- paste0("output/bioclim_manifold_", 1:n_dims, ".tif")
writeRaster(vae_rast, filenames,
            overwrite = TRUE)

plot(vae_rast)

cat("n_dims:", n_dims)



