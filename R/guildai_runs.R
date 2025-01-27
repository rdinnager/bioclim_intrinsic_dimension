library(guildai)
library(tidyverse)
#library(reticulate)

#py_config <- py_config()
#install_guild(python = py_exe())

n_runs <- 8

old_wd <- setwd("R")

assignInNamespace("find_guild", guildai:::find_r_guildai_guild, ns = "guildai")

Sys.setenv(GUILD_HOME = "/blue/rdinnage.fiu/rdinnage.fiu/.guild") #"/home/rdinnage.fiu/.guild") #"/blue/rdinnage.fiu/rdinnage.fiu/.guild")

runs_2.5 <- runs_info(tag = "ident_lat_lon_weighted", filter = 'active_dims = 5')
best_run <- runs_2.5 |>
  slice_min(scalars$loss)
best_run_chkpnt <- file.path(best_run$dir, "output", "trained_ivae.to")

for(i in 1:n_runs) {
  guild_run("bioclim_ivae_lat_long.R", tag = "ident_lat_lon_weighted_long_pre", as_job = FALSE,
           flags = list(loggamma_init = -5.0,
                        num_epochs = 10000,
                        lr = 0.0005,
                        res = "5",
                        checkpoint = best_run_chkpnt))
  guild_run("bioclim_ivae_lat_long.R", tag = "ident_lat_lon_weighted_long_pre", as_job = FALSE,
            flags = list(loggamma_init = -4.0,
                         num_epochs = 10000,
                         lr = 0.0005,
                         res = "5",
                         checkpoint = best_run_chkpnt))
}

for(i in 1:n_runs) {
  guild_run("bioclim_ivae_2.5_ident_lat_long.R", tag = "ident_lat_lon_weighted", as_job = FALSE,
            flags = "loggamma_init=-4.0")
  guild_run("bioclim_ivae_2.5_ident_lat_long.R", tag = "ident_lat_lon_weighted", as_job = FALSE,
            flags = "loggamma_init=-5.0")
}


for(i in 1:n_runs) {
  guild_run("bioclim_vae_2.5_guildai.R", tag = "regular")
}

for(i in 1:n_runs) {
  guild_run("bioclim_vae_2.5_ident.R", tag = "ident_ecoregion", as_job = FALSE)
}

for(i in 1:n_runs) {
  guild_run("bioclim_ivae_2.5_ident_lat.R", tag = "ident_lat", as_job = FALSE)
}

for(i in 1:n_runs) {
  guild_run("bioclim_ivae_lat_long.R", tag = "ident_lat_lon_weighted_10", as_job = FALSE,
            flags = list(loggamma_init=-4.0, res="10", num_epochs = 6000))
  guild_run("bioclim_ivae_lat_long.R", tag = "ident_lat_lon_weighted_10", as_job = FALSE,
            flags = list(loggamma_init=-5.0, res="10", num_epochs = 6000))
}

for(i in 1:n_runs) {
  guild_run("bioclim_ivae_lat_long.R", tag = "ident_lat_lon_weighted_5", as_job = FALSE,
            flags = list(loggamma_init=-4.0, res="5", num_epochs = 4000))
  guild_run("bioclim_ivae_lat_long.R", tag = "ident_lat_lon_weighted_5", as_job = FALSE,
            flags = list(loggamma_init=-5.0, res="5", num_epochs = 4000))
}

for(i in 1:n_runs) {
  guild_run("bioclim_ivae_2.5_ident_lat.R", tag = "ident_lat_angle", as_job = FALSE,
            flags = "loggamma_init=-3.0")
  guild_run("bioclim_ivae_2.5_ident_lat.R", tag = "ident_lat_angle", as_job = FALSE,
            flags = "loggamma_init=-4.0")
}

for(i in 1:n_runs) {
  guild_run("bioclim_ivae_2.5_ident_lat.R", tag = "ident_lat", as_job = FALSE,
            flags = "loggamma_init=-4.0")
}

setwd(old_wd)
