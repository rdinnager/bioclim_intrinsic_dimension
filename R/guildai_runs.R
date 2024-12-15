library(guildai)
#library(reticulate)

#py_config <- py_config()
#install_guild(python = py_exe())

n_runs <- 6

old_wd <- setwd("R")

assignInNamespace("find_guild", guildai:::find_r_guildai_guild, ns = "guildai")

Sys.setenv(GUILD_HOME = "/blue/rdinnage.fiu/rdinnage.fiu/.guild") #"/home/rdinnage.fiu/.guild") #"/blue/rdinnage.fiu/rdinnage.fiu/.guild")

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
