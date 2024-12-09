library(guildai)

n_runs <- 6

old_wd <- setwd("R")
for(i in 1:n_runs) {
  guild_run("bioclim_vae_2.5_guildai.R", tag = "regular")
}

for(i in 1:n_runs) {
  guild_run("bioclim_vae_2.5_ident.R", tag = "ident_ecoregion", as_job = FALSE)
}

for(i in 1:n_runs) {
  guild_run("bioclim_vae_2.5_ident_lat.R", tag = "ident_lat", as_job = FALSE)
}

for(i in 1:n_runs) {
  guild_run("bioclim_vae_2.5_ident_lat.R", tag = "ident_lat", as_job = FALSE,
            flags = list(loggamma_init = c(-3, -4)))
}

setwd(old_wd)
