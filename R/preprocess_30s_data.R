library(arrow)
library(terra)
library(santoku)
library(stringr)
library(purrr)

bioclim_files <- list.files("data/bioclim_30s", full.names = TRUE)
bioclim <- map(bioclim_files, terra::rast)
#bioclim <- do.call(c, bioclim)

# inds <- 1:nrow(bioclim)
# grps <- chop_equally(inds, 100)
# ind_list <- split(inds, grps)

test <- as.data.frame(bioclim[[1]], na.rm = TRUE, xy = TRUE)

make_parquet <- function(r, i, n) {
  df <- values(r[i, ], na.rm = TRUE, dataframe = TRUE, xy = TRUE)
  write_parquet(df, file.path("data", "bioclim_30s_parquet", paste0(
    "bioclim_", str_pad(n, 3, pad = "0")
  )))
}

iwalk(unname(ind_list),
      ~ make_parquet(bioclim, .x, .y))

