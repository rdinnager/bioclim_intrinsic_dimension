library(biocman)
library(ggplot2)
library(stars)

bioman <- get_data("stars")

S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
IS_sqrt <- function(x){x^2*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)

############## bioman1 ##############

val_min <- min(bioman[[1]], na.rm = TRUE)
val_max <- max(bioman[[1]], na.rm = TRUE)
col_range <- c(min(min(bioman[[1]], na.rm = TRUE), -max(bioman[[1]], na.rm = TRUE)),
               max(val_max, -val_min))

start_col <- (val_min - col_range[1]) / (col_range[2] - col_range[1])
end_col <- (val_max - col_range[1]) / (col_range[2] - col_range[1])

p1 <- ggplot() +
  geom_stars(data = bioman[1]) +
  coord_equal() +
  theme_void() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_scico(palette = "vanimo", begin = start_col, end = end_col,
                   trans = "S_sqrt",
                   name = "Scaled Value",
                   na.value = "white")

p1

ggsave("figures/manifold_1_map_unprojected.pdf", p1)

p1_2 <- p1 +
  theme(legend.position = "none")

png("figures/manifold_1_map.png", width = 4320, height = 2160)
plot(p1_2)
dev.off()

############## bioman2 ##############

val_min <- min(bioman[[2]], na.rm = TRUE)
val_max <- max(bioman[[2]], na.rm = TRUE)
col_range <- c(min(min(bioman[[2]], na.rm = TRUE), -max(bioman[[2]], na.rm = TRUE)),
               max(val_max, -val_min))

start_col <- (val_min - col_range[1]) / (col_range[2] - col_range[1])
end_col <- (val_max - col_range[1]) / (col_range[2] - col_range[1])

p2 <- ggplot() +
  geom_stars(data = bioman[2]) +
  coord_equal() +
  theme_void() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_scico(palette = "vanimo", begin = start_col, end = end_col,
                   trans = "S_sqrt",
                   name = "Value",
                   na.value = "white")

p2

############## bioman3 ##############

val_min <- min(bioman[[3]], na.rm = TRUE)
val_max <- max(bioman[[3]], na.rm = TRUE)
col_range <- c(min(min(bioman[[3]], na.rm = TRUE), -max(bioman[[3]], na.rm = TRUE)),
               max(val_max, -val_min))

start_col <- (val_min - col_range[1]) / (col_range[2] - col_range[1])
end_col <- (val_max - col_range[1]) / (col_range[2] - col_range[1])

p3 <- ggplot() +
  geom_stars(data = bioman[3]) +
  coord_equal() +
  theme_void() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_scico(palette = "vanimo", begin = start_col, end = end_col,
                   trans = "S_sqrt",
                   name = "Value",
                   na.value = "white")

p3

ggsave("figures/manifold_3_map_unprojected.pdf", p3)

p3_2 <- p3 +
  theme(legend.position = "none")

png("figures/manifold_3_map.png", width = 4320, height = 2160)
plot(p3_2)
dev.off()


############## bioman4 ##############

val_min <- min(bioman[[4]], na.rm = TRUE)
val_max <- max(bioman[[4]], na.rm = TRUE)
col_range <- c(min(min(bioman[[4]], na.rm = TRUE), -max(bioman[[4]], na.rm = TRUE)),
               max(val_max, -val_min))

start_col <- (val_min - col_range[1]) / (col_range[2] - col_range[1])
end_col <- (val_max - col_range[1]) / (col_range[2] - col_range[1])

p4 <- ggplot() +
  geom_stars(data = bioman[4]) +
  coord_equal() +
  theme_void() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_scico(palette = "vanimo", begin = start_col, end = end_col,
                   trans = "S_sqrt",
                   name = "Value",
                   na.value = "white")

p4

ggsave("figures/manifold_4_map_unprojected.pdf", p4)

p4_2 <- p4 +
  theme(legend.position = "none")

png("figures/manifold_4_map.png", width = 4320, height = 2160)
plot(p4_2)
dev.off()


############## bioman5 ##############

val_min <- min(bioman[[5]], na.rm = TRUE)
val_max <- max(bioman[[5]], na.rm = TRUE)
col_range <- c(min(min(bioman[[5]], na.rm = TRUE), -max(bioman[[5]], na.rm = TRUE)),
               max(val_max, -val_min))

start_col <- (val_min - col_range[1]) / (col_range[2] - col_range[1])
end_col <- (val_max - col_range[1]) / (col_range[2] - col_range[1])

p5 <- ggplot() +
  geom_stars(data = bioman[5]) +
  coord_equal() +
  theme_void() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_scico(palette = "vanimo", begin = start_col, end = end_col,
                   trans = "S_sqrt",
                   name = "Value",
                   na.value = "white")

p5

ggsave("figures/manifold_5_map_unprojected.pdf", p5)

p5_2 <- p5 +
  theme(legend.position = "none")

png("figures/manifold_5_map.png", width = 4320, height = 2160)
plot(p5_2)
dev.off()


p1_notr <- ggplot() +
  geom_stars(data = bioman[1]) +
  coord_equal() +
  theme_void() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_scico(palette = "vanimo", begin = start_col, end = end_col,
                   name = "Value",
                   na.value = "white")

p1_notr

ggsave("figures/manifold_1_map_unprojected_notr.pdf", p1_notr)

p1_2_notr <- p1_notr +
  theme(legend.position = "none")

png("figures/manifold_1_map_notr.png", width = 4320, height = 2160)
plot(p1_2_notr)
dev.off()
