library(torch)
library(terra)
library(tidyverse)
library(zeallot)
library(santoku)
library(scico)
library(ggrepel)
library(stringr)
library(patchwork)
library(scales)
library(Polychrome)
library(wesanderson)

set.seed(1234)

vae <- torch_load("data/trained_vae_1.to")

mani_ecoreg <- rast("data/bioclim_manifold_all_data.tif")
bioclim_scaled_rast <- rast("data/bioclim_scaled.tif")

mani_ecoreg <- c(mani_ecoreg[[c(1:6)]], bioclim_scaled_rast)

bioclim_names <- tibble(orig = c("wc2.1_2.5m_bio_1",
                                  "wc2.1_2.5m_bio_10",
                                  "wc2.1_2.5m_bio_11",
                                  "wc2.1_2.5m_bio_12",
                                  "wc2.1_2.5m_bio_13",
                                  "wc2.1_2.5m_bio_14",
                                  "wc2.1_2.5m_bio_15",
                                  "wc2.1_2.5m_bio_16",
                                  "wc2.1_2.5m_bio_17",
                                  "wc2.1_2.5m_bio_18",
                                  "wc2.1_2.5m_bio_19",
                                  "wc2.1_2.5m_bio_2",
                                  "wc2.1_2.5m_bio_3",
                                  "wc2.1_2.5m_bio_4",
                                  "wc2.1_2.5m_bio_5",
                                  "wc2.1_2.5m_bio_6",
                                  "wc2.1_2.5m_bio_7",
                                  "wc2.1_2.5m_bio_8",
                                  "wc2.1_2.5m_bio_9"),
                        new_name = c("BIO01",
                                     "BIO10",
                                     "BIO11",
                                     "BIO12",
                                     "BIO13",
                                     "BIO14",
                                     "BIO15",
                                     "BIO16",
                                     "BIO17",
                                     "BIO18",
                                     "BIO19",
                                     "BIO02",
                                     "BIO03",
                                     "BIO04",
                                     "BIO05",
                                     "BIO06",
                                     "BIO07",
                                     "BIO08",
                                     "BIO09")
)

bioclim_cats <-
  tribble(~Variable, ~Description,                                                 ~Type,
          "BIO01",    "Annual Mean Temperature",                                    "Temperature Value",
          "BIO02",    "Mean Diurnal Range (Mean of monthly (max temp - min temp))", "Temperature Variation",
          "BIO03",    "Isothermality (BIO2/BIO7) (×100)",                           "Temperature Variation",
          "BIO04",    "Temperature Seasonality (standard deviation ×100)",          "Temperature Variation",
          "BIO05",    "Max Temperature of Warmest Month",                           "Temperature Value",
          "BIO06",    "Min Temperature of Coldest Month",                           "Temperature Value",
          "BIO07",    "Temperature Annual Range (BIO5-BIO6)",                       "Temperature Variation",
          "BIO08",    "Mean Temperature of Wettest Quarter",                        "Temperature Precipitation Interaction",
          "BIO09",    "Mean Temperature of Driest Quarter",                         "Temperature Precipitation Interaction",
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

ecoreg_names <- read_csv("data/ecoreg_names.csv") %>%
  mutate(BIOME_NUM = as.character(BIOME_NUM)) %>%
    mutate(BIOME_NAME = str_wrap(BIOME_NAME, 35))

mani_df <- as.data.frame(mani_ecoreg, na.rm = TRUE) %>%
  select(starts_with("BIOMAN"), layer, starts_with("wc2.1_2.5m_bio"))

ecoreg_pal <- createPalette(n_distinct(ecoreg_names$BIOME_NAME),
                            wes_palettes$FantasticFox1[-1])

names(ecoreg_pal) <- ecoreg_names$BIOME_NAME

S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
IS_sqrt <- function(x){x^2*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)

plot_manifold <- function(var_name, num_intervals = 25) {
  mani_1_df <- mani_df %>%
    mutate(cuts = chop_equally({{ var_name }}, num_intervals)) %>%
    group_by(cuts) %>%
    summarise(across(starts_with("wc2.1_2.5m_bio"),
                     ~ mean(.x, na.rm = TRUE)),
              biomes = list(table(layer)),
              count = n(),
              {{ var_name }} := mean({{ var_name }}, na.rm = TRUE))

  bio_vals <- mani_1_df %>%
    group_by(cuts) %>%
    summarise({{ var_name }} := {{ var_name }}[1])

  mani_1_biomes <- mani_1_df %>%
    select(cuts, biomes) %>%
    unnest_longer(biomes, values_to = "biome_count", indices_to = "biome_num") %>%
    mutate(biome_count = as.vector(biome_count)) %>%
    complete(cuts, biome_num, fill = list(biome_count = 0)) %>%
    left_join(bio_vals) %>%
    left_join(ecoreg_names, by = c("biome_num" = "BIOME_NUM")) %>%
    mutate(biome_count = as.vector(biome_count),
           BIOME_NAME = as.factor(BIOME_NAME)) %>%
    group_by(BIOME_NAME) %>%
    mutate(props = biome_count / sum(biome_count)) %>%
    group_by(cuts) %>%
    mutate(props = props / sum(props),
           props = pmax(props, 0.001))

  mani_1_biome_labels <- mani_1_biomes %>%
    filter(cuts == tail(levels(cuts), 1)) %>%
    ungroup() %>%
    arrange(desc(BIOME_NAME)) %>%
    mutate(ypos = cumsum(props)) %>%
    mutate(ypos = (ypos + lag(ypos, default = 0)) / 2)

  mani_1_df2 <- mani_1_df %>%
    select(-biomes, -count) %>%
    pivot_longer(c(-cuts, -{{ var_name }}), values_to = "val", names_to = "var") %>%
    left_join(bioclim_names, by = c("var" = "orig")) %>%
    left_join(bioclim_cats, by = c("new_name" = "Variable")) %>%
    mutate(new_name = reorder(as.factor(new_name), as.numeric(as.factor(Type))))

  mani_1_df2_labels <- mani_1_df2 %>%
    filter(cuts == tail(levels(cuts), 1)) %>%
    group_by(new_name) %>%
    summarise(Description = Description[1],
              x = {{ var_name }}[1]) %>%
    mutate(Description = str_wrap(Description, 35))

  mani_1_breaks <- mani_1_df2 %>%
    group_by(cuts) %>%
    summarise(x = {{ var_name }}[1])

  val_min <- min(mani_1_df2$val)
  val_max <- max(mani_1_df2$val)
  col_range <- c(min(min(mani_1_df2$val), -max(mani_1_df2$val)),
                 max(val_max, -val_min))

  start_col <- (val_min - col_range[1]) / (col_range[2] - col_range[1])
  end_col <- (val_max - col_range[1]) / (col_range[2] - col_range[1])

  p1 <- ggplot(mani_1_df2, aes({{ var_name }}, new_name)) +
    geom_tile(aes(fill = val)) +
    geom_label_repel(aes(x, new_name, label = Description), data = mani_1_df2_labels,
                    xlim = c(max(mani_1_df2_labels$x) + 0.35, NA),
                    ylim = c(-11, 30),
                    hjust = 0,
                    min.segment.length = 0,
                    force = 2) +
    scale_fill_scico(palette = "bam", begin = start_col, end = end_col,
                     trans = "S_sqrt",
                     name = "Scaled Value") +
    scale_x_continuous(expand = expansion(c(0, 0.75))
                       # breaks = mani_1_breaks$x,
                       # labels = mani_1_breaks$cuts,
                       ) +
    coord_cartesian(clip = "off") +
    ylab("") +
    theme_minimal() +
    #theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 0)) +
    theme(legend.position = c(0.93, 0.5),
          # legend.direction = "horizontal",
          # legend.text = element_text(angle = -45, hjust = 0, vjust = 0),
          plot.margin = unit(c(0.1, 00, 0.1, 0), "npc"))

  p1

  p1_biome <- ggplot(mani_1_biomes, aes({{ var_name }}, props, fill = BIOME_NAME)) +
    geom_area() +
    geom_label_repel(aes({{ var_name }}, ypos, label = BIOME_NAME), data = mani_1_biome_labels,
                    xlim = c(max(mani_1_biome_labels %>% pull({{ var_name }})) + 0.15, NA),
                    ylim = c(-0.5, 1.5),
                    hjust = 0,
                    min.segment.length = 0,
                    force = 2,
                    max.overlaps = 25) +
    scale_x_continuous(expand = expansion(c(0, 0.75))) +
    scale_y_continuous(limits = c(0, 1.1), expand = c(0.1, 0.1),
                       breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
    scale_fill_manual(values = ecoreg_pal) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.margin = unit(c(0.1, 0, 0.1, 0), "npc"))

  p1_biome

  p <- p1 + p1_biome + plot_layout(ncol = 1)

  p

}

p1 <- plot_manifold(BIOMAN1)
#cairo_pdf("figures/manifold_1.pdf", height = 15, width = 12)
plot(p1)
#dev.off()
p2 <- plot_manifold(BIOMAN2)
p2
p3 <- plot_manifold(BIOMAN3)
p3
p4 <- plot_manifold(BIOMAN4)
p4
p5 <- plot_manifold(BIOMAN5)
p5

pca_rast <- rast("data/bioclim_manifold_all_data.tif")[[c(7:25)]]
writeRaster(pca_rast, "data/bioclim_pca.tif")

