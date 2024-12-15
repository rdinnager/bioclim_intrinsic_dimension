library(torch)

AngleEmbedding <- nn_module(
  "AngleEmbedding",
  initialize = function(embedding_dim = 32) {
    self$embedding_dim <- embedding_dim
    # Ensure embedding_dim is even for sine/cosine pairs
    if (embedding_dim %% 2 != 0) {
      stop("embedding_dim must be even")
    }

    # Create frequency bands for different scales
    self$freq_bands <- nn_buffer(2^torch_arange(0, (embedding_dim/2) - 1))
  },

  forward = function(angles) {
    # angles should be in radians
    # Expand angles to match frequency bands
    angles_expanded <- angles$unsqueeze(2)  # [batch, 1]

    # Calculate sine and cosine components
    freqs <- angles_expanded * self$freq_bands

    # Compute sin and cos
    sin_components <- torch_sin(freqs)
    cos_components <- torch_cos(freqs)

    # Interleave sin and cos components
    embeddings <- torch_zeros(angles$size(1), self$embedding_dim, device = angles$device)
    embeddings[, seq(1, self$embedding_dim, 2)] <- sin_components$squeeze(2)
    embeddings[, seq(2, self$embedding_dim, 2)] <- cos_components$squeeze(2)

    embeddings
  }
)

library(ggplot2)
library(tidyr)
library(dplyr)

#' Create angle embeddings using sine/cosine encoding
#'
#' @param angles Numeric vector of angles in radians
#' @param embedding_dim Integer specifying embedding dimension (must be even)
#' @return Matrix of dimensions length(angles) x embedding_dim containing embeddings
create_angle_embedding <- function(angles, embedding_dim = 32) {
  if (embedding_dim %% 2 != 0) {
    stop("embedding_dim must be even")
  }

  n_angles <- length(angles)
  freq_bands <- 2^(0:(embedding_dim/2 - 1))
  embeddings <- matrix(0, nrow = n_angles, ncol = embedding_dim)

  angle_freq <- outer(angles, freq_bands)
  embeddings[, seq(1, embedding_dim, 2)] <- sin(angle_freq)
  embeddings[, seq(2, embedding_dim, 2)] <- cos(angle_freq)

  embeddings
}

#' Create visualizations for angle embeddings
#'
#' @param angles Numeric vector of angles in radians
#' @param embedding_dim Integer specifying embedding dimension (must be even)
#' @return List containing multiple ggplot objects
visualize_embeddings <- function(angles, embedding_dim = 32) {
  # Generate embeddings
  embeddings <- create_angle_embedding(angles, embedding_dim)

  # Create properly padded dimension names
  dim_names <- sprintf("dim_%02d", 1:embedding_dim)

  # Create data frame for plotting
  embedding_df <- as.data.frame(embeddings)
  colnames(embedding_df) <- dim_names
  embedding_df$angle <- angles
  embedding_df$angle_degrees <- angles * 180 / pi

  # 1. Heatmap of embeddings
  long_df <- embedding_df %>%
    pivot_longer(cols = starts_with("dim_"),
                 names_to = "dimension",
                 values_to = "value")

  heatmap_plot <- ggplot(long_df, aes(x = dimension, y = angle_degrees, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +
    labs(title = "Embedding Values Across Dimensions",
         x = "Embedding Dimension",
         y = "Angle (degrees)",
         fill = "Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # 2. First few dimensions as functions of angle
  first_dims_df <- embedding_df %>%
    select(angle_degrees, dim_01, dim_02, dim_03, dim_04) %>%
    pivot_longer(cols = starts_with("dim_"),
                 names_to = "dimension",
                 values_to = "value")

  dimension_plot <- ggplot(first_dims_df, aes(x = angle_degrees, y = value, color = dimension)) +
    geom_line() +
    theme_minimal() +
    labs(title = "First Four Embedding Dimensions",
         x = "Angle (degrees)",
         y = "Embedding Value",
         color = "Dimension")

  # 3. Frequency spectrum visualization
  freq_df <- data.frame(
    frequency = rep(2^(0:(embedding_dim/2 - 1)), each = 2),
    dimension = sprintf("dim_%02d", 1:embedding_dim),
    type = rep(c("sin", "cos"), embedding_dim/2)
  )

  spectrum_plot <- ggplot(freq_df, aes(x = dimension, y = frequency, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_log10() +
    theme_minimal() +
    labs(title = "Frequency Spectrum of Embedding Dimensions",
         x = "Dimension",
         y = "Frequency (log scale)",
         fill = "Component Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Return list of plots
  list(
    heatmap = heatmap_plot,
    dimensions = dimension_plot,
    spectrum = spectrum_plot
  )
}

# Example usage:
demonstrate_embeddings <- function() {
  # Create sequence of angles
  angles <- seq(0, 2*pi, length.out = 1000)

  # Generate visualizations
  plots <- visualize_embeddings(angles, embedding_dim = 32)

  # Print plots one by one
  print(plots$heatmap)
  print(plots$dimensions)
  print(plots$spectrum)
}

#demonstrate_embeddings()
