match_matrices <- function(mat1, mat2, algorithm = c("greedy", "hungarian")) {
  algorithm <- match.arg(algorithm)

  if (!all(dim(mat1) == dim(mat2)))
    stop("Matrices must have the same dimensions.")

  # Compute correlation matrix between columns of mat1 and mat2
  cor_mat <- cor(mat1, mat2, use = "pairwise.complete.obs")
  n <- ncol(mat1)

  if (algorithm == "greedy") {
    matching <- rep(NA, n)
    corr_values <- rep(NA, n)
    available1 <- 1:n
    available2 <- 1:n
    while(length(available1) > 0) {
      sub_cor <- cor_mat[available1, available2, drop = FALSE]
      max_idx <- which(abs(sub_cor) == max(abs(sub_cor), na.rm = TRUE), arr.ind = TRUE)[1, ]
      i <- available1[max_idx[1]]
      j <- available2[max_idx[2]]
      matching[i] <- j
      corr_values[i] <- cor_mat[i, j]
      available1 <- setdiff(available1, i)
      available2 <- setdiff(available2, j)
    }
  } else if (algorithm == "hungarian") {
    if (!requireNamespace("RcppHungarian", quietly = TRUE))
      stop("Package 'RcppHungarian' is required for the Hungarian algorithm. Please install it.")
    # For maximum matching based on absolute correlation, convert to a cost matrix.
    cost_mat <- -abs(cor_mat)
    # Hungarian returns a vector: for each row (mat1 column), the corresponding mat2 column index.
    matching <- RcppHungarian::HungarianSolver(cost_mat)
    corr_values <- cor_mat[matching$pairs]
  }

  # Reorder mat2 columns to match mat1
  #mat2_reordered <- mat2[, matching, drop = FALSE]
  list(matching = matching,
       correlation = corr_values,
       mcc = mean(abs(corr_values)))
}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @return
#' @author rdinnage
#' @export
# model_runs <- model_runs_2.5 |>
#   unnest(cols = c(scalars, flags)) |>
#   filter(active_dims == 5)
run_mcc_analysis <- function(model_runs) {

  manifold_var_files <- file.path(model_runs$dir, "output/vae_manifold_vars.csv")
  bioman_run_dat <- map(manifold_var_files, read_csv)

  combs <- combn(1:length(bioman_run_dat), 2)

  all_mcc <- map(array_branch(combs, 2),
                 ~ match_matrices(bioman_run_dat[[.x[[1]]]] |>
                                    select(starts_with("manifold_")),
                                  bioman_run_dat[[.x[[2]]]] |>
                                    select(starts_with("manifold_")),
                                  algorithm = "hungarian"),
                 .progress = TRUE)

  names(all_mcc) <- paste(model_runs$label[combs[1, ]], model_runs$label[combs[2, ]],
                          sep = "<-->")

  dmat <- matrix(0, nrow = length(bioman_run_dat), ncol = length(bioman_run_dat))
  dmat[t(combs)] <- map_dbl(all_mcc, "mcc")
  clust <- hclust(as.dist(t(1 - dmat)))

  list(all_mcc = all_mcc, mean_mcc = mean(map_dbl(all_mcc, "mcc")), clust = clust, mcc_mat = dmat,
       mean_repeat = colMeans(abs(do.call(rbind, map(all_mcc, "correlation")))))

}
