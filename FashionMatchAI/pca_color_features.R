pca_color_features <- function(color_matrix) {

  if (!is.matrix(color_matrix) && !is.data.frame(color_matrix)) {
    stop("color_matrix must be a matrix or data frame")
  }

  # Fixed: guard lowered from 3 to 2.
  # predict_outfit() passes a richer palette matrix now (multiple rows per
  # garment from extract_color(return_palette=TRUE)), but we still protect
  # against any accidental 1-row call.
  if (nrow(color_matrix) < 2) {
    stop("Need at least 2 colour samples for PCA")
  }

  # ── PCA ─────────────────────────────────────────────────────────────────────
  # scale. = TRUE normalises each channel — important because R, G, B can have
  # very different variances depending on the garment colour palette.
  # When nrow == 2, prcomp returns 1 PC (rank-1 matrix) — that's fine, we
  # fall through to the 2-cluster default below rather than crashing.
  pca_model <- prcomp(color_matrix, scale. = TRUE)

  features      <- pca_model$x
  var_explained <- summary(pca_model)$importance["Proportion of Variance", ]

  # ── Optimal k via elbow method ───────────────────────────────────────────────
  n     <- nrow(features)
  max_k <- min(6, n - 1)

  # Not enough rows for elbow — default to "complementary" and return early
  if (max_k < 2) {
    return(list(
      model         = pca_model,
      features      = features,
      var_explained = var_explained,
      wss           = NA,
      optimal_k     = 2,
      clusters      = rep(1, n),
      palette_style = "complementary"
    ))
  }

  wss    <- numeric(max_k)
  wss[1] <- sum(scale(features, scale = FALSE) ^ 2)

  for (k in 2:max_k) {
    set.seed(42)
    km     <- kmeans(features, centers = k, nstart = 15, iter.max = 100)
    wss[k] <- km$tot.withinss
  }

  if (max_k >= 3) {
    drops     <- diff(wss)
    gains     <- diff(drops)
    optimal_k <- which.max(gains) + 1
    optimal_k <- max(2, min(optimal_k, max_k))
  } else {
    optimal_k <- 2
  }

  # ── Final clustering at optimal k ───────────────────────────────────────────
  set.seed(42)
  km_final <- kmeans(features, centers = optimal_k, nstart = 15, iter.max = 100)

  # ── Style label ─────────────────────────────────────────────────────────────
  palette_style <- if (optimal_k == 2) {
    sizes <- sort(table(km_final$cluster), decreasing = TRUE)
    ratio <- sizes[1] / sum(sizes)
    if (ratio > 0.75) "minimal" else "complementary"
  } else if (optimal_k == 3) {
    "triadic"
  } else {
    "eclectic"
  }

  return(list(
    model         = pca_model,
    features      = features,
    var_explained = var_explained,
    wss           = wss,
    optimal_k     = optimal_k,
    clusters      = km_final$cluster,
    palette_style = palette_style
  ))
}
