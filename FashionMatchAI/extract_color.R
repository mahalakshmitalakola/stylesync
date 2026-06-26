extract_color <- function(image_path, k = NULL, return_palette = FALSE) {

  library(imager)

  img <- load.image(image_path)
  img <- resize(img, 120, 120)

  r <- as.vector(img[, , , 1])
  g <- as.vector(img[, , , 2])
  b <- as.vector(img[, , , 3])

  # ── Background removal (single combined mask — fixes index misalignment) ────
  # Original filtered pixels and saturation in two separate steps, leaving
  # sat_keep and pixels with different row counts so indexing was wrong.
  # Fixed: build one logical mask applied to all three together.
  brightness <- (r + g + b) / 3
  saturation <- pmax(r, g, b) - pmin(r, g, b)

  keep <- (
    brightness > 0.06 &    # not near-black
    brightness < 0.94 &    # not near-white
    (saturation > 0.04 |   # meaningful colour, OR
     brightness < 0.35)    # dark garment (low brightness but valid)
  )

  r_k <- r[keep]; g_k <- g[keep]; b_k <- b[keep]

  if (length(r_k) < 50) {
    if (return_palette) return(list(dominant = c(0.5, 0.5, 0.5), palette = NULL))
    return(c(0.5, 0.5, 0.5))
  }

  pixels <- data.frame(r = r_k, g = g_k, b = b_k)

  # ── Auto-select k ────────────────────────────────────────────────────────────
  if (is.null(k)) {
    k <- if (nrow(pixels) < 5000) 3 else 5
  }
  k <- max(2, min(k, 8))

  # ── K-means ──────────────────────────────────────────────────────────────────
  set.seed(42)
  km <- kmeans(pixels, centers = k, nstart = 15, iter.max = 100)

  cluster_sizes   <- table(km$cluster)
  sorted_clusters <- as.numeric(names(sort(cluster_sizes, decreasing = TRUE)))

  dominant_cluster <- sorted_clusters[1]
  dominant_color   <- as.numeric(km$centers[dominant_cluster, ])

  if (!return_palette) {
    return(dominant_color)
  }

  palette <- lapply(sorted_clusters, function(cl) {
    list(
      color      = as.numeric(km$centers[cl, ]),
      proportion = as.numeric(cluster_sizes[as.character(cl)]) / nrow(pixels)
    )
  })

  return(list(
    dominant = dominant_color,
    palette  = palette
  ))
}
