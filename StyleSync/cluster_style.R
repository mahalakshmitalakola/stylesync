cluster_style <- function(pca_features, k = NULL) {

  # pca_features: the $features matrix returned by pca_color_features()
  # k:            if NULL, optimal k is selected automatically via silhouette

  if (!is.matrix(pca_features) && !is.data.frame(pca_features)) {
    stop("pca_features must be a matrix or data frame (use $features from pca_color_features)")
  }

  n <- nrow(pca_features)

  if (n < 3) {
    stop("Need at least 3 samples to cluster")
  }

  # ── Auto-select k using average silhouette width ────────────────────────────
  # Silhouette score measures how well each point fits its cluster vs the next
  # nearest cluster. Score range: -1 (wrong cluster) to +1 (perfect fit).
  # We pick the k that maximises average silhouette width.
  if (is.null(k)) {

    max_k   <- min(6, n - 1)
    sil_avg <- numeric(max_k)
    sil_avg[1] <- 0   # silhouette undefined for k=1

    for (ki in 2:max_k) {
      set.seed(42)
      km      <- kmeans(pca_features, centers = ki, nstart = 15, iter.max = 100)
      dists   <- dist(pca_features)

      # Manual silhouette calculation (avoids cluster package dependency)
      sil_vals <- numeric(n)
      for (i in seq_len(n)) {
        own_cluster   <- km$cluster[i]
        own_members   <- which(km$cluster == own_cluster)
        other_members <- which(km$cluster != own_cluster)

        a_i <- if (length(own_members) > 1) {
          mean(as.matrix(dists)[i, own_members[own_members != i]])
        } else {
          0
        }

        # b_i: mean distance to nearest other cluster
        other_clusters <- setdiff(unique(km$cluster), own_cluster)
        b_i <- min(sapply(other_clusters, function(c_k) {
          members <- which(km$cluster == c_k)
          mean(as.matrix(dists)[i, members])
        }))

        sil_vals[i] <- if (max(a_i, b_i) == 0) 0 else (b_i - a_i) / max(a_i, b_i)
      }
      sil_avg[ki] <- mean(sil_vals)
    }

    k <- which.max(sil_avg)
    k <- max(2, k)   # never below 2
  }

  # ── Final k-means at chosen k ───────────────────────────────────────────────
  set.seed(42)
  km_final <- kmeans(
    pca_features,
    centers  = k,
    nstart   = 20,     # more starts = more stable solution
    iter.max = 100
  )

  # ── Style label for each cluster ────────────────────────────────────────────
  # Centres are in PC space; interpret by size (large cluster = base palette)
  cluster_sizes  <- table(km_final$cluster)
  dominant_clust <- as.integer(names(which.max(cluster_sizes)))
  accent_clust   <- as.integer(names(which.min(cluster_sizes)))

  cluster_labels <- rep("secondary", k)
  cluster_labels[dominant_clust] <- "dominant"
  cluster_labels[accent_clust]   <- "accent"

  return(list(
    cluster        = km_final$cluster,       # cluster assignment per sample
    centers        = km_final$centers,       # cluster centroids in PC space
    withinss       = km_final$withinss,      # WSS per cluster
    tot_withinss   = km_final$tot.withinss,  # total WSS
    k_chosen       = k,
    cluster_labels = cluster_labels,         # "dominant" | "accent" | "secondary"
    dominant_clust = dominant_clust,
    accent_clust   = accent_clust
  ))
}
