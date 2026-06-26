detect_top_shape <- function(image_path) {

  library(imager)

  # ── Load, greyscale, resize ─────────────────────────────────────────────────
  img  <- load.image(image_path)
  gray <- grayscale(img)

  # 200 × 300 — taller than square to preserve torso proportions
  gray <- resize(gray, 200, 300)

  std_w <- width(gray)
  std_h <- height(gray)

  # ── Edge detection ──────────────────────────────────────────────────────────
  grad     <- imgradient(gray, "xy")
  edge_mag <- sqrt(grad$dx ^ 2 + grad$dy ^ 2)

  # 80th-percentile threshold: more robust than mean against skin-texture noise
  threshold <- quantile(as.vector(edge_mag), 0.80)
  binary    <- as.matrix(edge_mag > threshold)

  coords <- which(binary, arr.ind = TRUE)

  if (nrow(coords) < 80) {
    return("unknown")
  }

  y_vals <- coords[, 1]

  # ── Three vertical regions ──────────────────────────────────────────────────
  # Shoulder: top 30 % of the garment — widest point for most tops
  # Chest:    middle 30–60 % — bust / torso area
  # Waist:    bottom 60–100 % — where the top ends or tucks in
  shoulder_pts <- coords[y_vals <  quantile(y_vals, 0.30), ]
  chest_pts    <- coords[y_vals >= quantile(y_vals, 0.30) &
                         y_vals <  quantile(y_vals, 0.60), ]
  waist_pts    <- coords[y_vals >= quantile(y_vals, 0.60), ]

  # Require at least 10 edge points per region to measure width reliably
  if (nrow(shoulder_pts) < 10 ||
      nrow(chest_pts)    < 10 ||
      nrow(waist_pts)    < 10) {
    return("unknown")
  }

  shoulder_w <- max(shoulder_pts[, 2]) - min(shoulder_pts[, 2])
  chest_w    <- max(chest_pts[, 2])    - min(chest_pts[, 2])
  waist_w    <- max(waist_pts[, 2])    - min(waist_pts[, 2])

  # Guard against zero division
  if (shoulder_w == 0) return("unknown")

  waist_ratio <- waist_w  / shoulder_w   # < 0.70 = cinched, > 1.20 = flared
  chest_ratio <- chest_w  / shoulder_w   # > 1.05 = peplum / wrap flare

  # ── Classification ──────────────────────────────────────────────────────────

  # Tapered: shoulder > chest > waist — classic button-down or blazer taper
  if (shoulder_w > chest_w && chest_w > waist_w) {
    return("tapered")
  }

  # Fitted: waist significantly narrower than shoulder — bodycon / form-fit
  # Threshold 0.70: waist must be at least 30 % narrower than shoulder
  if (waist_ratio < 0.70) {
    return("fitted")
  }

  # Peplum / wrap: chest or waist flares OUT beyond shoulder
  # Threshold 1.10: chest at least 10 % wider than shoulder
  if (chest_ratio > 1.10 || waist_ratio > 1.20) {
    return("peplum")
  }

  # Straight: shoulder ≈ waist, within 10 % — boxy / relaxed fit
  if (abs(shoulder_w - waist_w) / shoulder_w < 0.10) {
    return("straight")
  }

  # Structured: shoulder much wider than waist (power-shoulder silhouette)
  # Threshold 1.30: shoulder at least 30 % wider than waist
  if (shoulder_w > waist_w * 1.30) {
    return("structured")
  }

  # Flowy: wide at every level, waist not cinched — oversized / tunic
  if (waist_ratio >= 1.0 && chest_ratio >= 0.95) {
    return("flowy")
  }

  return("wide")
}
