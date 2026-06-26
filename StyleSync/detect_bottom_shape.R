detect_bottom_shape <- function(image_path) {

  library(imager)

  # ── Load, greyscale, resize ─────────────────────────────────────────────────
  img  <- imager::load.image(image_path)
  gray <- imager::grayscale(img)

  # 200 × 350 — taller aspect ratio preserves trouser/skirt proportions
  gray <- imager::resize(gray, 200, 350)

  # ── Edge detection ──────────────────────────────────────────────────────────
  grad     <- imager::imgradient(gray, "xy")
  edge_mag <- sqrt(grad$dx ^ 2 + grad$dy ^ 2)

  # 85th-percentile threshold keeps only strong structural edges,
  # reducing noise from fabric texture and seam lines
  threshold <- quantile(as.vector(edge_mag), 0.85)
  binary    <- as.matrix(edge_mag > threshold)

  coords <- which(binary, arr.ind = TRUE)

  if (nrow(coords) < 80) {
    return("unknown")
  }

  y_vals <- coords[, 1]

  # ── Four vertical regions ───────────────────────────────────────────────────
  # Waist:  top 25 % — waistband / hip area
  # Hip:    25–50 % — fullest hip point
  # Thigh:  50–75 % — mid-leg width
  # Hem:    bottom 25 % — where the garment ends
  waist_pts <- coords[y_vals <  quantile(y_vals, 0.25), ]
  hip_pts   <- coords[y_vals >= quantile(y_vals, 0.25) &
                      y_vals <  quantile(y_vals, 0.50), ]
  thigh_pts <- coords[y_vals >= quantile(y_vals, 0.50) &
                      y_vals <  quantile(y_vals, 0.75), ]
  hem_pts   <- coords[y_vals >= quantile(y_vals, 0.75), ]

  # Require at least 10 edge points per region
  if (nrow(waist_pts) < 10 ||
      nrow(hip_pts)   < 10 ||
      nrow(thigh_pts) < 10 ||
      nrow(hem_pts)   < 10) {
    return("unknown")
  }

  waist_w <- max(waist_pts[, 2]) - min(waist_pts[, 2])
  hip_w   <- max(hip_pts[, 2])   - min(hip_pts[, 2])
  thigh_w <- max(thigh_pts[, 2]) - min(thigh_pts[, 2])
  hem_w   <- max(hem_pts[, 2])   - min(hem_pts[, 2])

  # Guard against zero division
  if (thigh_w == 0 || waist_w == 0) return("unknown")

  hem_ratio   <- hem_w   / thigh_w   # > 1.30 = flared, < 0.70 = tapered
  hip_ratio   <- hip_w   / waist_w   # > 1.25 = clear hip flare (a-line signal)
  thigh_ratio <- thigh_w / waist_w   # > 1.15 = wide leg

  # ── Classification ──────────────────────────────────────────────────────────

  # Flared / A-line: hem significantly wider than thigh
  # 1.30 threshold: hem at least 30 % wider — skirts, flared trousers
  if (hem_ratio > 1.30) {
    return("flared")
  }

  # Wide leg: thigh much wider than waist — palazzo, wide-leg trousers
  # 1.15 threshold: thigh at least 15 % wider than waist
  if (thigh_ratio > 1.15 && hem_ratio >= 0.85) {
    return("wide_leg")
  }

  # Tapered: hem significantly narrower than thigh — chinos, tapered joggers
  # 0.70 threshold: hem at least 30 % narrower than thigh
  if (hem_ratio < 0.70) {
    return("tapered")
  }

  # Straight: hem ≈ thigh width, within 10 % — classic straight-leg
  if (abs(hem_w - thigh_w) / thigh_w < 0.10) {
    return("straight")
  }

  # Skinny: thigh narrower than hip and waist — very close fit throughout
  if (thigh_w < waist_w * 0.90 && hem_w < waist_w * 0.85) {
    return("skinny")
  }

  # Bootcut: slight flare only at hem (hem slightly wider than thigh)
  if (hem_ratio >= 1.05 && hem_ratio <= 1.30) {
    return("bootcut")
  }

  return("straight")   # sensible default
}
