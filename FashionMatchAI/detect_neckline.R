detect_neckline <- function(top_img) {

  library(imager)

  # ── Load & normalise ────────────────────────────────────────────────────────
  img <- load.image(top_img)

  # Resize to fixed width for consistent measurements
  img <- resize(img, 300, 400)

  h <- height(img)
  w <- width(img)

  # ── Crop to neckline zone ───────────────────────────────────────────────────
  # The neckline sits in the top 35 % of a clothing image.
  # Using imsub() which is the correct imager crop helper.
  neckline_zone <- imsub(img,
                         x %inr% c(1, w),
                         y %inr% c(1, round(h * 0.35)))

  # ── Convert to greyscale & detect edges ────────────────────────────────────
  gray <- grayscale(neckline_zone)

  grad     <- imgradient(gray, "xy")
  edge_mag <- sqrt(grad$dx ^ 2 + grad$dy ^ 2)

  # Threshold: keep top 15 % of edge pixels
  threshold <- quantile(as.vector(edge_mag), 0.85)
  binary    <- as.matrix(edge_mag > threshold)

  # Not enough edge signal → can't classify
  if (sum(binary) < 80) {
    return("unknown")
  }

  nz_h <- nrow(binary)   # rows = y
  nz_w <- ncol(binary)   # cols = x

  # ── Regional edge sums ──────────────────────────────────────────────────────
  left_band   <- binary[, 1:round(nz_w * 0.28)]
  right_band  <- binary[, round(nz_w * 0.72):nz_w]
  center_band <- binary[, round(nz_w * 0.36):round(nz_w * 0.64)]

  top_band    <- binary[1:round(nz_h * 0.35), ]
  bottom_band <- binary[round(nz_h * 0.65):nz_h, ]

  left_sum   <- sum(left_band)
  right_sum  <- sum(right_band)
  center_sum <- sum(center_band)
  top_sum    <- sum(top_band)
  bottom_sum <- sum(bottom_band)
  total_sum  <- sum(binary)

  # ── Symmetry helpers ────────────────────────────────────────────────────────
  side_sum        <- left_sum + right_sum
  left_right_diff <- abs(left_sum - right_sum)
  is_symmetric    <- left_right_diff < 0.25 * side_sum

  # ── Classification rules ────────────────────────────────────────────────────
  # V-neck:  strong centre diagonal edges, centre dominates sides
  if (center_sum > 0.5 * total_sum && center_sum > side_sum) {
    return("v-neck")
  }

  # Boat neck: edges concentrated along the top horizontal band, symmetric
  if (top_sum > bottom_sum * 1.4 && is_symmetric && top_sum > center_sum) {
    return("boat neck")
  }

  # Round / Scoop: edges heavier in bottom of zone, symmetric, curved spread
  if (bottom_sum > top_sum * 1.3 && is_symmetric) {
    return("round / scoop")
  }

  # Square: fairly even distribution across top + sides, symmetric
  if (is_symmetric &&
      abs(top_sum - bottom_sum) < 0.3 * total_sum &&
      side_sum > center_sum) {
    return("square")
  }

  # Halter: strong asymmetry between left and right sides
  if (left_right_diff > 0.45 * side_sum) {
    return("halter")
  }

  # Off-shoulder: heavy edge signal on both outer bands, centre quiet
  if (side_sum > center_sum * 2 && is_symmetric) {
    return("off-shoulder")
  }

  return("unknown")
}
