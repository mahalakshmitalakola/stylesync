color_harmony <- function(top_rgb, bottom_rgb) {

  if (length(top_rgb) != 3 || length(bottom_rgb) != 3) {
    stop("RGB vectors must have length 3, values normalised 0-1")
  }

  # ── Convert RGB → HSV ───────────────────────────────────────────────────────
  # RGB Euclidean distance is perceptually wrong — two colours can look very
  # different but have a small RGB distance (e.g. dark blue vs dark purple).
  # HSV separates hue, saturation and brightness so differences map to what
  # the human eye actually perceives.
  rgb_to_hsv <- function(rgb) {
    r <- rgb[1]; g <- rgb[2]; b <- rgb[3]
    cmax  <- max(r, g, b)
    cmin  <- min(r, g, b)
    delta <- cmax - cmin

    # Value
    v <- cmax

    # Saturation
    s <- if (cmax == 0) 0 else delta / cmax

    # Hue (0-1 scale, wraps at 1)
    h <- if (delta == 0) {
      0
    } else if (cmax == r) {
      (((g - b) / delta) %% 6) / 6
    } else if (cmax == g) {
      (((b - r) / delta) + 2) / 6
    } else {
      (((r - g) / delta) + 4) / 6
    }

    c(h, s, v)
  }

  top_hsv    <- rgb_to_hsv(top_rgb)
  bottom_hsv <- rgb_to_hsv(bottom_rgb)

  # ── Hue distance — circular (hue wraps at 1) ────────────────────────────────
  hue_diff <- abs(top_hsv[1] - bottom_hsv[1])
  hue_diff <- min(hue_diff, 1 - hue_diff)   # shortest arc on the colour wheel

  # ── Saturation & value differences ─────────────────────────────────────────
  sat_diff <- abs(top_hsv[2] - bottom_hsv[2])
  val_diff <- abs(top_hsv[3] - bottom_hsv[3])

  # ── Weighted perceptual distance ────────────────────────────────────────────
  # Hue accounts for most of what we perceive as "clashing".
  # Saturation and value differences matter but are secondary.
  perceptual_dist <- sqrt(
    (hue_diff * 2.0) ^ 2 +   # hue weighted 2× — most important
    (sat_diff * 1.0) ^ 2 +   # saturation weighted 1×
    (val_diff * 0.5) ^ 2     # brightness weighted 0.5× — least important
  )

  # ── Max possible distance in this weighted space ────────────────────────────
  max_dist <- sqrt((0.5 * 2.0) ^ 2 + (1.0 * 1.0) ^ 2 + (1.0 * 0.5) ^ 2)

  # ── Score: 100 = identical colours, 0 = maximally different ────────────────
  score <- 100 - (perceptual_dist / max_dist) * 100
  score <- max(min(score, 100), 0)

  return(round(score, 2))
}
