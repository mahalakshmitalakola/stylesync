extract_skin_tone <- function(face_img) {

  library(imager)

  # ── Load & resize ───────────────────────────────────────────────────────────
  img <- load.image(face_img)

  # Resize for speed — 150x150 is enough for skin tone
  img <- resize(img, 150, 150)

  # ── Extract RGB channels as flat vectors ────────────────────────────────────
  r <- as.vector(img[, , , 1])
  g <- as.vector(img[, , , 2])
  b <- as.vector(img[, , , 3])

  # ── Skin pixel mask (Peer et al. empirical rule in normalised RGB) ──────────
  # Works across all skin tones (light through deep):
  #   R > G > B, R above minimum, not too bright (exclude white background),
  #   not too dark (exclude shadow/hair), and R-G difference within skin range.
  skin_mask <- (
    r > 0.25 &          # not too dark
    r < 0.97 &          # not blown-out white background
    g > 0.15 &
    b > 0.05 &
    r > g &             # red channel leads (skin rule)
    r > b &
    (r - g) > 0.03 &    # meaningful red-green separation
    (r - b) > 0.05 &    # meaningful red-blue separation
    (r - g) < 0.50      # cap: excludes very saturated reds (clothing)
  )

  # ── Safety check ────────────────────────────────────────────────────────────
  n_skin <- sum(skin_mask)

  if (n_skin < 100) {
    return(list(
      skin_tone  = "unknown",
      avg_color  = c(NA, NA, NA),
      confidence = 0
    ))
  }

  # ── Average skin colour ─────────────────────────────────────────────────────
  avg_r <- mean(r[skin_mask])
  avg_g <- mean(g[skin_mask])
  avg_b <- mean(b[skin_mask])

  # ── Classify using perceptual brightness (weighted luminance) ───────────────
  # ITU-R BT.601 luma weights — more accurate than simple mean
  luminance <- 0.299 * avg_r + 0.587 * avg_g + 0.114 * avg_b

  skin_tone <- if (luminance > 0.75) {
    "light"
  } else if (luminance > 0.55) {
    "medium"
  } else if (luminance > 0.38) {
    "tan"
  } else {
    "deep"
  }

  # ── Confidence: fraction of image pixels classified as skin ─────────────────
  total_pixels <- length(r)
  confidence   <- round(n_skin / total_pixels, 3)

  return(list(
    skin_tone  = skin_tone,
    avg_color  = c(avg_r, avg_g, avg_b),   # normalised 0-1 RGB
    luminance  = round(luminance, 3),
    confidence = confidence                 # 0–1; higher = more skin detected
  ))
}


# ── Helper: classify skin tone directly from an avg_color vector ────────────
# Used by skin_match.R so it doesn't need to re-load the image
skin_tone_from_color <- function(avg_color) {

  avg_r <- avg_color[1]
  avg_g <- avg_color[2]
  avg_b <- avg_color[3]

  luminance <- 0.299 * avg_r + 0.587 * avg_g + 0.114 * avg_b

  if (luminance > 0.75) return("light")
  if (luminance > 0.55) return("medium")
  if (luminance > 0.38) return("tan")
  return("deep")
}
