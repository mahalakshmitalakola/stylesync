skin_tone_match <- function(score, face_img, top_color, bottom_color) {

  # ── Extract skin tone from face image ───────────────────────────────────────
  # extract_skin_tone() uses a proper skin-pixel mask so it returns the actual
  # skin tone category directly.
  skin_result <- tryCatch(
    extract_skin_tone(face_img),
    error = function(e) list(skin_tone = "unknown", confidence = 0)
  )

  skin_tone  <- skin_result$skin_tone
  confidence <- skin_result$confidence   # 0-1: fraction of face pixels = skin

  # If skin detection was unreliable (< 5 % skin pixels found), skip adjustment
  if (skin_tone == "unknown" || confidence < 0.05) {
    return(score)
  }

  # ── Detect outfit colour types ───────────────────────────────────────────────
  top_type    <- detect_color_type(top_color)
  bottom_type <- detect_color_type(bottom_color)
  outfit_colors <- c(top_type, bottom_type)

  bonus <- 0

  # ── Colour rules per skin tone ───────────────────────────────────────────────
  # Each tier:
  #   best   → +10  (highly flattering contrast / harmony)
  #   good   → +7   (works well)
  #   neutral→ +4   (no strong effect)
  #   clash  → -5   (visually unflattering — washes out or oversaturates)

  if (skin_tone == "light") {
    if (any(outfit_colors %in% c("blue", "purple", "green", "red", "navy"))) {
      bonus <- 10   # strong jewel tones create great contrast on light skin
    } else if (any(outfit_colors %in% c("black", "pink", "teal"))) {
      bonus <- 7
    } else if (any(outfit_colors %in% c("grey", "white", "beige"))) {
      bonus <- 4    # safe neutrals — no strong contrast
    } else if (any(outfit_colors %in% c("yellow", "orange"))) {
      bonus <- -5   # can wash out very light skin tones
    } else {
      bonus <- 4
    }
  }

  else if (skin_tone == "medium") {
    if (any(outfit_colors %in% c("red", "orange", "yellow", "green", "teal"))) {
      bonus <- 10   # warm and earthy tones complement medium skin beautifully
    } else if (any(outfit_colors %in% c("blue", "purple", "black"))) {
      bonus <- 7
    } else if (any(outfit_colors %in% c("white", "grey", "beige"))) {
      bonus <- 4
    } else {
      bonus <- 4
    }
  }

  else if (skin_tone == "tan") {
    if (any(outfit_colors %in% c("white", "yellow", "orange", "green", "olive"))) {
      bonus <- 10   # bright and warm colours pop against tan skin
    } else if (any(outfit_colors %in% c("blue", "navy", "black", "teal"))) {
      bonus <- 7
    } else if (any(outfit_colors %in% c("beige", "brown"))) {
      bonus <- -5   # too close to skin tone — creates a monochromatic wash
    } else {
      bonus <- 4
    }
  }

  else if (skin_tone == "deep") {
    if (any(outfit_colors %in% c("white", "yellow", "orange", "pink", "red"))) {
      bonus <- 10   # high contrast colours are most striking on deep skin
    } else if (any(outfit_colors %in% c("blue", "purple", "green", "teal"))) {
      bonus <- 7
    } else if (any(outfit_colors %in% c("black", "navy", "brown"))) {
      bonus <- -5   # low contrast — dark on dark loses definition
    } else {
      bonus <- 4
    }
  }

  score <- score + bonus
  return(max(min(score, 100), 0))
}
