predict_outfit <- function(top_img,
                           bottom_img,
                           face_img   = NULL,
                           mood,
                           undertone,
                           body_shape = NULL) {

  undertone <- tolower(undertone)
  mood      <- tolower(mood)

  # ── 1. Extract dominant colours ─────────────────────────────────────────────
  top_color    <- extract_color(top_img)
  bottom_color <- extract_color(bottom_img)

  # ── 2. PCA colour features & palette style ──────────────────────────────────
  # Use return_palette = TRUE to get a richer colour matrix (k clusters per
  # garment) so pca_color_features() always receives more than 2 rows.
  top_palette    <- extract_color(top_img,    return_palette = TRUE)
  bottom_palette <- extract_color(bottom_img, return_palette = TRUE)

  palette_colors <- rbind(
    do.call(rbind, lapply(top_palette$palette,    function(x) x$color)),
    do.call(rbind, lapply(bottom_palette$palette, function(x) x$color))
  )

  pca_result    <- pca_color_features(palette_colors)
  palette_style <- pca_result$palette_style

  # ── 3. Base harmony score (HSV perceptual distance) ─────────────────────────
  score <- color_harmony(top_color, bottom_color)

  # ── 4. Undertone match ──────────────────────────────────────────────────────
  score <- undertone_match(score, top_color, bottom_color, undertone)

  # ── 5. Mood adjustment ──────────────────────────────────────────────────────
  score <- mood_adjustment(score, mood, top_color, bottom_color)

  # ── 6. Face shape detection ─────────────────────────────────────────────────
  face_shape       <- "unknown"
  skin_confidence  <- NA

  if (!is.null(face_img)) {
    face_data <- tryCatch(
      detect_face_shape(face_img),
      error = function(e) NULL
    )
    if (!is.null(face_data) && face_data$face_shape != "unknown") {
      face_shape <- face_data$face_shape
    }
  }

  # ── 7. Neckline detection & face compatibility ───────────────────────────────
  neckline <- tryCatch(
    detect_neckline(top_img),
    error = function(e) "unknown"
  )

  score <- neckline_face_match(score, neckline, face_shape)

  # ── 8. Skin tone compatibility ──────────────────────────────────────────────
  if (!is.null(face_img)) {
    skin_result     <- tryCatch(
      extract_skin_tone(face_img),
      error = function(e) list(skin_tone = "unknown", confidence = 0)
    )
    skin_confidence <- skin_result$confidence
    score           <- skin_tone_match(score, face_img, top_color, bottom_color)
  }

  # ── 9. Clothing silhouette (top & bottom) ───────────────────────────────────
  top_shape    <- tryCatch(detect_top_shape(top_img),       error = function(e) "unknown")
  bottom_shape <- tryCatch(detect_bottom_shape(bottom_img), error = function(e) "unknown")

  # ── 10. Body shape compatibility ────────────────────────────────────────────
  if (!is.null(body_shape) && body_shape != "") {
    score <- body_shape_match(score, body_shape, top_shape, bottom_shape)
  }

  # ── 11. Final clamp ─────────────────────────────────────────────────────────
  score <- max(min(score, 100), 0)

  # ── 12. Overall confidence ───────────────────────────────────────────────────
  # Combines skin detection confidence with whether face/neckline were detected.
  # Ranges 0-1: higher = more inputs were successfully read.
  known_signals <- sum(c(
    face_shape != "unknown",
    neckline   != "unknown",
    top_shape  != "unknown",
    bottom_shape != "unknown",
    !is.na(skin_confidence) && skin_confidence > 0.05
  ))
  overall_confidence <- round(known_signals / 5, 2)

  # ── Return structured result ────────────────────────────────────────────────
  return(list(

    Compatibility_Score  = round(score, 1),
    Overall_Confidence   = overall_confidence,   # 0-1

    # Colours
    Top_Color            = top_color,
    Bottom_Color         = bottom_color,
    Top_Color_Type       = detect_color_type(top_color),
    Bottom_Color_Type    = detect_color_type(bottom_color),

    # PCA / palette analysis
    Palette_Style        = palette_style,
    PCA_Optimal_K        = pca_result$optimal_k,

    # Silhouette
    Top_Shape            = top_shape,
    Bottom_Shape         = bottom_shape,

    # Face
    Face_Shape           = face_shape,
    Neckline             = neckline,
    Skin_Confidence      = skin_confidence,

    # Context
    Mood                 = mood,
    Undertone            = undertone,
    Body_Shape           = if (!is.null(body_shape)) body_shape else "not provided"
  ))
}
