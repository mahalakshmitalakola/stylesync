# ── FashionMatchAI — Test runner ─────────────────────────────────────────────
# Run this file to verify the full pipeline works end to end.
# Usage: source("test_run.R")

source("launch_dashboard.R")
load_fashion_modules()

cat("\n====================================================\n")
cat("  FashionMatchAI — Pipeline Test\n")
cat("====================================================\n\n")

# ── Helper: print result cleanly ─────────────────────────────────────────────
print_result <- function(label, result) {
  cat(sprintf("[ %s ]\n", label))
  cat(sprintf("  Compatibility Score : %s / 100\n",  result$Compatibility_Score))
  cat(sprintf("  Overall Confidence  : %s\n",         result$Overall_Confidence))
  cat(sprintf("  Palette Style       : %s\n",         result$Palette_Style))
  cat(sprintf("  Top Shape           : %s\n",         result$Top_Shape))
  cat(sprintf("  Bottom Shape        : %s\n",         result$Bottom_Shape))
  cat(sprintf("  Face Shape          : %s\n",         result$Face_Shape))
  cat(sprintf("  Neckline            : %s\n",         result$Neckline))
  cat(sprintf("  Top Colour Type     : %s\n",         result$Top_Color_Type))
  cat(sprintf("  Bottom Colour Type  : %s\n",         result$Bottom_Color_Type))
  cat(sprintf("  Mood                : %s\n",         result$Mood))
  cat(sprintf("  Undertone           : %s\n",         result$Undertone))
  cat(sprintf("  Body Shape          : %s\n",         result$Body_Shape))
  cat("\n")
}

# ── Unit tests: individual functions ─────────────────────────────────────────
cat("--- Unit tests ---\n\n")

# color_harmony: identical colours should score 100
h1 <- color_harmony(c(0.5, 0.2, 0.1), c(0.5, 0.2, 0.1))
cat(sprintf("color_harmony (identical)   : %s  [expected: 100]\n", h1))

# color_harmony: opposite colours should score low
h2 <- color_harmony(c(1, 0, 0), c(0, 1, 0))
cat(sprintf("color_harmony (red vs green): %s  [expected: < 50]\n", h2))

# extract_skin_tone: returns correct structure
dummy_face <- system.file("img", "parrots.png", package = "imager")
if (file.exists(dummy_face)) {
  st <- extract_skin_tone(dummy_face)
  cat(sprintf("extract_skin_tone structure : skin_tone=%s confidence=%s\n",
              st$skin_tone, st$confidence))
} else {
  cat("extract_skin_tone           : [skip — no test image found]\n")
}

# pca_color_features: should not crash with 6-row matrix
mat <- matrix(c(0.8,0.2,0.1, 0.1,0.7,0.3, 0.5,0.5,0.5,
                0.9,0.1,0.2, 0.2,0.8,0.4, 0.6,0.6,0.6),
              nrow = 6, byrow = TRUE)
pca_ok <- tryCatch({ pca_color_features(mat); "PASS" }, error = function(e) paste("FAIL:", e$message))
cat(sprintf("pca_color_features (6 rows) : %s\n", pca_ok))

# body_shape_match: hourglass + fitted top should add points
bsm <- body_shape_match(60, "hourglass", "fitted", "skinny")
cat(sprintf("body_shape_match            : %s  [expected: 80]\n", bsm))

cat("\n--- Integration test (requires real image files) ---\n\n")

# ── Integration test ─────────────────────────────────────────────────────────
# Replace these paths with real images to run the full pipeline.
top_img    <- "sample_images/top.jpg"
bottom_img <- "sample_images/bottom.jpg"
face_img   <- "sample_images/face.jpg"

if (file.exists(top_img) && file.exists(bottom_img)) {

  # Test 1: without face image
  result1 <- tryCatch(
    predict_outfit(
      top_img    = top_img,
      bottom_img = bottom_img,
      mood       = "casual",
      undertone  = "warm"
    ),
    error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(result1)) print_result("No face image", result1)

  # Test 2: with face image and body shape
  if (file.exists(face_img)) {
    result2 <- tryCatch(
      predict_outfit(
        top_img    = top_img,
        bottom_img = bottom_img,
        face_img   = face_img,
        mood       = "professional",
        undertone  = "cool",
        body_shape = "hourglass"
      ),
      error = function(e) { cat("ERROR:", e$message, "\n"); NULL }
    )
    if (!is.null(result2)) print_result("With face + body shape", result2)
  }

} else {
  cat("Integration test skipped — add images to sample_images/ folder:\n")
  cat("  top.jpg, bottom.jpg, face.jpg\n\n")
}

cat("====================================================\n")
cat("  Tests complete\n")
cat("====================================================\n")
