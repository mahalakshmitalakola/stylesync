test_that("color harmony scores identical colors higher than clashing colors", {
  identical_score <- color_harmony(c(0.5, 0.2, 0.1), c(0.5, 0.2, 0.1))
  clash_score <- color_harmony(c(1, 0, 0), c(0, 1, 0))

  expect_equal(identical_score, 100)
  expect_lt(clash_score, identical_score)
  expect_lt(clash_score, 60)
})

test_that("color temperature handles warm, cool, and zero-value colors", {
  expect_equal(color_temperature(c(1, 0.2, 0.1)), "warm")
  expect_equal(color_temperature(c(0.1, 0.2, 1)), "cool")
  expect_equal(color_temperature(c(0, 0, 0)), "neutral")
  expect_equal(color_temperature(c(NA, 0.2, 0.3)), "neutral")
})

test_that("color type detection covers common outfit colors", {
  expect_equal(detect_color_type(c(0.02, 0.02, 0.02)), "black")
  expect_equal(detect_color_type(c(0.95, 0.95, 0.95)), "white")
  expect_equal(detect_color_type(c(0.9, 0.1, 0.1)), "red")
  expect_equal(detect_color_type(c(0.1, 0.2, 0.8)), "navy")
})

test_that("mood adjustment covers UI mood choices", {
  expect_equal(mood_adjustment(50, "romantic", c(0.9, 0.1, 0.1), c(0.02, 0.02, 0.02)), 60)
  expect_equal(mood_adjustment(50, "minimal", c(0.02, 0.02, 0.02), c(0.95, 0.95, 0.95)), 58)
  expect_equal(mood_adjustment(50, "unknown mood", c(0.9, 0.1, 0.1), c(0.02, 0.02, 0.02)), 50)
})

test_that("undertone, neckline, and body shape scoring are clamped", {
  expect_equal(undertone_match(96, c(1, 0.3, 0.1), c(1, 0.2, 0.1), "warm"), 100)
  expect_equal(neckline_face_match(50, "v-neck", "oval"), 60)
  expect_equal(neckline_face_match(3, "halter", "oval"), 0)
  expect_equal(body_shape_match(60, "hourglass", "fitted", "skinny"), 80)
})

test_that("PCA color features returns a palette classification", {
  color_matrix <- matrix(c(
    0.8, 0.2, 0.1,
    0.1, 0.7, 0.3,
    0.5, 0.5, 0.5,
    0.9, 0.1, 0.2,
    0.2, 0.8, 0.4,
    0.6, 0.6, 0.6
  ), nrow = 6, byrow = TRUE)

  result <- pca_color_features(color_matrix)

  expect_true(result$optimal_k >= 2)
  expect_true(result$palette_style %in% c("minimal", "complementary", "triadic", "eclectic"))
  expect_equal(nrow(result$features), 6)
})
