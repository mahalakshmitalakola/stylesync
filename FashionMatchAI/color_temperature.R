color_temperature <- function(rgb) {

  if (length(rgb) != 3 || any(is.na(rgb))) {
    return("neutral")
  }

  R <- rgb[1]
  B <- rgb[3]

  total <- sum(rgb)
  if (total <= 0) {
    return("neutral")
  }

  r_ratio <- R / total
  b_ratio <- B / total

  if (r_ratio - b_ratio > 0.1) {
    return("warm")
  }

  if (b_ratio - r_ratio > 0.1) {
    return("cool")
  }

  "neutral"
}
