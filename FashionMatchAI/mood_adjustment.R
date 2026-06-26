mood_adjustment <- function(score, mood, top_color, bottom_color) {

  mood <- tolower(trimws(mood))

  top_type <- detect_color_type(top_color)
  bottom_type <- detect_color_type(bottom_color)
  outfit_colors <- c(top_type, bottom_type)

  score_palette <- function(strong = character(),
                            good = character(),
                            ok = character(),
                            fallback = 0) {
    if (any(outfit_colors %in% strong)) {
      return(10)
    }
    if (any(outfit_colors %in% good)) {
      return(8)
    }
    if (any(outfit_colors %in% ok)) {
      return(6)
    }
    fallback
  }

  bonus <- switch(
    mood,

    festive = score_palette(
      strong = c("red", "yellow", "orange", "purple", "pink"),
      good = c("green", "blue"),
      ok = c("black", "white", "grey", "brown", "neutral", "beige")
    ),

    party = score_palette(
      strong = c("black", "red", "purple", "pink"),
      good = c("blue", "green", "yellow", "orange"),
      ok = c("white", "grey", "brown", "neutral", "beige")
    ),

    professional = {
      formal_colors <- c("black", "navy", "grey", "white", "beige")
      if (all(outfit_colors %in% formal_colors)) {
        8
      } else {
        score_palette(
          strong = c("blue", "navy", "brown"),
          good = c("neutral", "green", "beige"),
          ok = c("red", "orange", "yellow", "pink", "purple"),
          fallback = 4
        )
      }
    },

    casual = score_palette(
      strong = c("blue", "green", "white", "denim"),
      good = c("yellow", "orange", "pink", "red", "purple"),
      ok = c("black", "grey", "brown", "neutral", "beige")
    ),

    date = score_palette(
      strong = c("red", "pink", "purple"),
      good = c("black", "white", "blue", "green"),
      ok = c("yellow", "orange", "brown", "grey", "neutral", "beige")
    ),

    romantic = score_palette(
      strong = c("red", "pink", "purple"),
      good = c("white", "beige", "blue"),
      ok = c("black", "grey", "green", "neutral")
    ),

    sporty = score_palette(
      strong = c("blue", "green", "white"),
      good = c("red", "orange", "yellow"),
      ok = c("black", "grey", "pink", "purple", "brown", "neutral")
    ),

    vacation = score_palette(
      strong = c("yellow", "orange", "pink"),
      good = c("green", "blue", "white", "neutral", "beige"),
      ok = c("black", "brown", "grey", "red", "purple")
    ),

    evening = score_palette(
      strong = c("black", "navy", "purple", "red"),
      good = c("pink", "grey", "blue", "brown"),
      ok = c("white", "yellow", "orange", "green", "neutral", "beige")
    ),

    bold = score_palette(
      strong = c("red", "orange", "yellow", "purple", "pink"),
      good = c("black", "blue", "green"),
      ok = c("white", "grey", "navy")
    ),

    minimal = {
      minimal_colors <- c("black", "white", "grey", "beige", "neutral", "navy")
      if (all(outfit_colors %in% minimal_colors)) 8 else 4
    },

    elegant = score_palette(
      strong = c("black", "navy", "purple", "white"),
      good = c("red", "pink", "grey", "beige"),
      ok = c("blue", "brown", "neutral")
    ),

    playful = score_palette(
      strong = c("yellow", "orange", "pink", "green"),
      good = c("blue", "red", "purple"),
      ok = c("white", "beige", "neutral")
    ),

    edgy = score_palette(
      strong = c("black", "red", "purple"),
      good = c("navy", "grey", "blue"),
      ok = c("white", "green", "pink")
    ),

    classic = {
      classic_colors <- c("black", "navy", "grey", "white", "blue", "brown", "beige")
      if (all(outfit_colors %in% classic_colors)) 8 else 5
    },

    0
  )

  max(min(score + bonus, 100), 0)
}
