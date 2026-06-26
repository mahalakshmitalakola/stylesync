body_shape_match <- function(score,
                             body_shape,
                             top_shape,
                             bottom_shape,
                             max_adjust = 20) {

  # max_adjust: configurable bonus cap (default 20).
  # Keeping it configurable means the Shiny UI can expose it as a slider
  # without touching this function.

  body_shape <- tolower(trimws(body_shape))

  # ── Partial unknown: adjust only the known half ─────────────────────────────
  # Original returned immediately if either shape was unknown.
  # Now we still score the known shape — a partial signal is better than none.
  top_known    <- !is.null(top_shape)    && top_shape    != "unknown"
  bottom_known <- !is.null(bottom_shape) && bottom_shape != "unknown"

  if (!top_known && !bottom_known) {
    return(score)
  }

  score_adjust <- 0

  # ── Lookup tables ────────────────────────────────────────────────────────────
  # Each body shape defines:
  #   $top_good    — tops that flatter: full bonus
  #   $top_ok      — tops that are fine: half bonus
  #   $bottom_good — bottoms that flatter: full bonus
  #   $bottom_ok   — bottoms that are fine: half bonus
  #   $top_bad     — tops that clash:  penalty
  #   $bottom_bad  — bottoms that clash: penalty
  #
  # Expanded vocab to match outputs from detect_top_shape() and
  # detect_bottom_shape() — e.g. "bootcut", "wide_leg", "peplum", "flowy".

  rules <- list(

    hourglass = list(
      top_good    = c("fitted", "wrap", "peplum", "tapered"),
      top_ok      = c("straight", "structured"),
      top_bad     = c("flowy", "wide"),
      bottom_good = c("skinny", "straight", "high_waist", "pencil_skirt"),
      bottom_ok   = c("bootcut", "tapered"),
      bottom_bad  = c("wide_leg", "flared")
    ),

    pear = list(
      top_good    = c("structured", "peplum", "flowy", "wide"),
      top_ok      = c("straight", "fitted"),
      top_bad     = c("tapered"),
      bottom_good = c("straight", "wide_leg", "flared", "bootcut"),
      bottom_ok   = c("tapered"),
      bottom_bad  = c("skinny")
    ),

    apple = list(
      top_good    = c("flowy", "straight", "peplum"),
      top_ok      = c("wide", "structured"),
      top_bad     = c("fitted", "tapered"),
      bottom_good = c("straight", "bootcut", "tapered", "wide_leg"),
      bottom_ok   = c("flared"),
      bottom_bad  = c("skinny")
    ),

    rectangle = list(
      top_good    = c("peplum", "flowy", "wide", "structured"),
      top_ok      = c("fitted", "straight"),
      top_bad     = c("tapered"),
      bottom_good = c("flared", "wide_leg", "bootcut"),
      bottom_ok   = c("straight", "tapered"),
      bottom_bad  = c("skinny")
    ),

    inverted_triangle = list(
      top_good    = c("straight", "flowy", "fitted"),
      top_ok      = c("tapered"),
      top_bad     = c("structured", "wide", "peplum"),
      bottom_good = c("wide_leg", "flared", "bootcut"),
      bottom_ok   = c("straight"),
      bottom_bad  = c("skinny", "tapered")
    )
  )

  # ── Unknown body shape — return unchanged ────────────────────────────────────
  if (!(body_shape %in% names(rules))) {
    return(score)
  }

  r <- rules[[body_shape]]

  # ── Top shape scoring ────────────────────────────────────────────────────────
  if (top_known) {
    if (top_shape %in% r$top_good) {
      score_adjust <- score_adjust + 10
    } else if (top_shape %in% r$top_ok) {
      score_adjust <- score_adjust + 5
    } else if (top_shape %in% r$top_bad) {
      score_adjust <- score_adjust - 5   # penalty for unflattering combination
    }
  }

  # ── Bottom shape scoring ─────────────────────────────────────────────────────
  if (bottom_known) {
    if (bottom_shape %in% r$bottom_good) {
      score_adjust <- score_adjust + 10
    } else if (bottom_shape %in% r$bottom_ok) {
      score_adjust <- score_adjust + 5
    } else if (bottom_shape %in% r$bottom_bad) {
      score_adjust <- score_adjust - 5
    }
  }

  # ── Apply configurable cap ───────────────────────────────────────────────────
  # Cap bonus (not penalty) — penalties are intentionally uncapped up to -10
  score_adjust <- min(score_adjust, max_adjust)

  final_score <- score + score_adjust
  final_score <- max(min(final_score, 100), 0)

  return(final_score)
}
