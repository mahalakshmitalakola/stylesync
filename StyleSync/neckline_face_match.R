neckline_face_match <- function(score, neckline, face_shape){

  if(is.null(neckline) | is.null(face_shape)){
    return(score)
  }

  neckline <- tolower(neckline)
  face_shape <- tolower(face_shape)

  # Normalize neckline names
  if(neckline %in% c("round / scoop","round","scoop")){
    neckline <- "scoop"
  }

  score_adjust <- 0

  match_rules <- list(
    oval = c("v-neck","scoop","boat neck","square"),
    round = c("v-neck","square"),
    heart = c("scoop","boat neck","v-neck"),
    square = c("scoop"),
    pear = c("boat neck","halter")
  )

  if(face_shape %in% names(match_rules)){

    if(neckline %in% match_rules[[face_shape]]){
      score_adjust <- 10
    } else {
      score_adjust <- -5
    }

  }

  final_score <- score + score_adjust

  final_score <- max(min(final_score,100),0)

  return(final_score)
}
