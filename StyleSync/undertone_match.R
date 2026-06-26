undertone_match <- function(score, top_color, bottom_color, undertone){

  top_temp <- color_temperature(top_color)
  bottom_temp <- color_temperature(bottom_color)

  if(undertone == "warm"){
    if(top_temp == "warm") score <- score + 5
    if(bottom_temp == "warm") score <- score + 5
  }

  else if(undertone == "cool"){
    if(top_temp == "cool") score <- score + 5
    if(bottom_temp == "cool") score <- score + 5
  }

  else if(undertone == "neutral"){

    if(top_temp == "neutral") score <- score + 3
    if(bottom_temp == "neutral") score <- score + 3

    score <- score + 2
  }

  return(min(score,100))
}
