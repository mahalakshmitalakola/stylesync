detect_color_type <- function(rgb){

  R <- rgb[1]
  G <- rgb[2]
  B <- rgb[3]

  # Black
  if(R < 0.15 & G < 0.15 & B < 0.15){
    return("black")
  }

  # White
  if(R > 0.9 & G > 0.9 & B > 0.9){
    return("white")
  }

  # Grey
  if(abs(R-G) < 0.08 & abs(R-B) < 0.08 & abs(G-B) < 0.08){
    return("grey")
  }

  # Beige / Cream
  if(R > 0.75 & G > 0.70 & B > 0.55){
    return("beige")
  }

  # Brown
  if(R > 0.4 & R < 0.7 & G < 0.4 & B < 0.3){
    return("brown")
  }

  # Red
  if(R > 0.65 & G < 0.4 & B < 0.4){
    return("red")
  }

  # Orange
  if(R > 0.7 & G > 0.4 & G < 0.7 & B < 0.3){
    return("orange")
  }

  # Yellow
  if(R > 0.7 & G > 0.7 & B < 0.4){
    return("yellow")
  }

  # Olive
  if(R > 0.4 & G > 0.5 & B < 0.3){
    return("olive")
  }

  # Green
  if(G > R & G > B){
    return("green")
  }

  # Teal
  if(G > 0.5 & B > 0.5 & R < 0.4){
    return("teal")
  }

  # Navy
  if(B > 0.4 & R < 0.3 & G < 0.4){
    return("navy")
  }

  # Blue
  if(B > R & B > G){
    return("blue")
  }

  # Denim
  if(B > 0.45 & G > 0.3 & R < 0.35){
    return("denim")
  }

  # Pink
  if(R > 0.7 & B > 0.5 & G < 0.6){
    return("pink")
  }

  # Purple
  if(R > 0.5 & B > 0.5 & G < 0.5){
    return("purple")
  }

  return("neutral")
}
