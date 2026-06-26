detect_face_shape <- function(img_path) {

  library(opencv)
  library(imager)

  # ── Load & detect face ──────────────────────────────────────────────────────
  img   <- ocv_read(img_path)
  faces <- ocv_face(img)

  if (length(faces) == 0) {
    return(list(
      face_shape = "unknown",
      forehead   = NA,
      cheek      = NA,
      jaw        = NA,
      face_length = NA
    ))
  }

  face <- faces[[1]]
  x <- face$x
  y <- face$y
  w <- face$width
  h <- face$height

  # ── Crop face region using ocv_crop (correct API) ──────────────────────────
  # Old code used img[y:(y+h), x:(x+w)] which is wrong for ocv objects.
  face_ocv  <- ocv_crop(img, x = x, y = y, width = w, height = h)

  # ── Convert to imager for edge analysis ────────────────────────────────────
  face_cimg <- as.cimg(face_ocv)
  gray      <- grayscale(face_cimg)

  # Resize to a standard size so width/height ratios are image-independent
  gray <- resize(gray, 200, 250)

  std_w <- width(gray)
  std_h <- height(gray)

  # ── Edge detection ──────────────────────────────────────────────────────────
  # Use a higher-than-mean threshold to reduce noise from skin texture and
  # uneven lighting — a known weakness of the original implementation.
  grad     <- imgradient(gray, "xy")
  edge_mag <- sqrt(grad$dx ^ 2 + grad$dy ^ 2)

  threshold <- quantile(as.vector(edge_mag), 0.80)   # top 20% edges only
  binary    <- as.matrix(edge_mag > threshold)

  coords <- which(binary, arr.ind = TRUE)

  if (nrow(coords) < 80) {
    return(list(
      face_shape  = "unknown",
      forehead    = NA,
      cheek       = NA,
      jaw         = NA,
      face_length = std_h
    ))
  }

  y_vals <- coords[, 1]

  # ── Define five vertical regions ────────────────────────────────────────────
  # Using five bands gives us forehead, upper-cheek, mid-cheek, lower-cheek
  # and jaw — finer than the original three, which missed diamond shape.
  forehead_pts  <- coords[y_vals < quantile(y_vals, 0.20), ]
  upper_chk_pts <- coords[y_vals >= quantile(y_vals, 0.25) &
                           y_vals <  quantile(y_vals, 0.45), ]
  mid_chk_pts   <- coords[y_vals >= quantile(y_vals, 0.40) &
                           y_vals <  quantile(y_vals, 0.60), ]
  lower_chk_pts <- coords[y_vals >= quantile(y_vals, 0.55) &
                           y_vals <  quantile(y_vals, 0.72), ]
  jaw_pts       <- coords[y_vals >= quantile(y_vals, 0.78), ]

  # Helper: width of a region (0 if region is empty)
  region_width <- function(pts) {
    if (nrow(pts) < 5) return(0)
    max(pts[, 2]) - min(pts[, 2])
  }

  forehead_w   <- region_width(forehead_pts)
  upper_chk_w  <- region_width(upper_chk_pts)
  cheek_w      <- region_width(mid_chk_pts)       # widest mid-face
  lower_chk_w  <- region_width(lower_chk_pts)
  jaw_w        <- region_width(jaw_pts)
  face_length  <- std_h

  # ── Classification ──────────────────────────────────────────────────────────
  shape <- "unknown"

  # Oval: face notably longer than wide, cheeks are the widest point
  if (face_length > cheek_w * 1.3 &&
      cheek_w >= forehead_w &&
      cheek_w >= jaw_w) {
    shape <- "oval"

  # Round: face length ≈ width, soft wide cheeks
  } else if (abs(face_length - cheek_w) < cheek_w * 0.18 &&
             cheek_w > forehead_w &&
             cheek_w > jaw_w) {
    shape <- "round"

  # Square: forehead ≈ jaw width, face not much longer than wide
  } else if (abs(forehead_w - jaw_w) < cheek_w * 0.12 &&
             face_length < cheek_w * 1.25) {
    shape <- "square"

  # Heart: wide forehead, narrow jaw
  } else if (forehead_w > cheek_w * 0.95 &&
             forehead_w > jaw_w * 1.20) {
    shape <- "heart"

  # Diamond: narrow forehead AND jaw, wide cheekbones
  # (original code had no diamond rule — relied on the else branch)
  } else if (cheek_w > forehead_w * 1.10 &&
             cheek_w > jaw_w * 1.10 &&
             abs(forehead_w - jaw_w) < cheek_w * 0.25) {
    shape <- "diamond"

  # Pear (oblong): jaw wider than forehead and cheeks
  } else if (jaw_w > cheek_w && jaw_w > forehead_w) {
    shape <- "pear"

  # Fallback: if nothing matched clearly, call it oval (most common)
  } else {
    shape <- "oval"
  }

  return(list(
    face_shape  = shape,
    forehead    = forehead_w,
    cheek       = cheek_w,
    jaw         = jaw_w,
    face_length = face_length
  ))
}
