# FashionMatchAI - Dashboard launcher
# Sources all function modules, then launches the Shiny dashboard.
# Run this file to start the app: source("launch_dashboard.R")

.fashion_project_dir <- function() {
  frames <- sys.frames()
  ofiles <- vapply(frames, function(frame) {
    if (!is.null(frame$ofile)) as.character(frame$ofile) else NA_character_
  }, character(1))
  ofiles <- ofiles[!is.na(ofiles) & nzchar(ofiles)]

  if (length(ofiles) > 0) {
    return(dirname(normalizePath(ofiles[length(ofiles)], winslash = "/", mustWork = FALSE)))
  }

  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(
      sub("^--file=", "", file_arg[1]),
      winslash = "/",
      mustWork = FALSE
    )))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

.fashion_default_base_dir <- .fashion_project_dir()

.fashion_app_dir <- function(base_dir) {
  root_has_app <- all(file.exists(file.path(base_dir, c("ui.R", "server.R"))))
  if (root_has_app) {
    return(base_dir)
  }

  shiny_dir <- file.path(base_dir, "shiny")
  shiny_has_app <- all(file.exists(file.path(shiny_dir, c("ui.R", "server.R"))))
  if (shiny_has_app) {
    return(shiny_dir)
  }

  stop(
    "Shiny app files not found. Expected ui.R and server.R in either:\n  ",
    base_dir, "\n  ", shiny_dir,
    call. = FALSE
  )
}

#' Source all FashionMatchAI modules
#'
#' Call this once at the top of any script or before running the dashboard.
#' Lists every module explicitly so missing files are caught early with a clear
#' error message rather than a cryptic "function not found" at runtime.
#'
#' @param base_dir Directory containing all .R module files. Defaults to the
#'   directory of this script so it works regardless of working directory.
#' @param quiet If TRUE, suppresses the success message.
#' @export
load_fashion_modules <- function(base_dir = NULL, quiet = FALSE) {

  if (is.null(base_dir)) {
    base_dir <- .fashion_default_base_dir
  }

  modules <- c(
    # Image processing - load before anything that calls them
    "extract_color.R",
    "extract_skin_tone.R",
    "detect_color_type.R",
    "color_temperature.R",
    "color_harmony.R",
    "pca_color_features.R",
    "cluster_style.R",

    # Shape detection
    "detect_top_shape.R",
    "detect_bottom_shape.R",
    "detect_face_shape.R",
    "detect_neckline.R",

    # Scoring adjustments
    "undertone_match.R",
    "mood_adjustment.R",
    "neckline_face_match.R",
    "skin_match.R",
    "body_shape_match.R",

    # Main pipeline - load last because it depends on all of the above
    "predict_outfit.R"
  )

  paths <- file.path(base_dir, modules)
  missing <- modules[!file.exists(paths)]

  if (length(missing) > 0) {
    stop(
      "The following modules were not found:\n  ",
      paste(missing, collapse = "\n  "),
      call. = FALSE
    )
  }

  for (path in paths) {
    source(path, local = FALSE)
  }

  if (!quiet) {
    message("All FashionMatchAI modules loaded successfully.")
  }

  invisible(list(base_dir = base_dir, modules = modules))
}

#' Launch the Fashion Match AI Shiny dashboard
#'
#' @param dev_mode If TRUE, sources modules from the current project directory.
#' @param launch_browser Passed to shiny::runApp().
#' @export
launch_dashboard <- function(dev_mode = TRUE, launch_browser = interactive()) {

  if (dev_mode) {
    base_dir <- .fashion_default_base_dir
  } else {
    base_dir <- system.file(package = "FashionMatchAI")
    if (!nzchar(base_dir)) {
      stop("Installed package directory for FashionMatchAI was not found.", call. = FALSE)
    }
  }

  load_fashion_modules(base_dir, quiet = TRUE)
  app_dir <- .fashion_app_dir(base_dir)

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to launch the dashboard.", call. = FALSE)
  }

  message("Launching FashionMatchAI dashboard from: ", app_dir)
  shiny::runApp(app_dir, launch.browser = launch_browser)
}

.fashion_running_via_rscript <- function() {
  calls <- vapply(sys.calls(), function(call) {
    paste(deparse(call), collapse = " ")
  }, character(1))

  source_call <- grepl("source\\(", calls) & grepl("launch_dashboard\\.R", calls)
  if (any(source_call)) {
    return(FALSE)
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0) {
    return(FALSE)
  }

  rscript_file <- normalizePath(
    sub("^--file=", "", file_arg[1]),
    winslash = "/",
    mustWork = FALSE
  )
  this_file <- normalizePath(
    file.path(.fashion_default_base_dir, "launch_dashboard.R"),
    winslash = "/",
    mustWork = FALSE
  )

  identical(rscript_file, this_file)
}

if (!interactive() && .fashion_running_via_rscript()) {
  launch_dashboard(dev_mode = TRUE, launch_browser = TRUE)
}
