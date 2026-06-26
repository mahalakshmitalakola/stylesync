library(testthat)

this_file <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE))
  }

  ofile <- sys.frame(1)$ofile
  if (!is.null(ofile)) {
    return(normalizePath(ofile, winslash = "/", mustWork = FALSE))
  }

  normalizePath("tests/testthat.R", winslash = "/", mustWork = FALSE)
}

project_dir <- normalizePath(file.path(dirname(this_file()), ".."), winslash = "/", mustWork = TRUE)
source(file.path(project_dir, "launch_dashboard.R"))
load_fashion_modules(project_dir, quiet = TRUE)

test_dir(file.path(project_dir, "tests", "testthat"), reporter = "summary")
