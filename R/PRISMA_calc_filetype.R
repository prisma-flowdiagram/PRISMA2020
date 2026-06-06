#' Calculate the correct filetime
#'
#' @description Work out the correct filetype to save the file as
#' @param fn The filename (including extension)
#' @param ft The filetype (which can be NA or NULL)
#' @return the filetype taken from the filename, or overriden by the ft param
#' @keywords internal
PRISMA_calc_filetype_ <- function(fn, ft) { #nolint
  # if the filetype is set, return that, otherwise
  # calculate the filetype from the extension (HTM becomes HTML)
  if (!is.na(ft) && !is.null(ft)) {
    the_ft <- toupper(ft)
  } else {
    the_ft <- toupper(tools::file_ext(fn))
    if (the_ft == "HTM") {
      the_ft <- "HTML"
    }
  }
  the_ft
}
