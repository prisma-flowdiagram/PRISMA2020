#' Save PRISMA2020 flow diagram
#' @description Save the output from [PRISMA_flowdiagram()] to the
#' working directory.
#' @param plotobj A plot produced using [PRISMA_flowdiagram()].
#' @param filename The filename to save (including extension)
#' @param filetype The filetype to save the plot in, supports:
#' HTML, ZIP, PDF, PNG, SVG, PS and WEBP
#' (if NA, the filetype will be calculated out based on the file extension)
#' HTML files maintain hyperlinks and tooltips
#' The ZIP option creates an archive containing the HTML file,
#' alongside supporting javascript and css files in an adjacent folder,
#' instead of embedded base64 within the HTML file
#' @param overwrite if TRUE, will overwrite an existing file
#' @param width passed as the width argument to
#' [rsvg::rsvg()] and similar functions
#' @param height passed as the height argument to
#' [rsvg::rsvg()] and similar functions
#' @param css passed as the css argument to
#' [rsvg::rsvg()] and similar functions
#' @return the absolute filename of the saved diagram plot.
#' @examples
#' csvFile <- system.file("extdata", "PRISMA.csv", package = "PRISMA2020")
#' data <- read.csv(csvFile);
#' data <- PRISMA_data(data);
#' plot <- PRISMA_flowdiagram(data,
#'                 fontsize = 12,
#'                 interactive = TRUE,
#'                 previous = FALSE,
#'                 other = TRUE);
#' PRISMA_checklist(plot, filename = tempfile(), filetype="html");
#' @export
PRISMA_checklist <- function()
  }
}
