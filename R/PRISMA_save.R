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
#' PRISMA_save(plot, filename = tempfile(), filetype="html");
#' @export
PRISMA_save <- function( #nolint
  plotobj,
  filename = "PRISMA2020_flowdiagram.html",
  filetype = NA,
  overwrite = FALSE,
  width = NULL,
  height = NULL,
  css = NULL
) {
  if (!file.exists(filename) || overwrite == TRUE) {
    format_real <- PRISMA_calc_filetype_(filename, filetype) #nolint
    switch(
      format_real,
      "HTML" = {
        tmp_html <- tempfile(
          pattern = "PRISMA2020_",
          tmpdir = tempdir(),
          fileext = ".html"
        )
        htmlwidgets::saveWidget(
          plotobj,
          file = tmp_html,
          title = "PRISMA2020 Flowdiagram"
        )
        if (!(file.copy(tmp_html, filename, overwrite = TRUE))) {
          stop("Error saving HTML")
        }
        file.remove(tmp_html)
      },
      "ZIP" = {
        curr_wd <- getwd()
        tmp_dir <- tempdir()
        setwd(tmp_dir)
        tmp_zipfile <- tempfile(
          pattern = "PRISMA2020_",
          tmpdir = tempdir(),
          fileext = ".zip"
        )
        tmp_html <- paste0(
          tools::file_path_sans_ext(basename(filename)),
          ".html"
        )
        tmp_libdir <- paste0(tmp_html, "_files")
        htmlwidgets::saveWidget(
          plotobj,
          file = tmp_html,
          libdir = tmp_libdir,
          selfcontained = FALSE,
          title = "PRISMA2020 Flowdiagram"
        )
        zip::zip(zipfile = tmp_zipfile, files = c(tmp_html, tmp_libdir))
        setwd(curr_wd)
        if (!(file.copy(paste0(tmp_zipfile), filename, overwrite = TRUE))) {
          stop("Error saving ZIP File")
        }
      },
      "PDF" = {
        tmp_svg <- PRISMA_gen_tmp_svg_(plotobj) #nolint
        rsvg::rsvg_pdf(
          tmp_svg,
          filename,
          width = width,
          height = height,
          css = css
        )
        file.remove(tmp_svg)
      },
      "PNG" = {
        tmp_svg <- PRISMA_gen_tmp_svg_(plotobj) #nolint
        rsvg::rsvg_png(
          tmp_svg,
          filename,
          width = width,
          height = height,
          css = css
        )
        file.remove(tmp_svg)
      },
      "SVG" = {
        tmp_svg <- PRISMA_gen_tmp_svg_(plotobj) #nolint
        if (!(file.copy(tmp_svg, filename, overwrite = TRUE))) {
          stop("Error saving SVG")
        }
        file.remove(tmp_svg)
      },
      "PS" = {
        tmp_svg <- PRISMA_gen_tmp_svg_(plotobj) #nolint
        rsvg::rsvg_ps(
          tmp_svg,
          filename,
          width = width,
          height = height,
          css = css
        )
        file.remove(tmp_svg)
      },
      "WEBP" = {
        tmp_svg <- PRISMA_gen_tmp_svg_(plotobj) #nolint
        rsvg::rsvg_webp(
          tmp_svg,
          filename,
          width = width,
          height = height,
          css = css
        )
        file.remove(tmp_svg)
      },
      stop("Please choose one of the supported file types")
    )
    return(tools::file_path_as_absolute(filename))
  } else {
    stop("File exists, please set overwite = TRUE to overwrite")
  }
}
