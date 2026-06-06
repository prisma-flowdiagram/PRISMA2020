#' Generate a temporary SVG from a plot object
#'
#' @description Generate and save a temporary SVG from a plot object
#' @param obj the plot object
#' @return the full path to the saved SVG
#' @keywords internal
PRISMA_gen_tmp_svg_ <- function(obj) { #nolint
  # generate temporary filenames
  tmpfilehtml <- tempfile(
    pattern = "PRISMA2020_",
    tmpdir = tempdir(),
    fileext = ".html"
  )
  tmpfilesvg <- tempfile(
    pattern = "PRISMA2020_",
    tmpdir = tempdir(),
    fileext = ".svg"
  )
  # save the widget as HTML and read it into a variable
  htmlwidgets::saveWidget(obj, file = tmpfilehtml)
  htmldata <- xml2::read_html(tmpfilehtml)
  # extract our labelling javascript using xml_find_first and xpath
  # it finds the first script element follwing the grViz class
  # this looks to be quite fragile if we change our injected JS
  js <- xml2::xml_text(
    xml2::xml_find_first(
      htmldata,
      '//div[contains(@class, "grViz")]//following-sibling::script'
    )
  )
  # use DiagrammeRsvg to export an SVG from the htmlwidgets code
  # this uses the V8 engine in the background so takes time
  # then read the SVG's XML into a variable
  svg <- DiagrammeRsvg::export_svg(obj)
  svg <- xml2::read_xml(svg)
  # we need to extract the node names and the label values from our JS
  # so find the appropriate part of the code
  # (again, sensitive to script changes)
  # we then extract the node names and labels and insert them into the SVG
  # in a similar manner to the original JS code
  jsnode <- stringr::str_split(
    stringr::str_remove_all(
      stringr::str_match(
        js, "const nodeMap = new Map\\(\\[(.*)\\]\\);"
      )[1, 2],
      "\\[|\"|]"
    ),
    ",\\s",
    simplify = TRUE
  )
  len <- length(jsnode)
  for (i in 1:len) {
    matsp <- stringr::str_split_fixed(jsnode[i], ",", 2)
    namespace <- xml2::xml_ns(svg)
    xml_text_node <- xml2::xml_find_first(
      svg,
      paste0('//d1:g[@id="', matsp[, 1], '"]//d1:text'),
      namespace
    )
    attr_x <- xml2::xml_attr(xml_text_node, "x")
    attr_y <- xml2::xml_attr(xml_text_node, "y")
    xml2::xml_attr(xml_text_node, "x") <- as.double(attr_y) * -1
    xml2::xml_attr(xml_text_node, "y") <- as.double(attr_x) + 2
    # libRSVG does not support css transforms
    # so we need to use the native SVG transform attribute
    xml2::xml_attr(xml_text_node, "transform") <- "rotate(-90)"
    xml2::xml_text(xml_text_node) <- matsp[, 2]
  }
  xml2::write_xml(svg, file = tmpfilesvg)
  tmpfilesvg
}