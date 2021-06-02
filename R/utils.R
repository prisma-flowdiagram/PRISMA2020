# Utility functions for PRISMA_flowdiagram

#' Generate / insert JS for labels
#'
#' @description Generate the javascript method to insert the side labels
#' @param plot the plot object (without side labels)
#' @param identification_text the text to use as the "identification" label
#' @param screening_text the text to use as the "screening" label
#' @param included_text the text to use as the "identification" label
#' @return the plot object (with JS to generate side labels)
#' @NoRd
insertJS_ <- function (plot, identification_text, screening_text, included_text) {
    # This JS loops through each node, and
    # locates the relevent <text> tag containing the label
    # The blank label is replaced with the relevent descriptive label
    # This is done in the loop as positioning due
    # to rotation otherwise being difficult
    # we rotate the text and adjust the position to ensure that
    # the now rotated text is displayed correctly
    javascript <- htmltools::HTML(paste0('
       const nodeMap = new Map([["node1","',identification_text,'"], ["node2","',screening_text,'"], ["node3","',included_text,'"]]);
       for (const [node, label] of nodeMap) {
         var theDiv = document.getElementById(node);
         var theText = theDiv.querySelector("text");
         var attrX = theText.getAttribute("x");
         var attrY = theText.getAttribute("y");
         theText.setAttribute("y",parseFloat(attrX)+2)
         theText.setAttribute("x",parseFloat(attrY)*-1)
         theText.setAttribute("style","transform: rotate(-90deg);")
         theText.innerHTML = label;
       }
    '))
    plot <- htmlwidgets::appendContent(plot, htmlwidgets::onStaticRenderComplete(javascript))
    return(plot)
}

#' Calculate the correct filetime
#'
#' @description Work out the correct filetype to save the file as
#' @param fn The filename (including extension)
#' @param ft The filetype (which can be NA or NULL)
#' @return the filetype taken from the filename, or overriden by the ft param
#' @NoRd
calc_filetype_ <- function(fn, ft) {
    # if the filetype is set, return that, otherwise
    # calculate the filetype from the extension (HTM becomes HTML)
    if(!is.na(ft) & !is.null(ft)){
      the_ft <- toupper(ft)
    } else {
      the_ft <- toupper(tools::file_ext(fn))
      if (the_ft == 'HTM') {
        the_ft <- 'HTML'
      }
    }
    return(the_ft)
}

#' Generate a temporary SVG from a plot object
#'
#' @description Generate and save a temporary SVG from a plot object
#' @param obj the plot object
#' @return the full path to the saved SVG
#' @NoRd
gen_tmp_svg_ <- function(obj) {
    # generate temporary filenames
    tmpfilehtml <- tempfile(pattern = "PRISMA2020_", tmpdir = tempdir(), fileext = ".html" )
    tmpfilesvg <- tempfile(pattern = "PRISMA2020_", tmpdir = tempdir(), fileext = ".svg" )
    # save the widget as HTML and read it into a variable
    htmlwidgets::saveWidget(obj, file=tmpfilehtml)
    htmldata <- xml2::read_html(tmpfilehtml)
    # extract our labelling javascript using xml_find_first and xpath
    # it finds the first script element follwing the grViz class - this looks to be quite fragile if we change our injected JS
    js <- xml2::xml_text(xml2::xml_find_first(htmldata,'//div[contains(@class, "grViz")]//following-sibling::script'))
    # use DiagrammeRsvg to export an SVG from the htmlwidgets code - this uses the V8 engine in the background so takes a little bit of time to run
    # then read the SVG's XML into a variable
    svg <- DiagrammeRsvg::export_svg(obj)
    svg <- xml2::read_xml(svg)
    # we need to extract the node names and the label values from our JS, so find the appropriate part of the code (again, sensitive to script changes)
    # we then extract the node names and labels and insert them into the SVG, in a similar manner to the original JS code
    jsnode <- stringr::str_split(
      stringr::str_remove_all(
        stringr::str_match(
          js, "const nodeMap = new Map\\(\\[(.*)\\]\\);"
        )[1,2],
        "\\[|\"|]"
      ),
      ",\\s",
      simplify = TRUE
    )
    len <- length(jsnode)
    for (i in 1:len) {
      matsp <- stringr::str_split_fixed(jsnode[i],",",2)
      namespace <- xml2::xml_ns(svg)
      xml_text_node <- xml2::xml_find_first(svg, paste0('//d1:g[@id="',matsp[,1],'"]//d1:text'), namespace)
      attrX <- xml2::xml_attr(xml_text_node, "x")
      attrY <- xml2::xml_attr(xml_text_node, "y")
      xml2::xml_attr(xml_text_node, "x") <- as.double(attrY)*-1
      xml2::xml_attr(xml_text_node, "y") <- as.double(attrX)+2
      # libRSVG does not support css transforms, so we need to use the native SVG transform attribute
      xml2::xml_attr(xml_text_node, "transform") <- "rotate(-90)"
      xml2::xml_text(xml_text_node) <- matsp[,2]
    }
    xml2::write_xml(svg, file = tmpfilesvg)
    return(tmpfilesvg)
}