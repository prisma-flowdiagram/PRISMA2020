# Utility functions for PRISMA_flowdiagram

#' Calculate the correct height of a box from a list (e.g. of exclusion reasons)
#' @description Get the correct height for a box
#' @param list a correctly wrapped (using stringr) list
#' @param offset the offset height (e.g. 3.5)
#' @param min the minimum number of rows before adjusting
#' @return the height of the box
#' @keywords internal
PRISMA_get_height_ <- function (list, offset, min = 3) { #nolint
  if (nrow(list) > min) {
    height <- offset - ((nrow(list) - (min + 1)) / 9)
  } else {
    height <- offset
  }
  return(height)
}

#' Generate / insert JS for labels
#' @description Generate the javascript method to insert the side labels
#' @param plot the plot object (without side labels)
#' @param identification_text the text to use as the "identification" label
#' @param screening_text the text to use as the "screening" label
#' @param included_text the text to use as the "identification" label
#' @return the plot object (with JS to generate side labels)
#' @keywords internal
PRISMA_insert_js_ <- function ( #nolint
  plot,
  identification_text,
  screening_text,
  included_text
) {
    # This JS loops through each node, and
    # locates the relevent <text> tag containing the label
    # The blank label is replaced with the relevent descriptive label
    # This is done in the loop as positioning due
    # to rotation otherwise being difficult
    # we rotate the text and adjust the position to ensure that
    # the now rotated text is displayed correctly
    javascript <- htmltools::HTML(paste0('
       const nodeMap = new Map([["node1","',
        identification_text,
      '"], ["node2","',
        screening_text,
      '"], ["node3","',
        included_text,
      '"]]);
       for (const [node, label] of nodeMap) {
         var theDiv = document.getElementById(node);
         var theText = theDiv.querySelector("text");
         var attrX = theText.getAttribute("x");
         var attrY = theText.getAttribute("y");
         theText.setAttribute("y",parseFloat(attrX))
         theText.setAttribute("x",parseFloat(attrY)*-1)
         theText.setAttribute("style","transform: rotate(-90deg);")
         theText.setAttribute("dominant-baseline", "middle")
         theText.innerHTML = label;
       }
    '))
    plot <- htmlwidgets::appendContent(
      plot, htmlwidgets::onStaticRenderComplete(javascript)
    )
    return(plot)
}

#' Add the hyperlink to the given node
#'
#' @description Generate the javascript method to insert the side labels
#' @param node the relevent node
#' @param url the URL the node should link to
#' @return An interactive flow diagram plot.
#' @keywords internal
PRISMA_add_hyperlink_ <- function( #nolint
  node,
  url
) {
  t <- paste0(
    "const ",
      node,
    ' = document.getElementById("',
      node,
    '");
    var link',
      node,
    ' = "<a href=\'',
      url,
    '\' target=\'_blank\'>" + ',
      node,
    '.innerHTML + "</a>";',
    "\n",
      node,
    ".innerHTML = link",
      node,
    ";"
  )
}

#' Plot interactive flow diagram for systematic reviews
#' @description Converts a PRISMA systematic review flow diagram into an
#' interactive HTML plot, for embedding links from each box.
#' @seealso [PRISMA_interactive_()]
#' @param plot A plot object from [PRISMA_flowdiagram()].
#' @param urls A dataframe consisting of two columns: nodes and urls. The first
#' column should contain 19 rows for the nodes from node1 to node19. The second
#' column should contain a corresponding URL for each node.
#' @param previous Logical argument (TRUE or FALSE) (supplied through
#' [PRISMA_flowdiagram()]) specifying whether previous studies were sought.
#' @param other Logical argument (TRUE or FALSE) (supplied through
#' [PRISMA_flowdiagram()]) specifying whether other studies were sought.
#' @return An interactive flow diagram plot.
#' @keywords internal
PRISMA_interactive_ <- function( #nolint
  plot,
  urls,
  previous,
  other
) {
  if (previous == TRUE && other == TRUE) {
    link <- data.frame(
      boxname = c(
        "identification",
        "screening",
        "included",
        "prevstud",
        "box1",
        "newstud",
        "box2",
        "box3",
        "box4",
        "box5",
        "box6",
        "box7",
        "box8",
        "box9",
        "box10",
        "othstud",
        "box11",
        "box12",
        "box13",
        "box14",
        "box15",
        "box16",
        "A",
        "B"
      ),
      node = paste0("node", seq(1, 24))
    )
    target <- c(
      "node1",
      "node2",
      "node3",
      "node4",
      "node5",
      "node23",
      "node6",
      "node7",
      "node8",
      "node9",
      "node10",
      "node11",
      "node12",
      "node13",
      "node14",
      "node15",
      "node22",
      "node16",
      "node17",
      "node18",
      "node19",
      "node20",
      "node21",
      "node24"
    )
  } else if (previous == FALSE && other == TRUE) {
    link <- data.frame(
      boxname = c(
        "identification",
        "screening",
        "included",
        "newstud",
        "box2",
        "box3",
        "box4",
        "box5",
        "box6",
        "box7",
        "box8",
        "box9",
        "box10",
        "othstud",
        "box11",
        "box12",
        "box13",
        "box14",
        "box15",
        "B"
      ),
      node = paste0("node", seq(1, 20))
    )
    target <- c(
      "node1",
      "node2",
      "node3",
      "node4",
      "node5",
      "node6",
      "node7",
      "node8",
      "node9",
      "node10",
      "node11",
      "node12",
      "node13",
      "node14",
      "node15",
      "node16",
      "node17",
      "node18",
      "node19",
      "node20"
    )
  }
  else if (previous == TRUE && other == FALSE) {
    link <- data.frame(
      boxname = c(
        "identification",
        "screening",
        "included",
        "prevstud",
        "box1",
        "newstud",
        "box2",
        "box3",
        "box4",
        "box5",
        "box6",
        "box7",
        "box8",
        "box9",
        "box10",
        "box16",
        "A"
      ),
      node = paste0("node", seq(1, 17))
    )
    target <- c(
      "node1",
      "node2",
      "node3",
      "node4",
      "node5",
      "node6",
      "node7",
      "node8",
      "node9",
      "node10",
      "node11",
      "node12",
      "node13",
      "node14",
      "node15",
      "node16",
      "node17"
    )
  }
  else {
    link <- data.frame(
      boxname = c(
        "identification",
        "screening",
        "included",
        "newstud",
        "box2",
        "box3",
        "box4",
        "box5",
        "box6",
        "box7",
        "box8",
        "box9",
        "box10"
      ),
      node = paste0("node", seq(1, 13))
    )
    target <- c(
      "node1",
      "node2",
      "node3",
      "node4",
      "node5",
      "node6",
      "node7",
      "node8",
      "node9",
      "node10",
      "node11",
      "node12",
      "node13"
    )
  }
  link <- merge(link, urls, by.x = "boxname", by.y = "box", all.x = TRUE)
  link <- link[match(target, link$node), ]
  node <- link$node
  url <- link$url
  #the following code adds the location link for the new window
  javascript <- htmltools::HTML(
    paste(
      mapply(
        PRISMA_add_hyperlink_,
        node,
        url
      ),
      collapse = "\n"
    )
  )
  htmlwidgets::prependContent(
    plot,
    htmlwidgets::onStaticRenderComplete(javascript)
  )
}

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
    if (!is.na(ft) & !is.null(ft)) {
      the_ft <- toupper(ft)
    } else {
      the_ft <- toupper(tools::file_ext(fn))
      if (the_ft == "HTM") {
        the_ft <- "HTML"
      }
    }
    return(the_ft)
}

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
    return(tmpfilesvg)
}