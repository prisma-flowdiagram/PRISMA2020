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