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
  t <- paste0( #nolint
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