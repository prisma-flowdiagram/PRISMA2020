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
  } else if (previous == TRUE && other == FALSE) {
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
  } else {
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