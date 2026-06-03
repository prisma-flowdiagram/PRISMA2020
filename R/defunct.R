#' Defunct function - replaced by "PRISMA_interactive_"
#' @description Defunct function - replaced by "PRISMA_interactive_"
#' @seealso [PRISMA_interactive_()]
#' @param plot A plot object from [PRISMA_flowdiagram()].
#' @param urls A dataframe consisting of two columns: nodes and urls. The first
#' column should contain 19 rows for the nodes from node1 to node19. The second
#' column should contain a corresponding URL for each node.
#' @param previous Logical argument (TRUE or FALSE) (supplied through
#' [PRISMA_flowdiagram()]) specifying whether previous studies were sought.
#' @param other Logical argument (TRUE or FALSE) (supplied through
#' [PRISMA_flowdiagram()]) specifying whether other studies were sought.
sr_flow_interactive <- function(plot,
                                urls,
                                previous,
                                other) {
  .Defunct("PRISMA_interactive_")
}

#' Defunct function - replaced by "PRISMA_data"
#' @description Defunct function - replaced by "PRISMA_data"
#' @seealso [PRISMA_data()]
#' @param data File to read in.
read_PRISMAdata <- function(data){ #nolint
  .Defunct("PRISMA_data")
}
