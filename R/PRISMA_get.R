#' Calculate the correct height of a box from a list (e.g. of exclusion reasons)
#' @description Get the correct height for a box
#' @param n the number of rows of text in the label
#' @param offset the offset height (e.g. 3.5)
#' @param min the minimum number of rows before adjusting
#' @return the height of the box
#' @keywords internal
PRISMA_get_height_ <- function (n, offset, min = 2) { #nolint
  lines <- n + 1
  if (isTRUE(lines > min)) {
    height <- offset + (lines * 0.25) - (min * 0.25)
  } else {
    height <- offset
  }
  height
}

#' Calculate the correct position of a node
#' @description Get the correct position for a node
#' @param first_box_location the location of the first node
#' @param offset the offset from the first node
#' @param length_orig the width/height of the original node
#' @param length_new the width/height of the new node
#' @param negative_offset is the offset negative (defaults to false)
#' @return the position of the node
#' @keywords internal
PRISMA_get_pos_ <- function (first_box_location, offset, length_orig, length_new, negative_offset = FALSE) { #nolint
  if (negative_offset == FALSE) {
    pos <- first_box_location + offset + (length_orig / 2) + (length_new / 2)
  } else {
    pos <- first_box_location - offset - (length_orig / 2) - (length_new / 2)
  }
  pos
}