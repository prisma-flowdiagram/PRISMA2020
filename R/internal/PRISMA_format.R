#' Format numbers with commas into numbers
#'
#' @description Turn strings containing numbers +/- commas into numbers
#' @param x the number to format
#' @return the number with commas removed
#' @keywords internal
PRISMA_format_number_ <- function(x) { #nolint
    if (is.character(x)) {
      x <- gsub(",", "", x)
      x <- gsub("[^0-9.]", "", x)
    }
    return(as.numeric(x))
}

#' Parse an exclusion reason into a data frame
#'
#' @description Parse an exclusion reason string and returns a dataframe
#' containing reasons and number
#' @param reasons the string to parse
#' @return a dataframe containing reasons and number applicable
#' @keywords internal
#'
PRISMA_parse_reasons_ <- function(reasons) { #nolint
    reasons_out <- NA
    if (grepl("[^0-9,]", as.character(reasons))) {
      reasons_out <- data.frame(
        reason = gsub(
          ",.*$",
          "",
          unlist(
            strsplit(
              as.character(reasons),
              split = "(;)( )?"
            )
          )
        ),
        n = scales::comma(
          PRISMA_format_number_( #nolint
            gsub(
              ".*?,([ 0-9,]*)|.*()",
              "\\1",
              unlist(
                strsplit(
                  as.character(reasons),
                  split = "(;)( )?"
                )
              )
            )
          )
        )
      )
    } else {
      reasons_out <- data.frame(
        reason = "",
        n = scales::comma(
          PRISMA_format_number_(as.character(reasons))
        )
      )
    }
    return(reasons_out)
}

#' Formats multiple exclusion reasons properly for printing
#'
#' @description Parse an exclusion reason dataframe from
#' [PRISMA2020::PRISMA_parse_reasons_()] and returns a properly formatted string
#' @param df the dataframe to parse
#' @return a string ready for printing
#' @keywords internal
#'
PRISMA_format_reasons_ <- function(df) { #nolint
    out_string <- paste0(
      ":",
      paste(
        paste(
          "\n",
          df[, 1],
          " (n = ", df[, 2], ")",
        sep = ""
        ),
      collapse = ""
      )
    )
  return(out_string)
}