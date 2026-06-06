#' Pull from CSV, or use a default value if not in the CSV
#' @description set the variable from the CSV, or a default value
#' @param expr the expression to get the variable from the CSV
#' @param var_name the variable name
#' @param default the default token
#' @return the result of 'expr', or 'default'
#' @keywords internal
PRISMA_default_or_csv_ <- function (expr, default, var_name = "not provided") { #nolint
  result <- expr
  if (length(result) == 0) {
    warning(
      c(
        "Variable ",
        var_name,
        " was not processed because it does",
        " not appear to exist within the CSV file",
        " using default value: ",
        default
      )
    )
    result <- default
  }
  result
}