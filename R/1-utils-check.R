#' Check Argument `to`
#'
#' `to` is used in most of the constructor functions.
#' It indicates a line, where an object will be added, by its number or name.
#'
#' @noRd
check_to <- function(to) {
  if (is.null(to)) return(invisible())

  # For `to_j` in `Slur()`
  name <- deparse(substitute(to))

  general <- sprintf("`%s` must be a string or a positive integer.", name)
  erify::check_type(to, c("character", "double", "integer"), name, general)
  valid <- expression(erify::is_string(x) || erify::is_n(x))
  erify::check_content(to, valid, name, general)
}


check_offset <- function(offset) {
  if (is.null(offset)) return(invisible())
  erify::check_type(offset, c("double", "integer"))
  erify::check_length(offset, 1)
  if (offset == 0 || is_duration_value(offset)) return(invisible())

  general <- paste(
    "`offset` must be a non-negative multiple of 1/256",
    "which is the shortest valid duration."
  )

  specific <- sprintf("`offset` is `%s`.", offset)
  erify::throw(general, specific)
}
