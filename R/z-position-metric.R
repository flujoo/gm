check_offset <- function(offset) {
  if (is.null(offset)) {
    return(invisible())
  }

  erify::check_type(offset, c("double", "integer"))
  erify::check_length(offset, 1)

  pass <- offset == 0 || is_tied(offset)

  if (pass) {
    return(invisible())
  }

  general <-
    "`offset` must be `0`, a duration value, or a sum of duration values."

  specific <- "`offset` is `{offset}`."
  erify::throw(general, specific, list(offset = offset))
}
