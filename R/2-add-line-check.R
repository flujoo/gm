#' Check If Line's Name Has Been Used
#' @noRd
check_line_name <- function(name, lines) {
  if (is.null(name) || !(name %in% lines$name)) return(invisible())

  general <- paste(
    "The name of the Line must not have been used by",
    "any Line in the Music."
  )
  specifics <- sprintf('Name "%s" has been used.', name)
  erify::throw(general, specifics)
}
