#' Normalize Tuplet Notation to `Tuplet` Object
#'
#' @keywords internal
#' @export
Tuplet <- function(tuplet_notation) {
  parts <- strsplit(tuplet_notation, "/|\\*|\\(|\\)|\\s")[[1]]
  parts <- parts[parts != ""]

  n = as.integer(parts[1])

  if (length(parts) == 3) {
    take <- parse_duration_base(parts[2])
    unit <- parse_duration_base(parts[3])
  } else {
    take <- NULL
    unit <- NULL
  }

  tuplet <- list(n = n, take = take, unit = unit)
  class(tuplet) <- "Tuplet"
  tuplet
}
