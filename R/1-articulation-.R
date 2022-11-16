#' @export
Articulation <- function(name, to, i) {
  # validation
  check_articulation_name(name)
  check_to(to)
  erify::check_n(i)

  # normalization
  name <- normalize_articulation_name(name)
  i <- as.integer(i)

  # construction
  articulation <- list(name = name, to = to, i = i)
  class(articulation) <- "Articulation"
  articulation
}


check_articulation_name <- function(name) {
  erify::check_string(name)

  pass <- any(!is.na(articulations) & articulations == tolower(name))
  if (pass) return(invisible())

  general <- sprintf(
    'Can not create an Articulation with `name = "%s"`.',
    name
  )

  erify::throw(general)
}


normalize_articulation_name <- function(name) {
  name <- tolower(name)
  i <- which(articulations == name, arr.ind = TRUE)[1]
  articulations$musescore[i]
}


#' @export
print.Articulation <- function(x, ...) {
  name <- x$name
  to <- x$to
  i <- x$i

  cat(capitalize(strsplit(name, " ")[[1]]), "\n")
  cat("\n")

  s_to <- if (is.character(to)) paste0('"', to, '"') else to
  cat("* to be added to Line", s_to, "\n")
  cat("* to be added at position", i, "\n")
}
