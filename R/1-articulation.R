#' @export
Articulation <- function(name, to, i) {
  # validation
  check_articulation_name(name)
  check_to(to)
  erify::check_n(i)

  # normalization
  names <- articulations$name
  if (!(name %in% names)) name <- names[which(articulations$abbr == name)]
  i <- as.integer(i)

  # construction
  articulation <- list(name = name, to = to, i = i)
  class(articulation) <- "Articulation"
  articulation
}


articulations <- data.frame(
  name = c(
    "accent", "staccato", "staccatissimo", "tenuto", "marcato",
    "scoop", "plop", "doit", "falloff", "stress", "unstress", "soft accent"
  ),

  abbr = c(">", ".", "'", "-", "^", rep(NA, 4), ",", "u", "<>")
)


check_articulation_name <- function(name) {
  erify::check_string(name)

  names <- articulations$name
  abbrs <- articulations$abbr
  terms <- character()

  for (i in seq_along(names)) {
    name_i <- names[i]
    abbr_i <- abbrs[i]

    term <- if (is.na(abbr_i)) {
      sprintf('`"%s"`', name_i)
    } else {
      sprintf('`"%s"` (or `"%s"`)', name_i, abbr_i)
    }

    terms <- c(terms, term)
  }

  valid <- c(names, abbrs[!is.na(abbrs)])
  general <- sprintf("`name` must be %s.", erify::join(terms))
  erify::check_content(name, valid, NULL, general)
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
