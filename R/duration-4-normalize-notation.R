#' @keywords internal
#' @export
Duration.character <- function(x, ...) {
  x <- gsub(" ", "", x)
  untied <- strsplit(x, "-")[[1]]
  parsed <- list()

  re_type <- paste(c(duration_types$name, duration_types$abbr), collapse = "|")
  re_base <- paste0("(", re_type, ")", "\\.{0,4}")
  re_tuplet <- paste0("/[1-9][0-9]*(\\*\\(", re_base, "/", re_base, "\\))?")

  for (atomic in untied) {
    base <- regmatches(atomic, regexpr(re_base, atomic))
    parsed_base <- parse_duration_base(base)
    tuplets <- regmatches(atomic, gregexpr(re_tuplet, atomic))[[1]]
    parsed_tuplets <- lapply(tuplets, Tuplet)
    parsed_atomic <- c(parsed_base, list(tuplets = parsed_tuplets))
    parsed <- c(parsed, list(parsed_atomic))
  }

  class(parsed) <- "Duration"
  parsed
}


parse_duration_base <- function(base) {
  re_type <- paste(c(duration_types$name, duration_types$abbr), collapse = "|")
  type <- regmatches(base, regexpr(re_type, base))

  # use full names instead of abbreviations
  if (type %in% duration_types$abbr) {
    type <- duration_types$name[duration_types$abbr == type]
  }

  dot <- nchar(regmatches(base, regexpr("\\.{1,4}", base)))
  if (length(dot) == 0) dot <- 0L

  list(type = type, dot = dot)
}


#' Normalize Tuplet Notation to `Tuplet` Object
#'
#' @keywords internal
#' @export
Tuplet <- function(tuplet_notation) {
  parts <- strsplit(tuplet_notation, "/|\\*|\\(|\\)|\\s")[[1]]
  parts <- parts[parts != ""]

  n <- as.integer(parts[1])

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


#' @keywords internal
#' @export
print.Tuplet <- function(x, ...) {
  cat(to_string(x), "\n")
}
