#' @keywords internal
#' @export
Duration.character <- function(x, ...) {
  duration <- parse_duration_notation(x)
  class(duration) <- "Duration"
  duration
}


parse_duration_notation <- function(duration) {
  duration <- gsub(" ", "", duration)

  # regular expressions
  re_type <- paste(
    c(duration_types$name, duration_types$abbr),
    collapse = "|"
  )

  re_base <- paste0("(", re_type, ")", "\\.{0,4}")
  re_ratio <- paste0("/[1-9][0-9]*(\\*\\(", re_base, "/", re_base, "\\))?")

  # extraction
  base <- regmatches(duration, regexpr(re_base, duration))
  ratios <- regmatches(duration, gregexpr(re_ratio, duration))[[1]]

  # parsing
  parsed_base <- parse_duration_base(base)
  parsed_ratios <- lapply(ratios, parse_tuplet_ratio)

  # construction
  c(parsed_base, list(ratios = parsed_ratios))
}


parse_duration_base <- function(base) {
  # regular expressions
  re_type <- paste(
    c(duration_types$name, duration_types$abbr),
    collapse = "|"
  )

  re_dot <- "\\.{1,4}"

  # extraction
  type <- regmatches(base, regexpr(re_type, base))
  dot <- regmatches(base, regexpr(re_dot, base))

  # normalization
  if (type %in% duration_types$abbr) {
    type <- duration_types$name[duration_types$abbr == type]
  }

  dot <- if (length(dot) == 0) 0L else nchar(dot)

  # construction
  list(type = type, dot = dot)
}


parse_tuplet_ratio <- function(ratio) {
  # extraction
  parts <- strsplit(ratio, "/|\\*|\\(|\\)|\\s")[[1]]
  parts <- parts[parts != ""]

  # normalization
  n <- as.integer(parts[1])

  if (length(parts) == 1) {
    take <- NULL
    unit <- NULL
  } else {
    take <- parse_duration_base(parts[2])
    unit <- parse_duration_base(parts[3])
  }

  # construction
  list(n = n, take = take, unit = unit)
}


divide_duration_type <- function(type, n) {
  types <- duration_types$name
  k <- floor(log2(n)) + which(types == type)
  types[k]
}
