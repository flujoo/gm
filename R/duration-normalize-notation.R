#' @keywords internal
#' @export
Duration.character <- function(x, ...) {
  x <- gsub(" ", "", x)

  # regular expressions
  re_type <- paste(
    c(duration_types$name, duration_types$abbr),
    collapse = "|"
  )

  re_base <- paste0("(", re_type, ")", "\\.{0,4}")
  re_ratio <- paste0("/[1-9][0-9]*(\\*\\(", re_base, "/", re_base, "\\))?")

  # extraction
  base <- regmatches(x, regexpr(re_base, x))
  ratios <- regmatches(x, gregexpr(re_ratio, x))[[1]]

  # parsing
  parsed_base <- parse_duration_base(base)
  parsed_ratios <- lapply(ratios, parse_tuplet_ratio)

  # construction
  duration <- c(parsed_base, list(ratios = parsed_ratios))
  class(duration) <- "Duration"
  duration
}


parse_duration_base <- function(base) {
  # extract duration type
  re_type <- paste(
    c(duration_types$name, duration_types$abbr),
    collapse = "|"
  )

  type <- regmatches(base, regexpr(re_type, base))

  # convert abbreviation to duration type
  if (type %in% duration_types$abbr) {
    type <- duration_types$name[duration_types$abbr == type]
  }

  # extract number of dots
  dot <- nchar(regmatches(base, regexpr("\\.{1,4}", base)))
  if (length(dot) == 0) dot <- 0L

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
