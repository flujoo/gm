#' @keywords internal
#' @export
Duration.character <- function(x, ...) {
  # convert the duration notation to a list of simple notations
  x <- gsub(" ", "", x)
  x <- strsplit(x, "-")[[1]]
  x <- as.list(x)

  # regular expressions
  re_type <- paste(
    c(duration_types$name, duration_types$abbr),
    collapse = "|"
  )

  re_base <- paste0("(", re_type, ")", "\\.{0,4}")
  re_ratio <- paste0("/[1-9][0-9]*(\\*\\(", re_base, "/", re_base, "\\))?")

  # parse each simple duration
  for (i in seq_along(x)) {
    simple <- x[[i]]

    # the duration base
    base <- regmatches(simple, regexpr(re_base, simple))
    parsed_base <- parse_duration_base(base)

    # the tuplet ratios
    ratios <- regmatches(simple, gregexpr(re_ratio, simple))[[1]]
    parsed_ratios <- lapply(ratios, parse_tuplet_ratio)

    # the simple duration
    x[[i]] <- c(parsed_base, list(ratios = parsed_ratios))
  }

  # construction
  class(x) <- "Duration"
  x
}


parse_duration_base <- function(base) {
  # extract the duration type
  re_type <- paste(
    c(duration_types$name, duration_types$abbr),
    collapse = "|"
  )

  type <- regmatches(base, regexpr(re_type, base))

  # convert abbreviation to duration type
  if (type %in% duration_types$abbr) {
    type <- duration_types$name[duration_types$abbr == type]
  }

  # extract the number of dots
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
