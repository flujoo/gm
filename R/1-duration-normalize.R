#' Normalize Duration Value or Notation to `Duration` Object
#'
#' @keywords internal
#' @export
Duration <- function(x, ...) {
  UseMethod("Duration")
}


#' @keywords internal
#' @export
Duration.character <- function(x, ...) {
  structure(
    parse_duration_notation(x),
    class = "Duration"
  )
}


parse_duration_notation <- function(duration) {
  duration <- gsub(" ", "", duration)

  re_type <- paste(
    c(duration_types$name, duration_types$abbr),
    collapse = "|"
  )

  re_base <- paste0("(", re_type, ")", "\\.{0,4}")
  re_ratio <- paste0("/[1-9][0-9]*(\\*\\(", re_base, "/", re_base, "\\))?")

  base <- regmatches(duration, regexpr(re_base, duration))
  ratios <- regmatches(duration, gregexpr(re_ratio, duration))[[1]]

  parsed_base <- parse_duration_base(base)
  parsed_ratios <- lapply(ratios, parse_tuplet_ratio)

  c(parsed_base, list(ratios = parsed_ratios))
}


parse_duration_base <- function(base) {
  re_type <- paste(
    c(duration_types$name, duration_types$abbr),
    collapse = "|"
  )

  re_dot <- "\\.{1,4}"

  type <- regmatches(base, regexpr(re_type, base))
  dot <- regmatches(base, regexpr(re_dot, base))

  if (type %in% duration_types$abbr) {
    type <- duration_types$name[duration_types$abbr == type]
  }

  dot <- if (length(dot) == 0) 0L else nchar(dot)

  list(type = type, dot = dot)
}


parse_tuplet_ratio <- function(ratio) {
  parts <- strsplit(ratio, "/|\\*|\\(|\\)|\\s")[[1]]
  parts <- parts[parts != ""]

  n <- as.integer(parts[1])

  if (length(parts) == 1) {
    take <- NULL
    unit <- NULL

  } else {
    take <- parse_duration_base(parts[2])
    unit <- parse_duration_base(parts[3])
  }

  list(n = n, take = take, unit = unit)
}


divide_duration_type <- function(type, n) {
  types <- duration_types$name
  k <- floor(log2(n)) + which(types == type)
  types[k]
}


complete_tuplet <- function(duration) {
  type <- duration$type
  dot <- duration$dot
  ratios <- duration$ratios

  for (i in seq_along(ratios)) {
    ratio <- ratios[[i]]
    take <- ratio$take

    if (is.null(take)) {
      type <- divide_duration_type(type, ratio$n)
      unit <- list(type = type, dot = dot)
      duration$ratios[[i]]$unit <- unit
      duration$ratios[[i]]$take <- unit

    } else {
      type <- take$type
      dot <- take$dot
    }
  }

  duration
}
