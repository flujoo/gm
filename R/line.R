#' @export
Line <- function(pitches, durations, name, as = "part", to = NULL,
                 bar = 1, offset = 0) {
  # normalize `pitches` and `durations`
  c_p <- class(pitches)[1]
  c_d <- class(durations)[1]

  if (c_p != "PitchLine") {
    pitches <- Pitch(pitches)
  }

  if (c_d != "DurationLine") {
    durations <- Duration(durations)
  }

  # check length
  l_p <- length(pitches)
  l_d <- length(durations)

  if (l_p != l_d) {
    glue::glue(
      "`pitches` and `durations` must have the same length.\n\n",
      "* `pitches` is of length {l_p}, `durations` {l_d}."
    ) %>% rlang::abort()
  }

  # check other arguments
  check_line_name(name)
  check_line_as(as)

  if (!is.null(to)) {
    check_line_to(to)
  }

  check_n(bar, name = "bar")
  check_line_offset(offset)

  # create Line
  list(
    pitches = pitches,
    durations = durations,
    name = name,
    as = as,
    to = to,
    bar = bar,
    offset = offset
  ) %>% `class<-`(c("Line"))
}



# validators --------------------------------------------------------

check_line_as <- function(as) {
  ass <- c("part", "staff", "voice")

  m <- ass %>%
    sapply(function(s) paste0('"', s, '"')) %>%
    join_words("or") %>%
    paste0("`as` must be ", ., ".")

  check_type(supplied = as, valid = "character", general = m)
  check_length(supplied = as, valid = 1, general = m, type = "character")
  check_content(supplied = as, valid = ass, general = m)
}


check_line_name <- function(name) {
  check_type(supplied = name, valid = "character", name = "name")
  check_length(supplied = name, valid = 1, name = "name", type = "character")
  check_na(supplied = name, name = "name")
}


check_line_to <- function(to) {
  check_type(supplied = to, valid = "character", name = "to")
  check_length(supplied = to, valid = 1, name = "to", type = "character")
  check_na(supplied = to, name = "to")
}


check_line_offset <- function(offset) {
  check_type(
    supplied = offset, valid = c("double", "integer"), name = "offset"
  )

  check_length(
    supplied = offset, valid = "l > 0", name = "offset",
    valid_phrase = "larger than 0"
  )

  if (offset != 0) {
    check_content(
      supplied = offset,
      valid = is_tied_value,
      general = "`offset` must be 0, a duration value or sum of ones."
    )
  }
}
