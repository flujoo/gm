#' @export
Part <- function(pitches, durations, ...) {
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

  # create Part
  part <- list(pitches = pitches, durations = durations)
  class(part) <- "Part"

  # check `...`
  args <- list(...)
  ns <- names(args)

  if ("as" %in% ns) {
    part$as <- args$as %T>%
      check_part_as()
  } else {
    part$as <- "part"
  }

  if ("to" %in% ns) {
    part$to <- args$to %T>%
      check_part_to()
  }

  if ("bar" %in% ns) {
    part$bar <- args$bar %T>%
      check_part_bar()
  } else {
    part$bar <- 1
  }

  if ("offset" %in% ns) {
    part$offset <- args$offset %T>%
      check_part_offset()
  } else {
    part$offset <- 0
  }

  if ("name" %in% ns) {
    part$name <- args$name %T>%
      check_part_name()
  }

  part
}



# validators --------------------------------------------------------

check_part_as <- function(as) {
  ass <- c("part", "staff", "voice")

  m <- ass %>%
    sapply(function(s) paste0('"', s, '"')) %>%
    join_words("or") %>%
    paste0("`as` must be ", ., ".")

  check_type(supplied = as, valid = "character", general = m)
  check_length(supplied = as, valid = 1, general = m, type = "character")
  check_content(supplied = as, valid = ass, general = m)
}


check_part_name <- function(name, argument) {
  check_type(supplied = name, valid = "character", name = "name")
  check_length(supplied = name, valid = 1, name = "name", type = "character")
}


check_part_bar <- function(bar) {
  check_type(supplied = bar, valid = c("double", "integer"), name = "bar")
  check_length(supplied = bar, valid = 1, name = "bar", type = "numeric")
  check_positive_integer(supplied = bar, name = "bar")
}


check_part_to <- function(to) {
  check_type(
    supplied = to, valid = c("character", "double", "integer"), name = "to"
  )

  check_length(supplied = to, valid = 1, name = "to", type = typeof(to))

  if (is.numeric(to)) {
    check_positive_integer(
      supplied = to,
      name = "to",
      general = "When `to` is a numeric, it must be a positive integer."
    )
  }
}


check_part_offset <- function(offset) {
  check_type(
    supplied = offset, valid = c("double", "integer"), name = "offset"
  )

  check_length(
    supplied = offset, valid = "l > 0", name = "offset",
    valid_phrase = "larger than 0"
  )

  check_content(
    supplied = offset,
    valid = is_tied_value,
    general = "`offset` must be a duration value or sum of ones."
  )
}



# Pitch + Duration --------------------------------------------------

#' @export
`+.HalfPart` <- function(pitches, durations) {
  c_p <- class(pitches)[1]
  c_d <- class(durations)[1]

  check_part_classes(c_p, c_d)
  check_part_length(pitches, durations)

  # normalize argument order
  if (c_p == "DurationLine" && c_d == "PitchLine") {
    . <- pitches
    pitches <- durations
    durations <- .
  }

  Part(pitches, durations)
}


check_part_classes <- function(class_left, class_right) {
  con <-
    class_left == "PitchLine" && class_right == "DurationLine" ||
    class_right == "PitchLine" && class_left == "DurationLine"

  if (!con) {
    a_left <- ifelse(class_left %in% vowel_types, "an", "a")
    a_right <- ifelse(class_right %in% vowel_types, "an", "a")

    glue::glue(
      "One side of `+` must be a PitchLine object ",
      "(the output of `Pitch()`), ",
      "the other side must be a DurationLine object ",
      "(the output of `Duration()`).\n\n",
      "* The left side is {a_left} {class_left}, ",
      "the right side is {a_right} {class_right}."
    ) %>% rlang::abort()
  }
}


check_part_length <- function(left, right) {
  l_left <- length(left)
  l_right <- length(right)

  if (l_left != l_right) {
    glue::glue(
      "Both sides of `+` must have the same length.\n\n",
      "* The left side is of length {l_left}, ",
      "the right side {l_right}."
    ) %>% rlang::abort()
  }
}
