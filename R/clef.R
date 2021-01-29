# https://www.dolmetsch.com/musictheory14.htm
#' @export
Clef <- function(sign, line = NULL, octave = NULL, position = NULL) {
  # check `sign`
  check_clef_sign(sign)
  # normalize `sign`
  sign %<>% toupper()

  # check `line`
  check_clef_line(line, sign)
  # normalize `line`
  if (is.null(line)) {
    line <- switch(
      sign,
      "G" = 2,
      "F" = 4,
      "C" = 3
    )
  }

  # check `octave`
  check_clef_octave(octave, sign, line)

  # check `position`
  if (!is.null(position)) {
    check_positive_integer(position)
  }

  # create Clef
  list(sign = sign, line = line, octave = octave, position = position) %>%
    `class<-`("Clef")
}


#' @export
print.Clef <- function(x, context = "console", silent = FALSE, ...) {
  # unpack
  sign <- x$sign
  line <- x$line
  octave <- x$octave
  position <- x$position

  # convert `sign` and `line` to string
  s_line <- as.character(line)

  if (sign == "G") {
    s <- switch(
      s_line,
      "1" = "French Clef",
      "2" = "treble Clef"
    )

  } else if (sign == "F") {
    s <- switch(
      s_line,
      "3" = "baritone F-Clef",
      "4" = "bass Clef",
      "5" = "subbass Clef"
    )

  } else if (sign == "C") {
    s <- switch(
      s_line,
      "1" = "soprano Clef",
      "2" = "mezzo-soprano Clef",
      "3" = "alto Clef",
      "4" = "tenor Clef",
      "5" = "baritone C-Clef"
    )
  }

  # convert `octave` to string
  if (!is.null(octave)) {
    s_octave <- ifelse(octave == 1, "octave up", "octave down")
    s %<>% paste(s_octave, .)
  }

  # convert `x` to string
  if (context == "inside") {

  } else if (context == "console") {
    if (!is.null(position)) {
      specific <- "to be added at position {position}"
      s %<>% generate_string(specific, environment())
    }
  }

  # print or return string
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


ClefLine <- function() {
  list(clefs = list()) %>% `class<-`("ClefLine")
}


# for "incompatible methods" error, can't use name `+.ClefLine`
merge_clef <- function(clef_line, clef) {
  clef %<>% normalize_key_bar(name = "position")
  clef_line$clefs %<>% merge_key(clef, name = "position")
  # the above two utils are borrowed from key.R

  clef_line
}


#' @keywords internal
#' @export
print.ClefLine <- function(x, silent = FALSE, ...) {
  # unpack
  clefs <- x$clefs
  l <- length(clefs)

  # empty form
  if (l == 0) {
    s <- ""

  # short form
  } else if (l == 1) {
    # unpack
    clef <- clefs[[1]]
    position <- clef$position

    # convert `clef` to string
    s <- print(clef, context = "inside", silent = TRUE)

    # omit `position` if it is 1
    if (position != 1) {
      s %<>% glue::glue(" at position {position}")
    }

  # long form
  } else {
    ss <- sapply(clefs, function(clef) {
      clef %>%
        print(context = "inside", silent = TRUE) %>%
        glue::glue(" at position {clef$position}")
    })

    s <- paste(ss, collapse = ", ")
  }

  # print or return string
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


#' @export
`+.Clef` <- function(clef, line) {
  c_clef <- class(clef)[1]
  c_line <- class(line)[1]

  check_binary_classes(c_clef, c_line, "Clef", "Line")

  # normalize argument order
  if (c_clef == "Line" && c_line == "Clef") {
    . <- clef
    clef <- line
    line <- .
  }

  # check `clef` against `line` length
  check_clef(clef$position, length(line$durations$durations))

  cl <- line$clefs

  if (is.null(cl)) {
    cl <- ClefLine()
  }

  line$clefs <- merge_clef(cl, clef)
  line
}


check_clef <- function(position, length) {
  if (!is.null(position) && position > length) {
    general <-
      "Can only add the Clef at a position no larger than the Line length."

    specific <- paste(
      "Can't add the Clef at position {position},",
      "which is larger than the Line length {length}."
    )

    show_errors(general, specific, env = environment())
  }
}


# guess a Clef for `pitches`, which is "to_Pitched"
infer_clef <- function(pitches) {
  # convert `pitches` to values
  pitches %<>%
    to_values.pitches() %>%
    unlist() %>%
    # remove any PitchRest
    {.[. != 0]}

  l <- length(pitches)

  if (l == 0) {
    return(Clef("G"))
  }

  # ranges for typical Clefs
  ranges <- list(
    # treble: C4, A5
    c(60, 81),
    # bass: E2, C4
    c(40, 60),
    # octave up treble: C5, A6
    c(72, 93),
    # octave down bass: E1, C3
    c(28, 48),
    # alto: D3, B4
    c(50, 71)
  )

  # measure the fitness of each range for `pitches`
  get_fitness <- function(range) {
    con <- pitches >= range[1] & pitches <= range[2]
    table(con)["TRUE"]
  }

  # find the fittest
  fs <- sapply(ranges, get_fitness)
  k <- which(fs == max(fs, na.rm = TRUE))[1]

  # get the corresponding Clef
  switch(
    as.character(k),
    "1" = Clef("G"),
    "2" = Clef("F"),
    "3" = Clef("G", 2, 1),
    "4" = Clef("F", 4, -1),
    "5" = Clef("C")
  )
}



# Clef validators ---------------------------------------------------------

check_clef_sign <- function(sign) {
  check_type(sign, "character")
  check_length(sign, 1)

  general <- '`sign` must be "G", "F" or "C".'
  check_content(sign, c("G", "F", "C", "g", "f", "c"), general = general)
}


check_clef_line <- function(line, sign) {
  if (is.null(line)) {
    return()
  }

  check_positive_integer(line)

  valid <- switch(
    sign,
    "G" = 1:2,
    "F" = 3:5,
    "C" = 1:5
  )

  phrase <- coordinate(valid)
  general <- 'When `sign` is "{sign}", `line` must be {phrase}.'
  check_content(line, valid, general = general, sign = sign, phrase = phrase)
}


check_clef_octave <- function(octave, sign, line) {
  if (is.null(octave)) {
    return()
  }

  con <- (sign == "G" && line == 2) || (sign == "F" && line == 4)

  if (!con) {
    general <- paste(
      'Only when `sign` is "G" and `line` is 2,',
      'or `sign` is "F" and `line` is 4,',
      '`octave` can be set.'
    )
    specific <- '`sign` is "{sign}", `line` is {line}.'
    show_errors(general, specific, env = environment())

  } else {
    check_type(octave, c("double", "integer"))
    check_length(octave, 1)
    check_content(octave, c(-1, 1))
  }
}
