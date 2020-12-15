#' @export
Line <- function(pitches, durations, name, as = "part", to = NULL,
                 after = TRUE, bar = 1, offset = 0) {
  # normalize `pitches` and `durations`
  c_p <- class(pitches)[1]
  c_d <- class(durations)[1]

  if (c_p != "PitchLine") {
    pitches <- PitchLine(pitches)
  }

  if (c_d != "DurationLine") {
    durations <- DurationLine(durations)
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
  check_line_to(to)
  check_line_after(after)
  check_n(bar, name = "bar")
  check_line_offset(offset)

  # create Line
  list(
    pitches = pitches,
    durations = durations,
    name = name,
    as = as,
    to = to,
    after = after,
    bar = bar,
    offset = offset
  ) %>% `class<-`("Line")
}



# validators in `Line` ----------------------------------------------

check_line_name <- function(name) {
  check_type(supplied = name, valid = "character", name = "name")
  check_length(supplied = name, valid = 1, name = "name", type = "character")
  check_na(supplied = name, name = "name")
}


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


check_line_to <- function(to) {
  if (is.null(to)) {
    return(invisible(to))
  }

  check_type(supplied = to, valid = "character", name = "to")
  check_length(supplied = to, valid = 1, name = "to", type = "character")
  check_na(supplied = to, name = "to")
}


check_line_after <- function(after) {
  general <- "`after` must be TRUE or FALSE."

  check_type(supplied = after, valid = "logical", general = general)

  check_length(
    supplied = after, valid = 1, type = "logical", general = general
  )

  check_content(
    supplied = after,
    valid = expression(!is.na(supplied)),
    general = general
  )
}


check_line_offset <- function(offset) {
  check_type(
    supplied = offset, valid = c("double", "integer"), name = "offset"
  )

  check_length(
    supplied = offset, valid = "l > 0", name = "offset",
    valid_phrase = "larger than 0"
  )

  check_content(
    supplied = offset,
    valid = expression(supplied == 0 || is_tied_value(supplied)),
    general = "`offset` must be 0, a duration value or sum of ones."
  )
}



# + Line ------------------------------------------------------------

add.Line <- function(term, music) {
  name <- term$name
  to <- term$to
  as <- term$as
  after <- term$after
  lns <- music$line_names
  parts <- music$parts
  l <- length(parts)

  check_add_line_name(name, lns)
  check_add_line_to(to, lns)

  music$line_names <- c(lns, name)

  # initialize `music$parts`
  if (l == 0) {
    music$parts <- list(list(list(term)))
    return(music)
  }

  # add to the end of `music`, if `to` is not specified
  if (is.null(to)) {
    if (as == "part") {
      music$parts[[l + 1]] <- list(list(term))

    } else {
      part <- music$parts[[l]]
      m <- length(part)

      if (as == "staff") {
        music$parts[[l]][[m + 1]] <- list(term)

      } else if (as == "voice") {
        staff <- part[[m]]
        n <- length(staff)
        music$parts[[l]][[m]][[n + 1]] <- term
      }
    }

    return(music)
  }

  # add to specified `to`
  for (i in 1:l) {
    part <- parts[[i]]
    m <- length(part)

    for (j in 1:m) {
      staff <- part[[j]]
      n <- length(staff)

      for (k in 1:n) {
        voice <- staff[[k]]
        voice_name <- voice$name

        if (to == voice_name) {

          if (as == "part") {
            music$parts <- append(
              parts,
              list(list(list(term))),
              ifelse(after, i, i - 1)
            )

          } else if (as == "staff") {
            music$parts[[i]] <- append(
              part,
              list(list(term)),
              ifelse(after, j, j - 1)
            )

          } else if (as == "voice") {
            check_voice_number(n, voice_name)

            music$parts[[i]][[j]] <- append(
              staff,
              list(term),
              ifelse(after, k, k - 1)
            )
          }

          return(music)
        }
      }
    }
  }
}



# validators in `add.Line` ------------------------------------------

check_add_line_name <- function(name, line_names) {
  con <-
    name %in% line_names

  if (con) {
    glue::glue(
      "Each Line in a Music object must have a unique name.",
      "\n\n",
      '* Name "{name}" has been used.'
    ) %>% rlang::abort()
  }
}


check_add_line_to <- function(to, line_names) {
  con <-
    !is.null(to) &&
    !(to %in% line_names)

  if (con) {
    glue::glue(
      "If `to` is specified,", " ",
      "it must refer to the name of a Line in the Music object.",
      "\n\n",
      '* Can\'t find Line with name "{to}".'
    ) %>% rlang::abort()
  }
}


check_voice_number <- function(voice_number, voice_name) {
  if (voice_number == 4) {
    glue::glue(
      "Each staff in a Music can have 4 voices at most.",
      "\n\n",
      '* Staff containing voice "{voice_name}" already has 4 voices.'
    ) %>% rlang::abort()
  }
}
