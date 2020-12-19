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
  check_type(name, "character")
  check_length(name, 1)
  check_na(supplied = name, name = "name")
}


check_line_as <- function(as) {
  ass <- c("part", "staff", "voice")

  m <- ass %>%
    sapply(function(s) paste0('"', s, '"')) %>%
    coordinate("or") %>%
    paste0("`as` must be ", ., ".")

  check_type(as, "character", general = m)
  check_length(as, 1, general = m)
  check_content(supplied = as, valid = ass, general = m)
}


check_line_to <- function(to) {
  if (is.null(to)) {
    return(invisible(to))
  }

  check_type(to, "character")
  check_length(to, 1)
  check_na(supplied = to, name = "to")
}


check_line_after <- function(after) {
  general <- "`after` must be TRUE or FALSE."

  check_type(after, "logical", general = general)
  check_length(after, 1, general = general)
  check_content(
    supplied = after,
    valid = expression(!is.na(supplied)),
    general = general
  )
}


check_line_offset <- function(offset) {
  check_type(offset, c("double", "integer"))

  check_length(offset, Inf)

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

  check_add_line_name(name, lns)
  check_add_line_to(to, lns)

  music$line_names <- c(lns, name)

  parts <- music$parts
  l <- length(parts)

  # initialize `music$parts`
  if (l == 0) {
    music$parts <- term %>% # voice
      list() %>% # voices
      list(voices = .) %>% # staff
      list() %>% # staffs
      list(staffs = .) %>% # part
      list() # parts

    return(music)
  }

  # add to the end of `music`, if `to` is not specified
  if (is.null(to)) {
    if (as == "part") {
      music$parts[[l + 1]] <- term %>% # voice
        list() %>% # voices
        list(voices = .) %>% # staff
        list() %>% # staffs
        list(staffs = .) # part

    } else {
      part <- music$parts[[l]]
      m <- length(part$staffs)

      if (as == "staff") {
        music$parts[[l]]$staffs[[m + 1]] <- term %>% # voice
          list() %>% # voices
          list(voices = .) # staff

      } else if (as == "voice") {
        staff <- part$staffs[[m]]
        n <- length(staff$voices)

        music$parts[[l]]$staffs[[m]]$voices[[n + 1]] <- term
      }
    }

    return(music)
  }

  # add to specified `to`
  for (i in 1:l) {
    part <- parts[[i]]
    staffs <- part$staffs
    m <- length(staffs)

    for (j in 1:m) {
      staff <- staffs[[j]]
      voices <- staff$voices
      n <- length(voices)

      for (k in 1:n) {
        voice <- voices[[k]]
        voice_name <- voice$name

        if (to == voice_name) {
          if (as == "part") {
            music$parts <- term %>% # voice
              list() %>% # voices
              list(voices = .) %>% # staff
              list() %>% # staffs
              list(staffs = .) %>% # part
              list() %>% # parts
              append(parts, ., ifelse(after, i, i - 1))

          } else if (as == "staff") {
            music$parts[[i]]$staffs <- term %>% # voice
              list() %>% # voices
              list(voices = .) %>% # staff
              list() %>% # staffs
              append(staffs, ., ifelse(after, j, j - 1))

          } else if (as == "voice") {
            check_voice_number(n, voice_name)

            music$parts[[i]]$staffs[[j]]$voices <- term %>% # voice
              list() %>% # voices
              append(voices, ., ifelse(after, k, k - 1))
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
