#' @export
Tempo <- function(tempo, unit = NULL, bar = NULL, offset = NULL) {
  # check arguments -------------------------------------------------------
  check_tempo(tempo)
  check_tempo_unit(unit)

  if (!is.null(bar)) {
    check_positive_integer(bar)
  }

  check_line_offset(offset)


  # normalize arguments ---------------------------------------------------
  tempo %<>% round(2)

  if (is.null(unit)) {
    unit <- "q"
  }

  d <- to_Duration(unit)
  unit <- print(d, silent = TRUE)
  # for Element "per-minute"
  bpm <- (tempo / to_value(d)) %>% round(2)


  # create Tempo ----------------------------------------------------------
  list(
    tempo = tempo,
    unit = unit,
    bpm = bpm,
    bar = bar,
    offset = offset
  ) %>% `class<-`("Tempo")
}


#' @export
print.Tempo <- function(x, context = "console", silent = FALSE, ...) {
  # convert `x$unit` and `x$bpm` ------------------------------------------
  s <- paste(x$unit, "=", x$bpm)


  # convert `x$bar` and `x$offset` ----------------------------------------
  if (context == "inside") {

  } else if (context == "console") {
    s %<>% paste("Tempo", .)
    specifics <- character(0)

    bar <- x$bar
    offset <- x$offset
    s_bar <- "to be added at bar {bar}"
    s_offset <- "with offset {offset}"

    if (!is.null(bar)) {
      if (is.null(offset)) {
        s_bar_offset <- s_bar
      } else {
        s_bar_offset <- paste(s_bar, s_offset)
      }
    } else {
      if (!is.null(offset)) {
        bar <- 1
        s_bar_offset <- paste(s_bar, s_offset)
      } else {
        s_bar_offset <- NULL
      }
    }

    specifics %<>% c(s_bar_offset)

    s %<>% generate_string(specifics, environment())
  }


  # print or return -------------------------------------------------------
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}



# validators --------------------------------------------------------------

check_tempo <- function(tempo) {
  check_type(tempo, c("double", "integer"))
  check_length(tempo, 1)

  # MuseScore tempo range
  valid <- expression(x >= 5 && x <= 999)
  general <- "`tempo` must be a number between 5 and 999."
  check_content(tempo, valid, general = general)
}


check_tempo_unit <- function(unit) {
  if (is.null(unit)) {
    return()
  }

  check_type(unit, c("character", "double", "integer"))
  check_length(unit, 1)

  # intersection of MuseScore and Finale valid beat units are
  # from "whole" to "16th"
  valid <- duration_types[4:8] %>% c(paste0(., "."))

  s_valid <- valid %>%
    sapply(quote_string) %>%
    paste(collapse = ", ")

  general <- paste0(
    "`unit` must be a duration notation, abbreviation or duration value, ",
    "which is equivalent to any of the following: ",
    s_valid,
    "."
  )

  if (is.character(unit)) {
    # include abbreviations
    valid <-
      duration_type_abbrs[4:8] %>%
      c(paste0(., ".")) %>%
      c(valid)

  } else if (is.numeric(unit)) {
    # include values
    valid %<>%
      lapply(to_Duration) %>%
      sapply(to_value)
  }

  check_content(unit, valid, general = general)
}



# Music + Tempo -----------------------------------------------------------

TempoLine <- function() {
  list(tempos = list()) %>% `class<-`("TempoLine")
}


#' @keywords internal
#' @export
`+.TempoLine` <- function(tempo_line, tempo) {
  # normalize bar and offset in `tempo`
  if (is.null(tempo$bar)) {
    tempo$bar <- 1L
  }

  if (is.null(tempo$offset)) {
    tempo$offset <- 0
  }

  # unpack
  tempos <- tempo_line$tempos
  l <- length(tempos)

  # early return
  if (l == 0) {
    tempo_line$tempos[[1]] <- tempo
    return(tempo_line)
  }

  # replace the Tempo with the same bar and offset in `tempo_line`,
  # or just append `tempo` and sort `tempos` latter
  for (i in 1:l) {
    tempo_i <- tempos[[i]]
    con <- tempo_i$bar == tempo$bar && tempo_i$offset == tempo$offset

    if (con) {
      tempos[[i]] <- tempo
      break
    }

    if (!con && i == l) {
      tempos %<>% c(list(tempo))
    }
  }

  tempo_line$tempos <- sort_clefs(tempos)
  tempo_line
}


add.Tempo <- function(term, music) {
  tl <- music$tempo_line

  if (is.null(tl)) {
    tl <- TempoLine()
  }

  music$tempo_line <- tl + term
  music
}


#' @keywords internal
#' @export
print.TempoLine <- function(x, silent = FALSE, ...) {
  tempos <- x$tempos
  l <- length(tempos)

  # empty form
  if (l == 0) {
    s <- ""

  # short form
  } else if (l == 1) {
    tempo <- tempos[[1]]
    bar <- tempo$bar
    offset <- tempo$offset

    if (offset != 0) {
      s_bar_offset <- " at bar {bar} with offset {offset}"
    } else if (bar != 1) {
      s_bar_offset <- " at bar {bar}"
    } else {
      s_bar_offset <- ""
    }

    s_tempo <- print(tempo, "inside", TRUE)
    s <- glue::glue("Tempo ", s_tempo, s_bar_offset)

  # long form
  } else {
    general <- "Tempos"

    specifics <- sapply(tempos, function(tempo) {
      bar <- tempo$bar
      offset <- tempo$offset

      if (offset != 0) {
        s_bar_offset <- " at bar {bar} with offset {offset}"
      } else {
        s_bar_offset <- " at bar {bar}"
      }

      s_tempo <- print(tempo, "inside", TRUE)
      glue::glue(s_tempo, s_bar_offset) %>% unclass()
    })

    s <- generate_string(general, specifics, environment())
  }

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}



# Music -> MusicXML -------------------------------------------------------

merge_tempo_line <- function(lines, tempo_line, meters) {
  if (is.null(tempo_line)) {
    return(lines)
  }

  tempos <- tempo_line$tempos

  # normalize bar and offset of each Tempo
  for (i in 1:length(tempos)) {
    tempo <- tempos[[i]]
    bar <- tempo$bar
    offset <- tempo$offset
    . <- normalize_bar_offset(bar, offset, meters)
    tempos[[i]]$bar <- .$bar
    tempos[[i]]$offset <- .$offset
  }

  tempos %<>% sort_clefs()

  # merge to first bar
  measures <- lines[[1]]$measures
  l <- length(measures)

  # merge Tempos measure by measure
  bars <- sapply(tempos, function(tempo) tempo$bar) %>%
    as.integer() %>%
    unique()

  for (bar in bars) {
    if (bar > l) {
      break
    }

    # get Tempos with bar `bar`
    ts <- Filter(function(tempo) tempo$bar == bar, tempos)

    # store Tempos, forwards and backup
    ns <- list()
    # offset accumulator
    v <- 0

    for (i in 1:length(ts)) {
      tempo <- ts[[i]]
      offset <- tempo$offset

      d <- offset - v
      v <- offset

      if (d == 0) {
        ns %<>% c(list(tempo))
      } else {
        ns %<>% c(list(Move(d, "forward"), tempo))
      }
    }

    # add backup
    v_meter <- find_meter(bar, meters) %>% to_value()
    ns %<>% c(list(Move(v_meter - v, "forward"), Move(v_meter, "backup")))

    # merge `ns` to first part
    # get first item in current Measure
    a <- measures[[bar]]$notes[[1]]
    k <- ifelse(class(a) == "Attributes", 1, 0)
    lines[[1]]$measures[[bar]]$notes %<>% append(ns, k)
  }

  lines
}


#' @keywords internal
#' @export
to_Element.Tempo <- function(x, ...) {
  d <- x$unit %>% to_Duration()

  contents <- list(
    Element("beat-unit", d$type),
    Element("per-minute", x$bpm)
  )

  if (d$dot == 1) {
    contents %<>% append(list(Element("beat-unit-dot")), 1)
  }

  metronome <- Element("metronome", contents)
  direction_type <- Element("direction-type", metronome)
  sound <- Element("sound", NULL, list(tempo = x$tempo))

  Element("direction", list(direction_type, sound), list(placement = "above"))
}
