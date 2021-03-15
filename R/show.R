# constructors ------------------------------------------------------------

# generalization of MusicXML elements backup and forward
# `direction` is "backup" or "forward"
# `...` includes `staff` and `voice`
Move <- function(duration, direction, ...) {
  list(duration = duration, direction = direction, ...) %>%
    `class<-`("Move")
}


#' @keywords internal
#' @export
print.Move <- function(x, silent = FALSE, ...) {
  # convert `x$direction`
  s_direction <- switch(
    x$direction,
    "backup" = "<-",
    "forward" = "->"
  )

  # convert `x`
  s <- paste0(s_direction, x$duration)

  # print or return
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


# to represent MusicXML element note
# `...` includes `invisible`, `staff` and `voice`
# it's more convenient to add marks in Pitches rather than in Notes,
# since a Note may contain more than one Pitch at its early stage
Note <- function(duration, pitch = PitchRest(), ...) {
  list(duration = duration, pitch = pitch, ...) %>%
    `class<-`("Note")
}


#' @keywords internal
#' @export
print.Note <- function(x, silent = FALSE, ...) {
  s_duration <- print(x$duration, "inside", TRUE)
  s_pitch <- print(x$pitch, TRUE)

  s <- "({s_duration}, {s_pitch})" %>%
    glue::glue() %>%
    unclass()

  # print or return
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


# rest for whole measure
Rest <- function(duration, ...) {
  list(duration = duration, ...) %>%
    `class<-`("Rest")
}


#' @keywords internal
#' @export
print.Rest <- function(x, silent = FALSE, ...) {
  s <- x$duration

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


Measure <- function(notes, number) {
  list(notes = notes, number = number) %>%
    `class<-`("Measure")
}


#' @keywords internal
#' @export
print.Measure <- function(x, silent = FALSE, ...) {
  s <-
    sapply(x$notes, print, silent = TRUE, context = "inside") %>%
    paste(collapse = ", ") %>%
    paste0(x$number, ": ", .)

  # print or return
  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


# to represent MusicXML element attributes
Attributes <- function(attributes) {
  list(attributes = attributes) %>% `class<-`("Attributes")
}


#' @keywords internal
#' @export
print.Attributes <- function(x, silent = FALSE, ...) {
  s <-
    sapply(x$attributes, print, silent = TRUE, context = "inside") %>%
    paste(collapse = ", ")

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


Part <- function(measures, number, name) {
  if (is.null(name)) {
    name <- number
  }

  list(measures = measures, number = number, name = name) %>%
    `class<-`("Part")
}


Score <- function(parts) {
  list(parts = parts) %>% `class<-`("Score")
}



# Line -> Measures --------------------------------------------------------

# 1. combine pitches and Durations to Notes

# 2. segment Notes into Measures

# 3. add a backup to each Measure if the Line is not a part

# 4. convert offset to a forward if the Line is a voice, or untie offset
# into rests if not

# 5. generate empty Measures for bars before specified `$bar`
# if the Line is a voice, or Measures of Rest if not

# 6. append Measures to some Lines to make all Lines contain the same number
# of Measures


segment <- function(line, meters) {
  # unpack `line`
  bar <- line$bar
  offset <- line$offset
  pitches <- line$pitches$pitches
  durations <- line$durations$durations
  l <- length(durations)

  # unpack `line$number`
  number <- line$number
  n2 <- number[2]
  n3 <- number[3]
  voice <- (n2 - 1) * 4 + n3
  # every staff has four voices

  # generate Measures before `bar`
  if (bar == 1) {
    ms <- list()
  } else {
    ms <- generate_measures(1:(bar - 1), meters, n2, n3, voice)
  }

  # initialize current measure
  m <- normalize_offset(offset, n2, n3, voice)

  # meter value for current measure
  v_meter <- find_meter(bar, meters) %>% to_value()
  # accumulated value of current measure
  v_accum <- offset

  for (i in 1:l) {
    # unpack
    d <- durations[[i]]
    v <- to_value(d)
    p <- pitches[[i]]
    c_ <- class(p)

    # if to untie `v`
    untie <- FALSE

    repeat {
      v_temp <- v_accum + v

      # deal with cross-barline `d`
      if (v_temp > v_meter) {
        ds <-
          # get rest value of current measure,
          (v_meter - v_accum) %>%
          # untie it,
          untie_duration_value(decreasing = FALSE) %>%
          # and convert it to Durations
          lapply(to_Duration)

        # mark tie and convert `ds` to Notes
        for (j in 1:length(ds)) {
          ds[[j]] %<>% Note(
            pitch = mark_tie_in_segment(p, c_, j),
            staff = n2,
            voice = voice
          )
        }

        # mark tie in `p` for next measure
        p %<>% mark_tie_in_segment(c_)
      }

      # add `d` or whatever to `m`
      if (v_temp <= v_meter) {
        if (isFALSE(untie)) {
          # generate Note and add it to `m`
          m %<>% c(list(Note(d, p, staff = n2, voice = voice)))
        } else {
          m %<>% c(to_Notes(v, pitch = p, staff = n2, voice = voice))
          untie <- FALSE
        }
      } else {
        m %<>% c(ds)
      }

      # complete the last measure with rests or forward
      if (v_temp < v_meter && i == l) {
        m %<>% c(normalize_offset(v_meter - v_temp, n2, n3, voice))
      }

      # add `m` to `ms`
      if (v_temp >= v_meter || i == l) {
        # add backup to any staff and voice
        # do this only when the current measure is complete and
        # ready to be appended
        if (n2 > 1 || n3 > 1) {
          m %<>% append(list(Move(v_meter, "backup")), 0)
        }

        ms %<>% c(list(Measure(m, bar)))
      }

      # update and reset variables
      if (v_temp < v_meter) {
        v_accum <- v_temp
      } else if (v_temp == v_meter) {
        v_accum <- 0
      } else if (v_temp > v_meter) {
        v <- v_temp - v_meter
        # note that `v` may not be a duration value
        untie <- TRUE
        v_accum <- 0
      }

      if (v_temp >= v_meter) {
        m <- list()
        bar <- bar + 1
        v_meter <- find_meter(bar, meters) %>% to_value()
      }

      # break
      if (v_temp <= v_meter) {
        break
      }
    }
  }

  ms
}


# generate Measures for specified bars
generate_measures <- function(bars, meters, n2, n3, voice) {
  ms <- list()

  for (bar in bars) {
    # for voice
    if (n3 != 1) {
      ns <- list()

    } else {
      d <- find_meter(bar, meters) %>% to_value()
      r <- Rest(d, staff = n2, voice = voice)

      # for part
      if (n2 == 1) {
        ns <- list(r)

      # for staff
      } else {
        b <- Move(d, "backup")
        ns <- list(b, r)
      }
    }

    ms %<>% c(list(Measure(ns, bar)))
  }

  ms
}


# convert (tied) duration value to Notes
to_Notes <- function(value, ...) {
  value %>%
    untie_duration_value(decreasing = FALSE) %>%
    lapply(to_Duration) %>%
    lapply(Note, ...)
}


# convert offset to a empty list, a forward in a list, or rests
normalize_offset <- function(offset, n2, n3, voice) {
  if (offset == 0) {
    return(list())
  }

  # convert `offset` to rests when the Line is not a voice
  if (n3 == 1) {
    to_Notes(offset, invisible = TRUE, staff = n2, voice = voice)

  # convert `offset` to a forward when the Line is a voice
  } else if (n3 > 1) {
    Move(offset, "forward", staff = n2, voice = voice) %>% list()
    # add it to list for convenience of `segment`
  }
}


# mark tie in untied Notes
mark_tie_in_segment <- function(pitch, type, i = NULL) {
  if (type == "Pitch") {
    if (!is.null(i)) {
      pitch$tie_start <- TRUE
    }

    if (is.null(i) || i != 1) {
      pitch$tie_stop <- TRUE
    }

  } else if (type == "PitchChord") {
    if (!is.null(i)) {
      for (j in 1:length(pitch)) {
        pitch[[j]]$tie_start <- TRUE
      }
    }

    if (is.null(i) || i != 1) {
      for (j in 1:length(pitch)) {
        pitch[[j]]$tie_stop <- TRUE
      }
    }
  }

  pitch
}


# convert each Line to Measures and add it to `$measures`
segment.lines <- function(lines, meters) {
  for (i in 1:length(lines)) {
    lines[[i]]$measures <- segment(lines[[i]], meters)
  }

  lines
}


# append Measures to some Lines,
# to make all Lines contain the same number of Measures
equalize <- function(lines, meters) {
  # get the max length
  l <- lines %>%
    lapply(function(line) line$measures) %>%
    sapply(length) %>%
    max()

  for (i in 1:length(lines)) {
    # unpack
    line <- lines[[i]]
    measures <- line$measures
    l_ <- length(measures)
    number <- line$number
    n2 <- number[2]
    n3 <- number[3]
    voice <- (n2 - 1) * 4 + n3

    if (l_ < l) {
      lines[[i]]$measures <-
        (l_ + 1):l %>%
        generate_measures(meters, n2, n3, voice) %>%
        c(measures, .)
    }
  }

  lines
}



# Music -> Score ----------------------------------------------------------

# merge any staff or voice to its parent part
merge_lines <- function(lines) {
  for (i in 1:length(lines)) {
    # unpack
    line <- lines[[i]]
    number <- line$number

    # skip if `line` is a part
    if (all(number[2:3] == c(1, 1))) {
      next
    }

    # get its parent part's number
    number_part <- c(number[1], 1, 1)
    # locate the part
    k <- locate_key_line(lines, number_part)

    measures <- line$measures

    # merge
    for (j in 1:length(measures)) {
      lines[[k]]$measures[[j]]$notes %<>% c(measures[[j]]$notes)
    }
  }

  lines
}


# add Element staves to each part
add_staves <- function(lines) {
  for (i in 1:length(lines)) {
    # unpack
    line <- lines[[i]]
    number <- line$number

    # skip non-part
    if (any(number[2:3] != c(1, 1))) {
      next
    }

    n <- count_staves(number, lines)

    if (n > 1) {
      staves <- Element("staves", n)
      lines[[i]]$measures[[1]]$notes[[1]]$attributes %<>%
        append(list(staves), 0)
    }
  }

  lines
}


# count the number of staves in a part
count_staves <- function(number, lines) {
  ns <- integer()

  for (line in lines) {
    number_ <- line$number

    if (number_[1] == number[1]) {
      ns %<>% c(number_[2])
    }
  }

  ns %>%
    unique() %>%
    length()
}


# split any chord into notes in each part
split_chord <- function(lines) {
  for (i in 1:length(lines)) {
    # unpack
    line <- lines[[i]]
    number <- line$number

    # skip non-part
    if (any(number[2:3] != c(1, 1))) {
      next
    }

    measures <- line$measures
    for (j in 1:length(measures)) {
      notes <- measures[[j]]$notes

      # to store non-chords and split chords
      ns <- list()

      for (note in notes) {
        if (class(note) != "Note") {
          ns %<>% c(list(note))
          next
        }

        pitches <- note$pitch

        if (class(pitches) != "PitchChord") {
          ns %<>% c(list(note))
          next
        }

        # split chord into notes
        for (k in 1:length(pitches)) {
          # copy `note` to keep `$voice`, `$staff`, ...
          n <- note

          # change `$pitch`
          n$pitch <- pitches[[k]]

          # add `$chord` from the second
          if (k != 1) {
            n$chord <- TRUE
          }

          ns %<>% c(list(n))
        }
      }

      lines[[i]]$measures[[j]]$notes <- ns
    }
  }

  lines
}


to_Score <- function(lines) {
  parts <- list()

  for (line in lines) {
    number <- line$number

    if (any(number[2:3] != c(1, 1))) {
      next
    }

    parts %<>% c(list(Part(line$measures, number[1], line$name)))
  }

  Score(parts)
}



# Score -> MusicXML -------------------------------------------------------

#' @keywords internal
#' @export
print.Score <- function(x, divisions, silent = FALSE, ...) {
  pre <- paste(
    '<?xml version="1.0" encoding="UTF-8" standalone="no"?>',
    '<!DOCTYPE score-partwise PUBLIC',
    '"-//Recordare//DTD MusicXML 3.1 Partwise//EN"',
    '"http://www.musicxml.org/dtds/partwise.dtd">',
    sep = "\n"
  )

  s <-
    to_Element(x, divisions) %>%
    print(silent = TRUE) %>%
    paste0(pre, "\n", .)

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}


#' @keywords internal
#' @export
to_Element.Score <- function(x, divisions, ...) {
  # get Element "part-list"
  part_list <-
    x$parts %>%
    lapply(function(part) {
      Element(
        "score-part",
        Element("part-name", part$name),
        list(id = paste0("P", part$number))
      )
    }) %>%
    Element("part-list", .)

  # get Elements "part"
  parts <- lapply(x$parts, to_Element, divisions = divisions)

  Element(
    "score-partwise",
    c(list(part_list), parts),
    list(version = "3.1")
  )
}


#' @keywords internal
#' @export
to_Element.Part <- function(x, divisions, ...) {
  Element(
    "part",
    lapply(x$measures, to_Element, divisions = divisions),
    list(id = paste0("P", x$number))
  )
}


#' @keywords internal
#' @export
to_Element.Measure <- function(x, divisions, ...) {
  Element(
    "measure",
    lapply(x$notes, to_Element, divisions = divisions),
    list(number = x$number)
  )
}


#' @keywords internal
#' @export
to_Element.Attributes <- function(x, ...) {
  Element(
    "attributes",
    lapply(x$attributes, to_Element)
  )
}


#' @keywords internal
#' @export
to_Element.Move <- function(x, divisions, ...) {
  contents <- list()

  contents %<>% c(list(Element("duration", x$duration * divisions)))

  # it seems that voice and staff can be omitted in forward,
  # add them for now anyway

  # voice should come before staff, or there will be an error in MuseScore:
  # "Element voice is not defined in this scope"
  voice <- x$voice
  if (!is.null(voice)) {
    contents %<>% c(list(Element("voice", voice)))
  }

  staff <- x$staff
  if (!is.null(staff)) {
    contents %<>% c(list(Element("staff", staff)))
  }

  Element(x$direction, contents)
}


#' @keywords internal
#' @export
to_Element.Rest <- function(x, divisions, ...) {
  contents <- list(
    Element("rest", attributes = list(measure = "yes")),
    Element("duration", x$duration * divisions),
    Element("voice", x$voice),
    Element("staff", x$staff)
  )

  Element("note", contents)
}


#' @keywords internal
#' @export
to_Element.Note <- function(x, divisions, ...) {
  # unpack
  pitch <- x$pitch
  duration <- x$duration

  contents <- list()
  notations <- list()

  # the order of adding Elements to `contents` can not be changed,
  # or there will be "* is not defined in this scope" error in MuseScore

  # add Element "chord"
  if (isTRUE(x$chord)) {
    contents %<>% c(list(Element("chord")))
  }

  # add Element "rest" or "pitch"
  contents %<>% c(list(to_Element(pitch)))

  # add Element "duration"
  contents %<>% c(list(Element("duration", to_value(duration) * divisions)))

  # add Elements "tie" and "tied"
  if (class(pitch) == "Pitch") {
    # stop
    if (isTRUE(pitch$tie_stop)) {
      contents %<>% c(list(Element("tie", NULL, list(type = "stop"))))
      notations %<>% c(list(Element("tied", NULL, list(type = "stop"))))
    }

    # start
    if (isTRUE(pitch$tie_start)) {
      contents %<>% c(list(Element("tie", NULL, list(type = "start"))))
      notations %<>% c(list(Element("tied", NULL, list(type = "start"))))
    }
  }

  # add Element "voice"
  voice <- x$voice
  if (!is.null(voice)) {
    contents %<>% c(list(Element("voice", voice)))
  }

  # add Element "type"
  contents %<>% c(list(to_Element_type(duration)))

  # add Elements "dot"
  contents %<>% c(to_Elements_dot(duration))

  # add Element "time-modification"
  time_modification <- to_Element_time_modification(duration)
  if (!is.null(time_modification)) {
    contents %<>% c(list(time_modification))
  }

  # add Elements "tuplet"
  notations %<>% c(to_Elements_tuplet(duration))

  # add Element "staff"
  staff <- x$staff
  if (!is.null(staff)) {
    contents %<>% c(list(Element("staff", staff)))
  }

  # add Element "notations"
  if (length(notations) != 0) {
    contents %<>% c(list(Element("notations", notations)))
  }

  attributes <- NULL
  if (isTRUE(x$invisible)) {
    attributes <- list(`print-object` = "no")
  }

  Element("note", contents, attributes)
}



# Music -> MusicXML -------------------------------------------------------

to_musicxml <- function(music) {
  # the Music must contain some Line
  check_music_lines(music$lines)

  # the Music must have a Meter at bar 1
  check_music_meter_line(music$meter_line)

  # normalize `$bar` and `$offset` of each Line,
  # to make `$offset` smaller than the length of Measure `$bar`
  music$lines %<>% normalize_bar_offset.lines(music$meter_line$meters)

  # check if there is any tuplet group crossing barline
  check_tuplet_group_over_bar(music$lines, music$meter_line$meters)

  # normalize `$key_lines` of the Music
  music$key_lines %<>% normalize_key_lines()

  # convert any PitchNotation/Value in the Music to Pitch
  music %<>% to_Pitch()

  # leave marks in tied Pitches in each Line
  music$lines %<>% mark_tie.lines()

  # convert each Line to Measures, and add the result to `$measures`
  music$lines %<>% segment.lines(music$meter_line$meters)

  # append Measures to some Lines,
  # to make all Lines have the same number of Measures
  music$lines %<>% equalize(music$meter_line$meters)

  # normalize `$clef_lines` of the Music
  music$clef_lines %<>%
    normalize_clef_lines(music$lines, music$meter_line$meters)

  # merge the Measures of any staff or voice to its parent part's
  music$lines %<>% merge_lines()

  # merge any Clef to its targeted part
  music$lines %<>%
    merge_clef_lines(music$clef_lines, music$meter_line$meters)

  # merge any Tempo to the first part
  music$lines %<>%
    merge_tempo_line(music$tempo_line, music$meter_line$meters)

  # add Element "staves" to each part
  music$lines %<>% add_staves()

  # merge any Meter to its targeted part
  music$lines %<>% merge_meter_line(music$meter_line$meters)

  # merge any Key to its targeted part
  music$lines %<>% merge_key_lines(music$key_lines)

  # get divisions and add Element "divisions" to each part
  divisions <- get_divisions(music$lines)
  music$lines %<>% add_divisions()

  # split any chord into notes in each part
  music$lines %<>% split_chord()

  # convert the Music to Score
  score <- to_Score(music$lines)

  # generate MusicXML
  print(score, divisions, silent = TRUE)
}


check_music_lines <- function(lines) {
  if (is.null(lines)) {
    general <- "The Music must contain some Line."

    specifics <- c(
      "The Music contains no Line.",
      "Use `+ Line()` to add a Line."
    )

    show_errors(general, specifics)
  }
}


check_music_meter_line <- function(meter_line) {
  general <- "The Music must have a Meter at bar 1."
  specifics <- character(0)

  if (is.null(meter_line)) {
    specifics <- "The Music contains no Meter."
  } else if (meter_line$meters[[1]]$bar != 1) {
    specifics <- "The Music has no Meter at bar 1."
  }

  if (length(specifics) != 0) {
    specifics %<>% c("Use `+ Meter()` to add a Meter.")
    show_errors(general, specifics)
  }
}



# MuseScore ---------------------------------------------------------------

# configure MuseScore:

# open ".Renviron" file with `usethis::edit_r_environ()`

# add the path to MuseScore in it:
# `MUSESCORE_PATH=/your/path/to/musescore`

# check the default paths in various systems:
# https://musescore.org/en/handbook/revert-factory-settings


# call MuseScore to export MusicXML
# `from` and `to` specify file paths
# `...` are other options passed to MuseScore
call_musescore <- function(from, to, ...) {
  # get MuseScore path from ".Renviron" file
  path <- Sys.getenv("MUSESCORE_PATH")

  # infer the path if `MUSESCORE_PATH` is not specified
  if (path == "") {
    # check operating system
    os <- Sys.info()["sysname"]

    if (os == "Darwin") {
      path <- "/Applications/MuseScore\ 3.app/Contents/MacOS/mscore"
    } else if (os == "Windows") {
      path <- "%ProgramFiles%/MuseScore 3/bin/MuseScore3.exe"
    } else {
      path <- "mscore"
    }
  }

  # try calling MuseScore
  tryCatch(
    {system2(path, c(from, "-o", to, ...), stderr = NULL)},
    warning = function(w) abort_musescore()
  )
}


abort_musescore <- function() {
  general <- paste(
    "MuseScore must be installed",
    "to convert Music object to score or audio file."
  )

  specifics <- c(
    "Can't find MuseScore.",
    'See `vignette("mr")` for how to install and configure MuseScore.'
  )

  show_errors(general, specifics)
}



# export MusicXML ---------------------------------------------------------

check_export_dir_path <- function(dir_path) {
  check_type(dir_path, "character")
  check_length(dir_path, 1)

  general <- "`dir_path` must be a path to an existing directory."
  check_content(dir_path, dir.exists, general = general)
}


check_export_formats <- function(formats) {
  check_type(formats, "character")
  check_length(formats, Inf)

  valid <- c(
    # MuseScore
    "mscz", "mscx",
    # graphic
    "pdf", "png", "svg",
    # audio
    "wav", "mp3", "flac", "ogg", "midi", "mid",
    # musicxml
    "musicxml", "mxl", "xml",
    # misc
    "metajson", "mlog", "mpos", "spos"
  )

  s_valid <- valid %>%
    sapply(quote_string) %>%
    coordinate()

  general <- "`formats` must be {s_valid}."
  specifics <- character()

  l <- length(formats)
  fs <- tolower(formats)

  if (l == 1 && !(fs %in% valid)) {
    specifics <- '`formats` is "{formats}".'

  } else if (l > 1) {
    specific <- '`formats[{i}]` is "{format}".'

    for (i in 1:l) {
      format <- formats[i]

      if (!(fs[i] %in% valid)) {
        specifics <- specific %>%
          glue::glue() %>%
          unclass() %>%
          c(specifics, .)
      }
    }
  }

  show_errors(general, specifics, env = environment())
}


normalize_export_formats <- function(formats) {
  formats %>%
    tolower() %>%
    unique()
}


export_musicxml <- function(musicxml, dir_path, file_name, formats,
                            dpi = "") {
  check_export_dir_path(dir_path)
  check_name(file_name)
  check_export_formats(formats)

  dir_path %<>% normalizePath() # remove last "/"(s)
  formats %<>% normalize_export_formats()

  # file path without extension
  name_path <- file.path(dir_path, file_name)

  # export `musicxml` to musicxml file first
  # create musicxml file path
  if ("musicxml" %in% formats) {
    musicxml_path <- paste0(name_path, ".musicxml")
  } else {
    musicxml_path <- tempfile(fileext = ".musicxml")
  }

  writeLines(musicxml, musicxml_path)

  for (format in formats) {
    # skip musicxml file
    if (format == "musicxml") {
      next
    }

    # file name extension
    extension <- paste0(".", format)
    # file path
    file_path <- paste0(name_path, extension)

    # export `musicxml` to non-graphic file directly
    if (!(format %in% c("png", "svg"))) {
      call_musescore(musicxml_path, file_path)
    }

    # MuseScore splits long graphic files,
    # so export `musicxml` to temporary dir first,
    # then combine split parts

    # create temporary path
    tmp_path <- tempfile(fileext = extension)
    # export `musicxml` to the temporary path
    call_musescore(musicxml_path, tmp_path, "-T 20", dpi)

    # there may be split graphic files in the temporary dir now,
    # check and combine them

    # names of all files in the temporary dir
    tmp_files <- list.files(tempdir(), full.names = TRUE)
    # the paths to all exported graphic files
    graphic_paths <-
      # get the file name part (no extension) of `tmp_path`
      tools::file_path_sans_ext(basename(tmp_path)) %>%
      # generate a regular expression
      paste0(".*\\.", format, "$") %>%
      # get the paths of all exported graphic files
      {tmp_files[grep(., tmp_files)]}

    l <- length(graphic_paths)
    # just copy and rename one-part graphic file
    if (l == 1) {
      file.copy(graphic_paths, file_path)
    # combine graphic file parts
    } else {
      graphic_paths %>%
        magick::image_read() %>%
        magick::image_append(stack = TRUE) %>%
        magick::image_write(path = file_path, format = format)
    }

    # remove temporary graphic files
    unlink(graphic_paths)
  }

  # remove temporary musicxml file
  if (!("musicxml" %in% formats)) {
    unlink(musicxml_path)
  }
}



# show MusicXML -----------------------------------------------------------

check_show_to <- function(to) {
  if (is.null(to)) {
    return()
  }

  check_type(to, "character")
  check_length(to, 1:2)

  # check content
  valid <- c("score", "audio")
  general <- '`to` must be "score", "audio" or both, if specified.'
  specifics <- character(0)
  l <- length(to)

  # the wording is more nuanced, don't merge this clause
  if (l == 1) {
    check_content(to, valid, general = general)

  } else {
    for (i in 1:l) {
      to_i <- to[[i]]
      if (!(to_i %in% valid)) {
        specifics[length(specifics) + 1] <-
          '`to[{i}]` is "{to_i}."' %>%
          glue::glue() %>%
          unclass()
      }
    }

    show_errors(general, specifics)
  }
}


normalize_show_to <- function(to) {
  if (is.null(to)) {
    return("png")
  }

  for (i in 1:length(to)) {
    to[i] %<>% switch(
      "score" = "png",
      "audio" = "mp3"
    )
  }

  to
}


# check if mr is used in R Jupyter Notebook
is_jupyter <- function() {
  f <- getOption("jupyter.base_display_func")

  if (is.function(f)) {
    tryCatch(
      {f()},
      error = function(e) TRUE,
      warning = function(w) FALSE
    )

  } else {
    FALSE
  }
}


# figure out the context in which `show_musicxml` is called,
# to tell it is R Markdown, RStudio, or normal R console
# refs:
# https://stackoverflow.com/questions/33107908/
# how-to-tell-if-code-is-executed-within-a-knitr-rmarkdown-context
# https://stackoverflow.com/questions/12389158/
# check-if-r-is-running-in-rstudio
get_show_context <- function() {
  # check if it is R Markdown first
  # if you put this clause and the second into an R Markdown file,
  # then call `knitr::knit()` on that file from RStudio,
  # both clauses will be TRUE
  if (isTRUE(getOption('knitr.in.progress'))) {
    "rmd"
  } else if (rstudioapi::isAvailable()) {
    "rstudio"
  } else if (is_jupyter()) {
    "jupyter"
  } else {
    "other"
  }
}


show_musicxml <- function(musicxml, to) {
  check_show_to(to)
  to %<>% normalize_show_to()

  name_path <- tempfile()
  dir_path <- dirname(name_path)
  file_name <- basename(name_path)

  export_musicxml(musicxml, dir_path, file_name, to, "-r 115")

  context <- get_show_context()

  # generate HTML object(s) to show
  contents <- list()

  for (format in to) {
    file_path <- paste0(name_path, ".", format)

    # use relative path
    if (context != "rmd") {
      file_path %<>% basename()
    }

    if (format == "mp3") {
      html_object <- file_path %>%
        {htmltools::tags$source(src = ., type = "audio/mp3")} %>%
        htmltools::tags$audio(controls = NA, .) %>%
        htmltools::tags$p()

    } else if (format == "png") {
      html_object <- file_path %>%
        {htmltools::tags$img(src = ., style = "max-width: 100%;")} %>%
        htmltools::tags$p()
    }

    contents %<>% c(list(html_object))
  }

  if (length(contents) == 1) {
    contents %<>% .[[1]]
  } else {
    contents %<>% do.call(htmltools::tags$div, .)
  }

  # show file(s)
  if (context == "rmd") {
    return(contents)
  }

  html_template <- paste(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    '<meta charset="utf-8">',
    "</head>",
    "<body>",
    "{{{ content }}}",
    "</body>",
    "</html>",
    sep = "\n"
  )

  html_path <- paste0(name_path, ".", "html")

  # export `contents` to HTML file
  writeLines(
    whisker::whisker.render(html_template, list(content = contents)),
    html_path
  )

  # open the HTML file
  if (context == "rstudio") {
    rstudioapi::viewer(html_path)
  } else if (context == "other") {
    utils::browseURL(html_path)
  }
}



# show/export Music -------------------------------------------------------

#' @export
show.Music <- function(x, to = NULL, ...) {
  x %>%
    to_musicxml() %>%
    show_musicxml(to)
}


#' @export
export.Music <- function(x, dir_path, file_name, formats, ...) {
  x %>%
    to_musicxml() %>%
    export_musicxml(dir_path, file_name, formats)
}
