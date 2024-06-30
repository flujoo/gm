to_MusicXML_score <- function(lines, notes, meters, divisions) {
  contents <- list()

  musicxml_part_list <- to_MusicXML_part_list(lines)
  contents <- c(contents, list(musicxml_part_list))

  part_numbers <- notes[["part"]]

  for (part_number in sort(unique(part_numbers))) {
    part <- notes[part_numbers == part_number, ]
    musicxml_part <- to_MusicXML_part(part, meters, divisions)
    contents <- c(contents, list(musicxml_part))
  }

  MusicXML("score-partwise", contents)
}


to_MusicXML_part <- function(part, meters, divisions) {
  contents <- list()

  measure_numbers <- part[["start_bar"]]
  last_measure_number <- max(measure_numbers)

  for (measure_number in sort(unique(measure_numbers))) {
    measure <- part[measure_numbers == measure_number, ]
    musicxml_measure <- to_MusicXML_measure(measure, meters, divisions)

    if (measure_number == 1) {
      staves <- length(unique(part[["staff"]]))
      attributes <- Attributes(divisions = divisions, staves = staves)
      musicxml_measure <- insert(attributes, musicxml_measure)
    }

    if (measure_number == last_measure_number) {
      # Insert a Barline
    }

    contents <- c(contents, list(musicxml_measure))
  }

  id <- paste0("P", part[["part"]][1])
  musicxml <- MusicXML("part", contents, list(id = id))

  # Retain line numbers for inserting components
  musicxml[["lines"]] <- unique(part[["line"]])

  musicxml
}


to_MusicXML_measure <- function(measure, meters, divisions) {
  contents <- list()

  line_numbers <- measure[["line"]]
  measure_number <- measure[["start_bar"]][1]

  # Find the Meter for the current bar
  k <- find_by_bar(measure_number, meters[["bar"]])
  meter <- meters[k, ]
  measure_length <- to_value(meter)
  measure_duration <- measure_length * divisions

  for (line_number in unique(line_numbers)) {
    line <- measure[line_numbers == line_number, ]

    is_staff <- line[["staff"]][1] > 1
    is_voice <- line[["voice"]][1] > 1

    if (is_staff || is_voice) {
      musicxml_backup <- to_MusicXML_backup(measure_duration)
      contents <- c(contents, list(musicxml_backup))
    }

    if (is_voice) {
      start_offset <- line[1, ][["start_offset"]]

      if (start_offset > 0) {
        musicxml_forward <- to_MusicXML_forward(start_offset * divisions)
        contents <- c(contents, list(musicxml_forward))
      }
    }

    musicxml_notes <- lapply(
      seq_len(NROW(line)),
      function(k) to_MusicXML(line[k, ], divisions)
    )

    contents <- c(contents, musicxml_notes)

    if (is_voice) {
      rest_offset <- measure_length - rev(line[["end_offset"]])[1]

      if (rest_offset > 0) {
        musicxml_forward <- to_MusicXML_forward(rest_offset * divisions)
        contents <- c(contents, list(musicxml_forward))
      }
    }
  }

  musicxml <- MusicXML("measure", contents, list(number = measure_number))

  # Retain `i` for inserting components
  is <- unique(measure[["i"]])
  is <- is[!is.na(is)]
  musicxml[["is"]] <- is

  musicxml
}


to_MusicXML_part_list <- function(lines) {
  contents <- list()
  part_numbers <- lines[["part"]]

  for (part_number in unique(part_numbers)) {
    part <- lines[part_numbers == part_number, ]

    names <- part[["name"]]
    name <- names[!is.na(names)][1]
    if (is.na(name)) name <- ""

    musicxml_score_part <- MusicXML(
      "score-part",
      MusicXML("part-name", name),
      list(id = paste0("P", part_number))
    )

    contents <- c(contents, list(musicxml_score_part))
  }

  MusicXML("part-list", contents)
}
