to_MusicXML_score <- function(lines, notes, meters) {
  contents <- list()

  musicxml_part_list <- to_MusicXML_part_list(lines)
  contents <- c(contents, list(musicxml_part_list))

  divisions <- infer_divisions(lines, notes, meters)
  part_numbers <- notes[["part"]]

  for (part_number in sort(unique(part_numbers))) {
    part <- notes[part_numbers == part_number, ]
    musicxml_part <- to_MusicXML_part(part, divisions)
    contents <- c(contents, list(musicxml_part))
  }

  MusicXML("score-partwise", contents)
}


to_MusicXML_part <- function(part, divisions) {
  contents <- list()

  measure_numbers <- part[["start_bar"]]
  last_measure_number <- max(measure_numbers)

  for (measure_number in sort(unique(measure_numbers))) {
    measure <- part[measure_numbers == measure_number, ]
    musicxml_measure <- to_MusicXML_measure(measure)

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
