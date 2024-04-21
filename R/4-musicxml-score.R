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
