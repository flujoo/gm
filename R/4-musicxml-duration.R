to_MusicXML_non_tuplet <- function(duration) {
  musicxml <- list(MusicXML("type", duration[["type"]]))

  dot <- duration[["dot"]]
  if (dot != 0) musicxml <- c(musicxml, rep(list(MusicXML("dot")), dot))

  musicxml
}


get_actual_normal_numbers <- function(tuplet) {
  pairs <- list()
  take <- tuplet

  for (ratio in tuplet[["ratios"]]) {
    normal <- to_value_base(take) / to_value_base(ratio[["unit"]])
    pairs <- c(pairs, list(c(actual = ratio[["n"]], normal = normal)))
    take <- ratio[["take"]]
  }

  pairs
}
