to_MusicXML_non_tuplet <- function(duration) {
  musicxml <- list(MusicXML("type", duration[["type"]]))

  dot <- duration[["dot"]]
  if (dot != 0) musicxml <- c(musicxml, rep(list(MusicXML("dot")), dot))

  musicxml
}


#' @returns A list of number pairs. The numbers are the contents of
#' the `<tuplet-number>` elements in `<tuplet-actual>` and
#' `<tuplet-normal>` elements.
#'
#' @noRd
get_actual_normal_pairs <- function(tuplet) {
  pairs <- list()
  take <- tuplet

  for (ratio in tuplet[["ratios"]]) {
    actual <- ratio[["n"]]
    normal <- to_value_base(take) / to_value_base(ratio[["unit"]])

    pair <- c(actual = actual, normal = normal)
    pairs <- c(pairs, list(pair))

    take <- ratio[["take"]]
  }

  pairs
}
