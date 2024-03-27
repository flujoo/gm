#' @keywords internal
#' @export
to_MusicXML.Duration <- function(x, ...) {
  depth <- length(x[["ratios"]])
  if (depth == 0) return(to_MusicXML_non_tuplet(x))

  tuplet <- complete_tuplet(x)
  ratios <- tuplet[["ratios"]]

  last_take <- ratios[[depth]][["take"]]
  musicxml_type <- MusicXML("type", last_take[["type"]])
  musicxml_dots <- rep(list(MusicXML("dot")), last_take[["dot"]])

  actual_normal_pairs <- get_actual_normal_pairs(tuplet)
}


to_MusicXML_non_tuplet <- function(duration) {
  musicxml_type <- MusicXML("type", duration[["type"]])
  musicxml_dots <- rep(list(MusicXML("dot")), duration[["dot"]])

  c(list(musicxml_type), musicxml_dots)
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
