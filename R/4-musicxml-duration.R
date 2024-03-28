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
  musicxml_duration <- c(list(musicxml_type), musicxml_dots)

  list(
    duration = musicxml_duration,
    tuplet = list()
  )
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


#' @description `<normal-type>` and `<normal-dot>` elements represent
#' the deepest unit of a tuplet.
#'
#' @noRd
to_MusicXML_time_modification <- function(actual_normal_pairs, last_unit) {
  musicxml_actual_notes <- MusicXML(
    "actual-notes",
    prod(sapply(actual_normal_pairs, function(pair) pair[["actual"]]))
  )

  musicxml_normal_notes <- MusicXML(
    "normal-notes",
    prod(sapply(actual_normal_pairs, function(pair) pair[["normal"]]))
  )

  musicxml_normal_type <- MusicXML("normal-type", last_unit[["type"]])

  musicxml_normal_dots <- rep(
    list(MusicXML("normal-dot")),
    last_unit[["dot"]]
  )

  contents <- c(
    list(musicxml_actual_notes, musicxml_normal_notes, musicxml_normal_type),
    musicxml_normal_dots
  )

  MusicXML("time-modification", contents)
}
