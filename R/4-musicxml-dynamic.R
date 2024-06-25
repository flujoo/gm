#' @details See `to_url("elements/dynamics")`.
#' @keywords internal
#' @export
#' @noRd
to_MusicXML.Dynamic <- function(x, ...) {
  contents <- list()


  # <dynamics> and <direction-type> ----------------------------

  dynamic_tags <- c(
    "p", "pp", "ppp", "pppp", "ppppp", "pppppp",
    "f", "ff", "fff", "ffff", "fffff", "ffffff",
    "mp", "mf",
    "sf", "sfp", "sfpp", "fp", "rf", "rfz",
    "sfz", "sffz", "fz", "n", "pf", "sfzp"
  )

  marking <- x[["marking"]]

  musicxml_dynamics <- if (marking %in% dynamic_tags) {
    MusicXML(marking)

  } else {
    MusicXML("other-dynamics", marking)
  }

  musicxml_direction_type <- MusicXML(
    "direction-type",
    MusicXML("dynamics", musicxml_dynamics)
  )

  contents <- c(contents, list(musicxml_direction_type))


  # <sound> ----------------------------------------------------

  velocity <- x[["velocity"]]

  if (!is.na(velocity)) {
    dynamics <- round(velocity / 90 * 100, 2)
    musicxml_sound <- MusicXML("sound", NULL, list(dynamics = dynamics))
    contents <- c(contents, list(musicxml_sound))
  }


  # `placement` ------------------------------------------------

  attributes <- list(placement = if (x[["above"]]) "above" else "below")


  # <direction> ------------------------------------------------

  MusicXML("direction", contents, attributes)
}


#' @keywords internal
#' @export
insert.Dynamic <- function(x, to, ...) {
  insert_direction(x, to, "first")
}
