add_ornament <- function(object, music) {
  to <- object$to
  i <- object$i
  lines <- music$lines
  notes <- music$notes

  # Validation
  check_add_to(to, lines, object)
  line <- normalize_to(to, lines)
  check_i(i, line, notes)
  check_i_rest(object, line, notes)

  # Normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  # Construction
  update_ornaments(music, object)
}


#' @keywords internal
#' @export
add.Mordent <- add_ornament


#' @keywords internal
#' @export
add.Schleifer <- add_ornament


#' @keywords internal
#' @export
add.Tremolo <- add_ornament


#' @keywords internal
#' @export
add.Turn <- add_ornament


update_ornaments <- function(music, ornament) {
  music <- remove_ornaments(music, ornament)

  name <- paste0(tolower(class(ornament)), "s")
  music[[name]] <- rbind(music[[name]], to_case(ornament))

  music
}


remove_ornaments <- function(music, ornament) {
  line <- ornament$line
  i <- ornament$i

  # Existing ornament types in the Music
  names <- intersect(
    names(music),
    c("trills", "turns", "mordents", "tremolos", "schleifers")
  )

  # Remove the ornaments that have the same location
  for (name in names) {
    # Let `update_trills()` do the job
    if (name == "trills" && inherits(ornament, "Trill")) next

    ornaments <- music[[name]]

    is <- ornaments$i
    to_remove <- ornaments$line == line & is == i

    # Only "point" ornaments are dealt with
    if (name == "trills") {
      js <- ornaments$j
      to_remove <- to_remove & (is.na(js) | js == is)
    }

    if (!any(to_remove)) next

    updated <- ornaments[!to_remove, ]
    if (NROW(updated) == 0) updated <- NULL
    music[[name]] <- updated
  }

  music
}
