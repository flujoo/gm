update_ornaments <- function(music, ornament) {
  music <- remove_ornaments(music, ornament)

  name <- paste0(tolower(class(ornament)), "s")
  music[[name]] <- rbind(music[[name]], to_case(ornament))

  music
}


remove_ornaments <- function(music, ornament) {
  line <- ornament$line
  i <- ornament$i

  # the names of the existing types of ornaments in the Music
  names <- intersect(
    names(music),
    c("trills", "turns", "mordents", "tremolos", "schleifers")
  )

  # remove the ornaments that have the same location
  for (name in names) {
    # let `update_trills()` do the job
    if (name == "trills" && inherits(ornament, "Trill")) next

    ornaments <- music[[name]]

    is <- ornaments$i
    to_remove <- ornaments$line == line & is == i

    # only "point" ornaments are dealt with
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