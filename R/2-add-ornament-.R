update_ornaments <- function(music, ornament) {
  line <- ornament$line
  i <- ornament$i

  # the names of the existing types of ornaments in the Music
  names <- intersect(
    names(music),
    c("trills", "turns", "mordents", "tremolos", "schleifers")
  )

  # remove the ornaments that have the same location
  for (name in names) {
    ornaments <- music[[name]]

    to_remove <- ornaments$line == line & ornaments$i == i

    # only "point" ornaments are dealt with
    if (name %in% c("trills", "tremolos")) {
      to_remove <- to_remove | is.na(ornaments$j)
    }

    if (!any(to_remove)) next

    updated <- ornaments[!to_remove, ]
    if (NROW(updated) == 0) updated <- NULL
    music[[name]] <- updated
  }

  # add the incoming ornament
  name <- paste0(tolower(class(ornament)), "s")
  music[[name]] <- rbind(music[[name]], to_case(ornament))

  music
}
