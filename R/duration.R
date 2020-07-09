split.duration_notation <- function(duration_notation) {
  # split slurred duration notation
  dns <- strsplit(duration_notation, "-")[[1]]

  # split un-slurred duration notation(s)
  type <- c()
  dot <- c()
  tuplet <- c()

  for (dn in dns) {
    # separate tuplet
    typedot_tuplet <- strsplit(dn, "/")[[1]]
    tuplet <- c(tuplet, as.numeric(typedot_tuplet[2]))
    # separate dot from type
    typedot <- typedot_tuplet[1]
    type_dot <- strsplit(typedot, "[.]")[[1]]
    type_ <- type_dot[1]
    type <- c(type, type_)
    dot_ <- substr(typedot, nchar(type_) + 1, nchar(typedot))
    dot <- c(dot, nchar(dot_))
  }

  list(type = type, dot = dot, tuplet = tuplet)
}

