#' @export
Music <- function() {
  # initialize `$lines`
  lines <- tibble::tibble(
    name = character(),
    part = integer(),
    staff = integer(),
    voice = integer(),
    segment = integer(),
    bar = integer(),
    offset = double()
  )

  # initialize `$notes`
  notes <- tibble::tibble(
    line = integer(),
    i = integer(),
    j = integer(),
    pitch = list(),
    pn = character(),
    pv = integer(),
    duration = list(),
    dn = character(),
    dv = double()
  )

  # initialize `$local`
  local <- tibble::tibble(
    line = integer(),
    i = integer(),
    j = integer(),
    object = list(),
    notation = character(),
    value = double()
  )

  # initialize `$global`
  global <- tibble::tibble(
    object = list(),
    notation = character(),
    value = double(),
    bar = integer(),
    offset = double(),
    line = integer(),
    scope = character()
  )

  # initialize `$meta`
  meta <- tibble::tibble(
    item = character(),
    content = character()
  )

  # create Music
  list(
    lines = lines,
    notes = notes,
    global = global,
    local = local,
    meta = meta
  ) %>% `class<-`("Music")
}


#' @export
`+.Music` <- function(music, object) {
  cs <- c("Line", "Meter", "Key", "Clef", "Tempo")
  erify::check_binary_classes(music, object, "Music", cs, "+")

  # normalize argument order
  if (inherits(music, cs) && inherits(object, "Music")) {
    . <- music
    music <- object
    object <- .
  }

  add(object, music)
}


#' @keywords internal
#' @export
add <- function(object, music) {
  UseMethod("add")
}


#' @export
print.Music <- function(x, ...) {
  cat("Music\n")

  # unpack
  lines <- x$lines
  notes <- x$notes
  global <- x$global
  local <- x$local

  # get row number
  l_lines <- nrow(lines) # can indicate `$notes` too
  l_global <- nrow(global)
  l_local <- nrow(local)

  # if to add enter
  if (l_lines != 0 || l_global != 0 || l_local != 0) {
    cat("\n")
  }

  # `$lines` and `$notes`
  if (l_lines != 0) {
    cat("of lines:\n\n")
    print(lines)
    cat("\n")

    cat("of notes:\n\n")
    print(notes)
  }

  # if to add enter
  if (l_global != 0 || l_local != 0) {
    cat("\n")
  }

  # `$global`
  if (l_global != 0) {
    cat("of global objects:\n\n")
    print(global)
  }
}
