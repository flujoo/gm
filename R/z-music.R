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
    object = list(),
    notation = character()
  )

  # create Music
  list(
    lines = lines,
    notes = notes,
    local = local,
    global = global,
    meta = meta
  ) %>% `class<-`("Music")
}


#' @export
`+.Music` <- function(music, object) {
  cs <- c("Line", "Meter", "Key", "Clef", "Tempo", "Tie", "Meta")
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
  cat("Music\n\n")

  cat("* of lines:\n\n")
  print(x$lines)
  cat("\n")

  cat("* of notes:\n\n")
  print(x$notes)
  cat("\n")

  cat("* of objects added to notes:\n\n")
  print(x$local)
  cat("\n")

  cat("* of global objects:\n\n")
  print(x$global)
  cat("\n")

  cat("* of meta-information:\n\n")
  print(x$meta)
}
