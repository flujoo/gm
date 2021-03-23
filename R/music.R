#' @title Initialize `Music` Object
#'
#' @description Initialize a `Music` object.
#'
#' `Music` objects represent whole music pieces.
#'
#' @details A typical workflow with `Music` objects:
#'
#' 1. Initialize an empty `Music` object with [gm::Music()].
#'
#' 2. Add components to it with [gm::+.Music()].
#'
#' 3. Print it, or display it as musical score or audio file with
#' [gm::show()], to check its structure.
#'
#' 4. Keep adding components and checking it until you get what you want.
#'
#' 5. Sometimes you may want to export the final `Music` object with
#' [gm::export()].
#'
#' @return A list with class `Music`.
#'
#' @seealso [gm::+.Music()] for adding components to a `Music` object.
#'
#' [gm::show()] for displaying a `Music` object as musical score and
#' audio file.
#'
#' [gm::export()] for exporting a `Music` object to various file formats.
#'
#' @examples
#' # initialize a Music object
#' Music()
#'
#' # print a Music object to check its structure
#' m <- Music() + Meter(4, 4) + Line(list("C4"), list(4))
#' m
#' @export
Music <- function() {
  list() %>% `class<-`("Music")
}


#' @title Add Component to `Music` Object
#'
#' @description Add a component to a `Music` object.
#'
#' @param music A `Music` object.
#' @param term A `Line`, `Meter`, `Key`, `Clef` or `Tempo` object.
#'
#' @return A list with class `Music`.
#'
#' @seealso [gm::Music()] for initializing a `Music` object.
#'
#' [gm::Line()], [gm::Meter()], [gm::Key()], [gm::Clef()] and [gm::Tempo()]
#' for creating objects of corresponding classes.
#'
#' @examples
#' # initialize a Music object
#' m <- Music()
#'
#' # add a Line object
#' m <- m + Line(list("C4"), list(1))
#' m
#'
#' # add a Meter object
#' m <- m + Meter(4, 4)
#' m
#'
#' # add a Key object
#' m <- m + Key(1)
#' m
#'
#' # add a Clef object
#' m <- m + Clef("G", to = 1)
#' m
#'
#' # add a Tempo object
#' m <- m + Tempo(120)
#' m
#' @export
`+.Music` <- function(music, term) {
  c_l <- class(music)[1]
  c_r <- class(term)[1]
  cs_l <- "Music"
  cs_r <- c("Line", "Meter", "Key", "Clef", "Tempo")

  check_binary_classes(c_l, c_r, cs_l, cs_r)

  # normalize argument order
  if (c_l %in% cs_r && c_r %in% cs_l) {
    . <- music
    music <- term
    term <- .
  }

  add(term, music)
}


#' @keywords internal
#' @export
add <- function(term, music) {
  UseMethod("add")
}


#' @export
print.Music <- function(x, ...) {
  ss <- "Music"

  lines <- x$lines
  if (!is.null(lines)) {
    for (i in 1:length(lines)) {
      ss[[length(ss) + 1]] <- lines[[i]] %>% print("inside", TRUE, i)
    }
  }

  meter_line <- x$meter_line
  if (!is.null(meter_line)) {
    ss[[length(ss) + 1]] <- meter_line %>% print(silent = TRUE)
  }

  key_lines <- x$key_lines
  if (!is.null(key_lines)) {
    ss <- key_lines %>%
      sapply(print, silent = TRUE) %>%
      c(ss, .)
  }

  clef_lines <- x$clef_lines
  if (!is.null(clef_lines)) {
    ss <- clef_lines %>%
      sapply(print, silent = TRUE) %>%
      c(ss, .)
  }

  tempo_line <- x$tempo_line
  if (!is.null(tempo_line)) {
    ss[[length(ss) + 1]] <- tempo_line %>% print(silent = TRUE)
  }

  ss %>%
    paste(collapse = "\n\n") %>%
    cat("\n")
}
