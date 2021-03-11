#' @title Initialize `Music` Object
#'
#' @description Initialize a `Music` object, to which you can add other
#' components with [mr::+.Music()].
#'
#' @return A list with class `Music`.
#'
#' @seealso [mr::+.Music()] for adding other components to a `Music` object.
#'
#' @examples
#' # Initialize a Music
#' m <- Music()
#' m
#'
#' # add other components
#' m + Meter(4, 4) + Line(list("C4"), list(1))
#' @export
Music <- function() {
  list() %>% `class<-`("Music")
}


#' @title Add Component to `Music` Object
#'
#' @description Add a component to a `Music` object.
#'
#' @param music A `music` object.
#' @param term A `Line`, `Meter`, `Key`, `Clef` or `Tempo` object.
#'
#' @return A list with class `Music`.
#'
#' @seealso [mr::Music()] for Initializing a `Music` object.
#'
#' [mr::Line()], [mr::Meter()], [mr::Key()], [mr::Clef()] and [mr::Tempo()]
#' for creating objects of corresponding classes.
#'
#' @examples
#' # Initialize a Music
#' m <- Music()
#' m
#'
#' # add a Line
#' m <- m + Line(list("C4"), list(1))
#' m
#'
#' # add a Meter
#' m <- m + Meter(4, 4)
#' m
#'
#' # add a Key
#' m <- m + Key(1)
#' m
#'
#' # add a Clef
#' m <- m + Clef("G", to = 1)
#' m
#'
#' # add a Tempo
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
