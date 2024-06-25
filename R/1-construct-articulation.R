#' Create `Articulation` Object
#'
#' Create an `Articulation` object to represent an articulation mark.
#'
#' @details Supported articulation names and symbols:
#' `r document_data_frame(articulations)`
#' The names are from
#' [the MusicXML specification](`r to_url("elements/articulations/")`)
#' and MuseScore.
#'
#' @param name A single character, which represents the name or symbol
#' of the articulation. For example, to create a staccato dot, `name` can
#' be `"staccato"` or `"."`, which looks like a staccato. See the
#' *Details* section for supported articulations.
#'
#' @param i A single positive integer, which represents the position
#' of the articulation in a musical line.
#'
#' @param to Optional. A single character or a single positive integer,
#' which indicates the musical line where to add the articulation.
#'
#' @returns A list of class `Articulation`.
#'
#' @seealso [gm::+.Music()] for adding an `Articulation` to
#' a `Music` object.
#'
#' @export
#'
#' @examples
#' # Create a staccato
#' staccato <- Articulation(".", 1)
#' staccato
#'
#' # Add it to a `Music`
#' music <- Music() + Meter(4, 4) + Line(c("C4", "D4")) + staccato
#' music
#'
#' # Generate the music score
#' if (interactive()) {
#'   show(music)
#' }
Articulation <- function(name, i, to = NULL) {
  # Validation
  check_content_df(name, articulations)
  erify::check_n(i)
  check_to(to)

  # Normalization
  name <- articulations[["musicxml"]][which(articulations == name, TRUE)[1]]
  i <- as.integer(i)

  # Construction
  structure(
    list(to = to, i = i, name = name),
    class = "Articulation"
  )
}


check_articulation_name <- function(name) {
  valid <- unique(unlist(articulations))
  valid <- valid[!is.na(valid)]
  erify::check_content(name, valid)
}


articulations <- rbind(
  data.frame(musescore = "accent", musicxml = "accent", symbol = ">"),

  c("staccato"       , "staccato"       , "."          ),
  c("staccatissimo"  , "staccatissimo"  , "'"          ),
  c("tenuto"         , "tenuto"         , "-"          ),
  c("tenuto-staccato", "detached-legato", "-."         ),
  c("marcato"        , "strong-accent"  , "^"          ),
  c("scoop"          , "scoop"          , NA_character_),
  c("plop"           , "plop"           , NA_character_),
  c("doit"           , "doit"           , NA_character_),
  c("fall"           , "falloff"        , NA_character_),
  c("stress"         , "stress"         , ","          ),
  c("unstress"       , "unstress"       , "u"          ),
  c("soft accent"    , "soft-accent"    , "<>"         )
)


#' @export
print.Articulation <- function(x, ...) {
  cat("Articulation", "\n\n")
  cat("*", x$name, "\n")
  print_to_i_j(x$to, x$i)
}
