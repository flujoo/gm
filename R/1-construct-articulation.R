#' @export
Articulation <- function(name, i, to = NULL) {
  # Validation
  check_articulation_name(name)
  erify::check_n(i)
  check_to(to)

  # Normalization
  name <- articulations$musescore[which(articulations == name, TRUE)[1]]
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


document_articulations <- function() {
  doc <- ""

  for (i in seq_len(nrow(articulations))) {
    names <- as.character(articulations[i, ])
    names <- unique(names[!is.na(names)])
    names <- paste0('`"', names, '"`')
    doc_i <- sprintf("- %s\n", erify::join(names))
    doc <- paste0(doc, doc_i)
  }

  doc
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
