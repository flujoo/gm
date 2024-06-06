to_url <- function(path) {
  paste0(
    "https://w3c.github.io/musicxml/musicxml-reference/",
    path
  )
}


document_data_frame <- function(df) {
  doc <- ""

  for (i in seq_len(nrow(df))) {
    names <- as.character(df[i, ])
    names <- unique(names[!is.na(names)])
    names <- paste0('"', names, '"')
    doc_i <- sprintf("- %s\n", erify::join(names))
    doc <- paste0(doc, doc_i)
  }

  doc
}


#' @keywords internal
#' @export
to_string <- function(x, ...) {
  UseMethod("to_string")
}


#' @keywords internal
#' @export
to_value <- function(x) {
  UseMethod("to_value")
}


data_frame <- if (requireNamespace("tibble", quietly = TRUE)) {
  tibble::tibble

} else {
  data.frame
}
