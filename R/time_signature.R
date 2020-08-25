#' @title Validate Time Signature Notations
#' @return A logical vector.
validate.time_signatures <- function(time_signatures) {
  reg <- paste0(
    "^",
    # numerator
    # accept additive time signature
    "([1-9][0-9]*(\\+[1-9][0-9]*)*)",
    "/",
    # denominator
    "(", paste(2^(0:6), collapse = "|"), ")",
    "$"
  )
  grepl(reg, time_signatures)
}


#' @title Split Time Signature Notation
#' @description Split a time signature notation into two parts, with
#' \code{"numerator"} and \code{"denominator"} as names.
analyze.time_signature <- function(time_signature) {
  ts_ <- strsplit(time_signature, "/")[[1]]
  n <- strsplit(ts_[1], "[+]")[[1]]
  d <- ts_[2]
  list(numerator = as.integer(n), denominator = as.integer(d))
}


#' @title Create TimeSignature Object
#' @export
TimeSignature <- function(time_signature) {
  if (!validate.time_signatures(time_signature)) {
    stop("invalid time signature notation")
  }
  ts_ <- analyze.time_signature(time_signature)
  class(ts_) <- "TimeSignature"
  ts_
}


#' @export
print.TimeSignature <- function(x, ...) {
  n <- paste(x$numerator, collapse = "+")
  d <- x$denominator
  s <- paste0(n, "/", d)
  cat(s, "\n")
  invisible(s)
}


#' @title Get Value of Denominator Part of TimeSignature Object
to_value.denominator <- function(denominator) {
  2 ^ (2 - log2(denominator))
}


#' @title Get Value of TimeSignature Object
to_value.TimeSignature <- function(time_signature) {
  v_d <- to_value.denominator(time_signature$denominator)
  v_n <- sum(time_signature$numerator)
  v_d * v_n
}


#' @title Convert TimeSignature to Element "Time"
#' @details MusicXML element "beats" and "beat-type" have no attribute.
#' See \url{https://usermanuals.musicxml.com/MusicXML/Content/
#' EL-MusicXML-time.htm}.
to_Element.TimeSignature <- function(time_signature, attribute) {
  content <- list(
    Element("beats", paste(time_signature$numerator, collapse = "+")),
    Element("beat-type", time_signature$denominator)
  )
  Element("time", content, attribute)
}
