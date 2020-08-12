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
  list(numerator = n, denominator = d)
}


#' @title Create Time Signature Object
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
