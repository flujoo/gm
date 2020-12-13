#' @export
Key <- function(key, bar = 1) {
  check_key(key)
  check_n(bar, "bar")

  list(key = key, bar = bar) %>% `class<-`(c("Key", "BarAddOn", "Printable"))
}


check_key <- function(key) {
  check_type(supplied = key, valid = c("double", "integer"), name = "key")
  check_length(supplied = key, valid = 1, name = "key", type = "numeric")

  check_content(
    supplied = key,
    valid = -7:7,
    general = "`key` must be any integer between -7 and 7."
  )
}


#' @keywords internal
#' @export
to_string.Key <- function(x, ...) {
  x$key
}
