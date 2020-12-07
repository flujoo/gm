#' @export
Key <- function(accidental_number, bar = 1) {
  check_accidental_number(accidental_number)
  check_n(bar, "bar")

  list(
    accidental_number = accidental_number,
    bar = bar
  ) %>% `class<-`(c("Key", "BarAddOn", "Printable"))
}


check_accidental_number <- function(accidental_number) {
  check_type(
    supplied = accidental_number,
    valid = c("double", "integer"),
    name = "accidental_number"
  )

  check_length(
    supplied = accidental_number,
    valid = 1,
    name = "accidental_number",
    type = "numeric"
  )

  check_content(
    supplied = accidental_number,
    valid = -7:7,
    general = "`accidental_number` must be any integer between -7 and 7."
  )
}


#' @keywords internal
#' @export
to_string.Key <- function(x, ...) {
  x$accidental_number
}
