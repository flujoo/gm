#' Split Duration Value Into Duration Type Values
#' @noRd
untie_duration_value <- function(value) {
  values <- duration_types[["value"]]

  if (value %in% values) return(value)
  if (value < rev(values)[1]) stop()

  head <- values[value > values][1]
  c(untie_duration_value(value - head), head)
}
