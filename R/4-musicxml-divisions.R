to_fraction_type <- function(type) {
  k <- which(type == duration_types[["name"]])
  value <- duration_types[["value"]][k]

  if (k <= 6) {
    c(value, 1)

  } else {
    c(1, 1/value)
  }
}


to_fraction_dot <- function(dot) {
  c(2^(dot + 1) - 1, 2^dot)
}
