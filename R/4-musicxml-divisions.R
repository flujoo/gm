get_divisions <- function(durations) {
  fractions <- lapply(durations, to_fraction)
  denominators <- sapply(fractions, function(fraction) fraction[2])
  Reduce(get_lowest_common_multiple, denominators)
}


to_fraction <- function(notation) {
  duration <- to_Duration(notation)
  duration <- complete_tuplet(duration)

  fraction <- to_fraction_base(duration) * Reduce(
    `*`,
    lapply(duration[["ratios"]], to_fraction_ratio),
    c(1, 1)
  )

  fraction / get_greatest_common_divisor(fraction[1], fraction[2])
}


to_fraction_ratio <- function(ratio) {
  to_fraction_base(ratio[["take"]]) *
    rev(to_fraction_base(ratio[["unit"]])) *
    c(1, ratio[["n"]])
}


to_fraction_base <- function(base) {
  to_fraction_type(base[["type"]]) * to_fraction_dot(base[["dot"]])
}


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


get_lowest_common_multiple <- function(a, b) {
  a * b / get_greatest_common_divisor(a, b)
}


get_greatest_common_divisor <- function(a, b) {
  while (b != 0) {
    x <- a
    a <- b
    b <- x %% b
  }

  a
}
