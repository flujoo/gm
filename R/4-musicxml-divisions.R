to_fraction_type <- function(type) {
  fraction <- c(1, 1)

  k <- which(type == duration_types[["name"]])
  value <- duration_types[["value"]][k]

  if (k < 6) {
    fraction[1] <- value

  } else if (k > 6) {
    fraction[2] <- 1 / value
  }

  fraction
}


