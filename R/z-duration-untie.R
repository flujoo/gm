# untie sum of duration values
untie <- function(value, decreasing = FALSE, approximate = TRUE) {
  value %<>% round_number()

  # all undotted duration values
  values <- duration_types$value

  if (value %in% values) {
    # return `value` if it's one of the duration values
    return(value)

  } else if (value < values[length(values)]) {
    # stop if `value` is smaller than the smallest duration value
    if (approximate) {
      return()
    } else {
      stop()
    }

  } else {
    if (value > values[1]) {
      # when `value` is larger than the largest duration value
      k <- 1

    } else {
      # when `value` falls in between any two adjacent duration values
      ks <- which(values > value)
      k <- ks[length(ks)] + 1
    }

    # untied duration value
    v <- values[k]

    if (decreasing) {
      c(v, untie(value - v, decreasing, approximate))
    } else {
      c(untie(value - v, decreasing, approximate), v)
    }
  }
}


# https://stackoverflow.com/questions/9508518/
# why-are-these-numbers-not-equal
round_number <- function(x) {
  i <- 0

  repeat {
    y <- round(x, digits = i)
    pass <- all.equal(x, y) %>% isTRUE()

    if (pass) {
      return(y)
    }

    i <- i + 1
  }
}
