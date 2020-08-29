#' @title Round Number to Nearest
#' @details See \url{https://stackoverflow.com/questions/9508518/
#' why-are-these-numbers-not-equal}.
round_ <- function(x) {
  sapply(x, function(x) {
    i <- 0
    while (!identical(all.equal(round(x, i), x), TRUE)) {
      i <- i + 1
    }
    round(x, i)
  })
}
