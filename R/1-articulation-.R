#' @export
Articulation <- function(name, to, i) {
  # validation
  check_articulation_name(name)
  check_to(to)
  erify::check_n(i)

  # normalization
  name <- articulations$musescore[which(articulations == name, TRUE)[1]]
  i <- as.integer(i)

  # construction
  articulation <- list(to = to, i = i, name = name)
  class(articulation) <- "Articulation"
  articulation
}


check_articulation_name <- function(name) {
  valid <- unique(unlist(articulations))
  valid <- valid[!is.na(valid)]
  erify::check_content(name, valid)
}


#' @export
print.Articulation <- function(x, ...) {
  cat("Articulation", "\n\n")
  cat("*", x$name, "\n")
  print_to_ij(x$to, x$i)
}
