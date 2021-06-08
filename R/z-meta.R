Meta <- function(class, content, ...) {
  erify::check_string(content)

  args <- list(...)
  ns <- names(args)

  if ("size" %in% ns) {
    check_meta_size(args$size)
  }

  list(
    content = content,
    ...
  ) %>% `class<-`(c(class, "Meta"))
}


#' @export
Title <- function(content, size = NULL) {
  Meta("Title", content, size = size)
}


#' @export
Subtitle <- function(content, size = NULL) {
  Meta("Subtitle", content, size = size)
}


#' @export
Composer <- function(content, size = NULL) {
  Meta("Composer", content, size = size)
}


#' @export
Lyricist <- function(content, size = NULL) {
  Meta("Lyricist", content, size = size)
}


#' @export
Copyright <- function(content) {
  Meta("Copyright", content)
}


#' @export
print.Meta <- function(x, ...) {
  cat(paste0(class(x)[1], ": ", x$content), "\n")

  # unpack
  size <- x$size

  # if to print each component
  print_size <- !is.null(size)

  # if to add enter
  if (print_size) {
    cat("\n")
  }

  # `$size`
  if (print_size) {
    cat("* to be displayed with font size", size, "\n")
  }
}


#' @keywords internal
#' @export
add.Meta <- function(object, music) {
  meta <- music$meta

  # remove the object in `music$meta` which has the same classes with `object`
  for (i in seq_len(nrow(meta))) {
    if (isa(meta$object[[i]], class(object))) {
      meta <- meta[-i, ]
      break
    }
  }

  notation <- object$content

  music$meta <- tibble::add_case(
    meta,
    object = list(object),
    notation = notation
  )

  music
}


check_meta_size <- function(size) {
  if (is.null(size)) {
    return(invisible())
  }

  general <- "`size` must be a positive number."

  erify::check_type(size, c("double", "integer"), NULL, general)
  erify::check_length(size, 1, NULL, general)

  valid <- expression(!is.na(x) && x > 0)
  erify::check_content(size, valid, NULL, general)
}
