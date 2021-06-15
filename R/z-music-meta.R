Meta <- function(class, content, ...) {
  erify::check_string(content)

  args <- list(...)
  ns <- names(args)

  if ("size" %in% ns) {
    size <- args$size

    if (!is.null(size)) {
      erify::check_positive(size)
    }
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

  # initialize `meta`
  if (is.null(meta)) {
    meta <- tibble::tibble(
      object = list(),
      notation = character()
    )
  }

  # remove the object in `meta` which has the same classes with `object`
  for (i in seq_len(nrow(meta))) {
    if (isa(meta$object[[i]], class(object))) {
      meta <- meta[-i, ]
      break
    }
  }

  music$meta <- tibble::add_case(
    meta,
    object = list(object),
    notation = !!object$content
  )

  music
}
