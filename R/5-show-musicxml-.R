#' @keywords internal
#' @export
#' @noRd
show.MusicXML <- function(x, to, musescore = NULL) {
  context <- getOption("gm.context")
  name_path <- tempfile()
  export(x, paste0(name_path, ".musicxml"), to, c("-r 115", musescore))

  # For Word and PDF files, ignore the audio format
  if (context == "rmd_other" && ("png" %in% to)) {
    # In case MuseScore splits a long image
    paths <- list.files(tempdir(), full.names = TRUE)
    paths <- paths[grepl(name_path, paths)]

    knitr::include_graphics(paths)
    return(invisible())
  }

  content <- generate_show_content(name_path, to, context)
  if (context %in% c("rmd", "jupyter", "shiny")) return(content)

  html_path <- to_html(content, name_path)

  if (context == "rstudio" && requireNamespace("rstudioapi", quietly = TRUE)) {
    rstudioapi::viewer(html_path)

  } else {
    utils::browseURL(html_path)
  }
}


generate_show_content <- function(name_path, to, context) {
  content <- list()

  for (format in to) {
    file_path <- generate_file_path(name_path, format, context)

    if (format == "mp3") {
      html <- htmltools::tags$source(src = file_path, type = "audio/mp3")
      html <- htmltools::tags$audio(controls = NA, html)
      html <- htmltools::p(html)

    } else if (format == "png") {
      html <- htmltools::img(src = file_path, style = "max-width: 100%;")
      html <- htmltools::p(html)
    }

    content <- c(content, list(html))
  }

  if (length(content) == 1) {
    content[[1]]

  } else {
    htmltools::div(content)
  }
}


generate_file_path <- function(name_path, format, context) {
  file_path <- paste0(name_path, ".", format)

  if (context %in% c("rstudio", "other")) {
    file_path <- basename(file_path)
  }

  if (context %in% c("jupyter", "rmd", "shiny")) {
    file_path <- base64enc::dataURI(file = file_path)
  }

  file_path
}


to_html <- function(content, name_path) {
  html <- paste(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    '<meta charset="utf-8">',
    "</head>",
    "<body>",
    "%s",
    "</body>",
    "</html>",
    sep = "\n"
  )

  html_path <- paste0(name_path, ".", "html")
  content <- as.character(content)
  html <- sprintf(html, content)
  writeLines(html, html_path)
  html_path
}
