#' @keywords internal
#' @export
#' @noRd
show.MusicXML <- function(x, to, musescore = NULL) {
  context <- getOption("gm.context")

  name_path <- tempfile()
  export(x, paste0(name_path, ".musicxml"), to, c("-r 115", musescore))

  file_paths <- get_show_paths(name_path, to)
  file_paths <- convert_show_paths(file_paths, context)
  if (context == "rmd_other") return(file_paths)

  html <- to_html(file_paths, context)
  if (context %in% c("rmd", "jupyter", "shiny")) return(html)

  show_html(html, name_path, context)
}


#' @description In case MuseScore splits a long image, get the paths
#' of the score and audio file by matching the file name. Then sort the paths
#' according to the order of the given formats.
#'
#' @noRd
get_show_paths <- function(name_path, to) {
  file_paths <- list.files(tempdir(), full.names = TRUE)
  file_paths <- file_paths[grepl(name_path, file_paths)]

  # The MusicXML file is excluded in the meantime
  file_paths <- sapply(
    to,
    function(format) file_paths[grepl(paste0(format, "$"), file_paths)],
    simplify = FALSE
  )

  unlist(file_paths)
}


convert_show_paths <- function(file_paths, context) {
  if (context == "rmd_other") {
    # For Word and PDF files, ignore the audio format
    file_paths <- file_paths[grepl("png$", file_paths)]
    file_paths <- knitr::include_graphics(file_paths)

  } else if (context %in% c("rstudio", "other")) {
    # Because the files will be referred to from an HTML file
    file_paths <- basename(file_paths)
  }

  file_paths
}


to_html <- function(file_paths, context) {
  to_data_url <- context %in% c("jupyter", "rmd", "shiny")
  htmls <- list()

  for (file_path in file_paths) {
    format <- rev(strsplit(file_path, "[.]")[[1]])[1]
    if (to_data_url) file_path <- base64enc::dataURI(file = file_path)

    if (format == "mp3") {
      html <- htmltools::tags$source(src = file_path, type = "audio/mp3")
      html <- htmltools::tags$audio(controls = NA, html)
      html <- htmltools::p(html)

    } else if (format == "png") {
      html <- htmltools::img(src = file_path, style = "max-width: 100%;")
      html <- htmltools::p(html)
    }

    htmls <- c(htmls, list(html))
  }

  if (length(htmls) == 1) htmls[[1]] else htmltools::div(htmls)
}


show_html <- function(html, name_path, context) {
  html <- paste(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    '<meta charset="utf-8">',
    "</head>",
    "<body>",
    as.character(html),
    "</body>",
    "</html>",
    sep = "\n"
  )

  html_path <- paste0(name_path, ".", "html")
  writeLines(html, html_path)

  if (context == "rstudio" && requireNamespace("rstudioapi", quietly = TRUE)) {
    rstudioapi::viewer(html_path)

  } else {
    utils::browseURL(html_path)
  }
}
