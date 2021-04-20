# MuseScore ---------------------------------------------------------------

# configure MuseScore:

# open ".Renviron" file with `usethis::edit_r_environ()`

# add the path to MuseScore in it:
# `MUSESCORE_PATH=/your/path/to/musescore`

# check the default paths in various systems:
# https://musescore.org/en/handbook/revert-factory-settings


# call MuseScore to export MusicXML
# `from` and `to` specify file paths
# `...` are other options passed to MuseScore
call_musescore <- function(from, to, musescore) {
  # get MuseScore path from ".Renviron" file
  path <- Sys.getenv("MUSESCORE_PATH")

  # infer the path if `MUSESCORE_PATH` is not specified
  if (path == "") {
    # check operating system
    os <- Sys.info()["sysname"]

    if (os == "Darwin") {
      path <- "/Applications/MuseScore\ 3.app/Contents/MacOS/mscore"
    } else if (os == "Windows") {
      path <- "C:/Program Files/MuseScore 3/bin/MuseScore3.exe"
    } else {
      path <- "mscore"
    }
  }

  # try calling MuseScore
  tryCatch(
    {system2(path, c(from, "-o", to, musescore), stderr = NULL)},
    warning = function(w) abort_musescore()
  )
}


abort_musescore <- function() {
  general <- paste(
    "MuseScore must be installed",
    "to convert Music object to score or audio file."
  )

  specifics <- c(
    "Can't find MuseScore.",
    'See `vignette("gm", package = "gm")` for how to configure MuseScore.'
  )

  show_errors(general, specifics)
}



# export MusicXML ---------------------------------------------------------

check_export_dir_path <- function(dir_path) {
  check_type(dir_path, "character")
  check_length(dir_path, 1)

  general <- "`dir_path` must be a path to an existing directory."
  check_content(dir_path, dir.exists, general = general)
}


check_export_formats <- function(formats) {
  check_type(formats, "character")
  check_length(formats, Inf)

  valid <- c(
    # MuseScore
    "mscz", "mscx",
    # graphic
    "pdf", "png", "svg",
    # audio
    "wav", "mp3", "flac", "ogg", "midi", "mid",
    # musicxml
    "musicxml", "mxl", "xml",
    # misc
    "metajson", "mlog", "mpos", "spos"
  )

  s_valid <- valid %>%
    sapply(quote_string) %>%
    coordinate()

  general <- "`formats` must be {s_valid}."
  specifics <- character()

  l <- length(formats)
  fs <- tolower(formats)

  if (l == 1 && !(fs %in% valid)) {
    specifics <- '`formats` is "{formats}".'

  } else if (l > 1) {
    specific <- '`formats[{i}]` is "{format}".'

    for (i in 1:l) {
      format <- formats[i]

      if (!(fs[i] %in% valid)) {
        specifics <- specific %>%
          glue::glue() %>%
          unclass() %>%
          c(specifics, .)
      }
    }
  }

  show_errors(general, specifics, env = environment())
}


normalize_export_formats <- function(formats) {
  formats %>%
    tolower() %>%
    unique()
}


export_musicxml <- function(musicxml, dir_path, file_name, formats,
                            musescore) {
  check_export_dir_path(dir_path)
  check_name(file_name)
  check_export_formats(formats)

  dir_path %<>% normalizePath() # remove last "/"(s)
  formats %<>% normalize_export_formats()

  # file path without extension
  name_path <- file.path(dir_path, file_name)

  # export `musicxml` to musicxml file first
  # create musicxml file path
  if ("musicxml" %in% formats) {
    musicxml_path <- paste0(name_path, ".musicxml")
  } else {
    musicxml_path <- tempfile(fileext = ".musicxml")
  }

  writeLines(musicxml, musicxml_path)

  for (format in formats) {
    # skip musicxml file
    if (format == "musicxml") {
      next
    }

    # file name extension
    extension <- paste0(".", format)
    # file path
    file_path <- paste0(name_path, extension)

    # export `musicxml` to non-graphic file directly
    if (!(format %in% c("png", "svg"))) {
      call_musescore(musicxml_path, file_path, musescore)
      next
    }

    # MuseScore splits long graphic files,
    # so export `musicxml` to temporary dir first,
    # then combine split parts

    # create temporary path
    tmp_path <- tempfile(fileext = extension)
    # export `musicxml` to the temporary path
    call_musescore(musicxml_path, tmp_path, c("-T 0", musescore))

    # there may be split graphic files in the temporary dir now,
    # check and combine them

    # names of all files in the temporary dir
    tmp_files <- list.files(tempdir(), full.names = TRUE)
    # the paths to all exported graphic files
    graphic_paths <-
      # get the file name part (no extension) of `tmp_path`
      strsplit(basename(tmp_path), "[.]")[[1]][1] %>%
      # generate a regular expression
      paste0(".*\\.", format, "$") %>%
      # get the paths of all exported graphic files
      {tmp_files[grep(., tmp_files)]}

    l <- length(graphic_paths)
    # just copy and rename one-part graphic file
    if (l == 1) {
      file.copy(graphic_paths, file_path)
      # combine graphic file parts
    } else {
      graphic_paths %>%
        magick::image_read() %>%
        magick::image_append(stack = TRUE) %>%
        magick::image_write(path = file_path, format = format)
    }

    # remove temporary graphic files
    unlink(graphic_paths)
  }

  # remove temporary musicxml file
  if (!("musicxml" %in% formats)) {
    unlink(musicxml_path)
  }
}



# show MusicXML -----------------------------------------------------------

check_show_to <- function(to) {
  if (is.null(to)) {
    return()
  }

  check_type(to, "character")
  check_length(to, 1:2)

  # check content
  valid <- c("score", "audio")
  general <- '`to` must be "score", "audio" or both, if specified.'
  specifics <- character(0)
  l <- length(to)

  # the wording is more nuanced, don't merge this clause
  if (l == 1) {
    check_content(to, valid, general = general)

  } else {
    for (i in 1:l) {
      to_i <- to[[i]]
      if (!(to_i %in% valid)) {
        specifics[length(specifics) + 1] <-
          '`to[{i}]` is "{to_i}."' %>%
          glue::glue() %>%
          unclass()
      }
    }

    show_errors(general, specifics)
  }
}


normalize_show_to <- function(to) {
  if (is.null(to)) {
    return("png")
  }

  for (i in 1:length(to)) {
    to[i] %<>% switch(
      "score" = "png",
      "audio" = "mp3"
    )
  }

  to
}


# check if gm is used in R Jupyter Notebook
is_jupyter <- function() {
  f <- getOption("jupyter.base_display_func")

  if (is.function(f)) {
    tryCatch(
      {f()},
      error = function(e) TRUE,
      warning = function(w) FALSE
    )

  } else {
    FALSE
  }
}


# figure out the context in which `show_musicxml` is called,
# to tell it is R Markdown, RStudio, or normal R console
# refs:
# https://stackoverflow.com/questions/33107908/
# how-to-tell-if-code-is-executed-within-a-knitr-rmarkdown-context
# https://stackoverflow.com/questions/12389158/
# check-if-r-is-running-in-rstudio
get_show_context <- function() {
  # check if it is R Markdown first
  # if you put this clause and the second into an R Markdown file,
  # then call `knitr::knit()` on that file from RStudio,
  # both clauses will be TRUE
  if (isTRUE(getOption('knitr.in.progress'))) {
    # check if knit to pdf or word
    ifelse(knitr::is_html_output(), "rmd", "rmd_other")
  } else if (rstudioapi::isAvailable()) {
    "rstudio"
  } else if (is_jupyter()) {
    "jupyter"
  } else {
    "other"
  }
}


show_musicxml <- function(musicxml, to, musescore) {
  check_show_to(to)
  to %<>% normalize_show_to()

  name_path <- tempfile()
  dir_path <- dirname(name_path)
  file_name <- basename(name_path)

  export_musicxml(musicxml, dir_path, file_name, to, c("-r 115", musescore))
  context <- get_show_context()

  if (context == "rmd_other" && ("png" %in% to)) {
    name_path %>%
      paste0(".png") %>%
      knitr::include_graphics()

  } else {
    content <- generate_show_content(name_path, to, context)

    if (context %in% c("rmd", "jupyter")) {
      content

    } else {
      html_path <- to_html(content, name_path)

      if (context == "rstudio") {
        rstudioapi::viewer(html_path)

      } else if (context == "other") {
        utils::browseURL(html_path)
      }
    }
  }
}


# write `content` to HTML file and return its path
to_html <- function(content, name_path) {
  html <- paste(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    '<meta charset="utf-8">',
    "</head>",
    "<body>",
    "{content}",
    "</body>",
    "</html>",
    sep = "\n"
  )

  html_path <- paste0(name_path, ".", "html")
  content %<>% as.character()
  html %<>% glue::glue()
  writeLines(html, html_path)
  html_path
}


# generate the content to show in different contexts
generate_show_content <- function(name_path, to, context) {
  content <- list()

  for (format in to) {
    file_path <- generate_file_path(name_path, format, context)

    if (format == "mp3") {
      html_object <- file_path %>%
        {htmltools::tags$source(src = ., type = "audio/mp3")} %>%
        htmltools::tags$audio(controls = NA, .) %>%
        htmltools::tags$p()

    } else if (format == "png") {
      html_object <- file_path %>%
        {htmltools::tags$img(src = ., style = "max-width: 100%;")} %>%
        htmltools::tags$p()
    }

    content %<>% c(list(html_object))
  }

  if (length(content) == 1) {
    content[[1]]
  } else {
    htmltools::tags$div(content)
  }
}


generate_file_path <- function(name_path, format, context) {
  # create absolute path
  file_path <- paste0(name_path, ".", format)

  # use relative path in RStudio and normal R console
  if (context %in% c("rstudio", "other")) {
    file_path %<>% basename()
  }

  # use data URL in R Markdown documents and Jupyter Notebooks
  if (context %in% c("jupyter", "rmd")) {
    file_path %<>% {base64enc::dataURI(file = .)}
  }

  file_path
}



# show/export Music -------------------------------------------------------

#' @title Show Object
#'
#' @description Show an object as musical score or audio file.
#'
#' @param x An object.
#'
#' @param to Optional. A character vector which contains "score", "audio" or
#' both, which indicates whether to show the object as musical score or
#' audio file. The default value is "score".
#'
#' @param musescore Optional. A character vector which represents the
#' command line options passed to MuseScore. See
#' <https://musescore.org/en/handbook/3/command-line-options> for
#' MuseScore command line options. Also see "Examples" section.
#'
#' @return Invisible `NULL`.
#'
#' The generated musical score or audio file is
#'
#' 1. showed in Viewer panel if `show` is called in RStudio,
#'
#' 2. included in generated HTML file if called in R Markdown document,
#'
#' 3. showed in output cell if called in R Jupyter Notebook, and
#'
#' 4. showed in user's browser if called in a normal R console.
#'
#' @seealso <https://musescore.org/en/handbook/3/command-line-options>
#' for MuseScore command line options.
#'
#' @examples
#' if (interactive()) {
#'   m <- Music() + Meter(4, 4) + Line(list("C4"), list(4))
#'   show(m, c("score", "audio"), "-r 800 -T 5")
#' }
#' @export
show <- function(x, to, musescore) {
  UseMethod("show")
}


#' @describeIn show show a `Music` object.
#' @export
show.Music <- function(x, to = NULL, musescore = NULL) {
  x %>%
    to_musicxml() %>%
    show_musicxml(to, musescore)
}


#' @title Export Object
#'
#' @description Export an object to various file formats.
#'
#' @param x An object.
#'
#' @param dir_path A single character which specifies the directory to
#' which to export the object.
#'
#' @param file_name A single character which specifies the name of the
#' exported file(s).
#'
#' @param formats A character vector which specifies the file formats.
#' Supported file formats are "mscz", "mscx", "pdf", "png", "svg", "wav",
#' "mp3", "flac", "ogg", "midi", "mid", "musicxml", "mxl", "xml", "metajson",
#' "mlog", "mpos" and "spos".
#'
#' @param musescore Optional. A character vector which represents the
#' command line options passed to MuseScore. See
#' <https://musescore.org/en/handbook/3/command-line-options> for
#' MuseScore command line options. Also see "Examples" section.
#'
#' @return Invisible `NULL`.
#'
#' Files with name `file_name` and with extensions
#' `formats` are generated in `dir_path`.
#'
#' @seealso <https://musescore.org/en/handbook/3/command-line-options>
#' for MuseScore command line options.
#'
#' @examples
#' if (interactive()) {
#'   m <- Music() + Meter(4, 4) + Line(list("C4"), list(4))
#'   export(m, tempdir(), "x", c("mp3", "png"), "-r 200 -b 520")
#' }
#' @export
export <- function(x, dir_path, file_name, formats, musescore) {
  UseMethod("export")
}


#' @describeIn export export a `Music` object.
#' @export
export.Music <- function(x, dir_path, file_name, formats, musescore = NULL) {
  x %>%
    to_musicxml() %>%
    export_musicxml(dir_path, file_name, formats, musescore)
}
