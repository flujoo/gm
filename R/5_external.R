#' @title Invoke MuseScore to Do the Actual Exporting Job
musescore <- function(from, to, ops = "") {

  # invoke MuseScore
  core <- function(ms) {
    system2(ms, c(from, "-o", to, ops), stderr = NULL)
  }

  # try to get MuseScore path from .Renviron
  ms <- Sys.getenv("MUSESCORE_PATH")
  if (ms == "") {
    # guess path
    # https://musescore.org/en/handbook/revert-factory-settings
    os <- Sys.info()["sysname"]
    if (os == "Darwin") {
      ms <- "/Applications/MuseScore\ 3.app/Contents/MacOS/mscore"
    } else if (os == "Windows") {
      ms <- "%ProgramFiles%/MuseScore 3/bin/MuseScore3.exe"
    } else {
      ms <- "mscore"
    }
  }

  # trigger an error if can not get correct path
  tryCatch({core(ms)},
           warning = function(e) {
             m <- paste(
               "Please download MuseScore from",
               "<https://musescore.org/en/download>,",
               'and add the path to it into your ".Renviron" file',
               'by setting "MUSESCORE_PATH=/your/path/to/musescore".',
               'You may open ".Renviron" file',
               "by calling `usethis::edit_r_environ()`."
             )
             message(m)
             stop("can not invoke MuseScore")
           }
  )
}


#' @title Export MusicXML Object to File
export.MusicXML <- function(object, path, more_formats = character()) {
  # all formats
  fs <- tolower(c(tools::file_ext(path), more_formats))
  # supported file formats
  # https://musescore.org/en/handbook/command-line-options
  formats <- c(
    # MuseScore
    "mscz", "mscx",
    # graphic
    "pdf", "png", "svg",
    # audio
    "wav", "mp3", "flac", "ogg",
    # musicxml
    "musicxml", "mxl", "xml",
    # misc
    "mid", "metajson", "mlog", "mpos", "spos"
  )

  # supported file formats
  fs_s <- fs[fs %in% formats]
  # unsupported file formats
  fs_u <- fs[!(fs %in% formats)]
  if (length(fs_s) == 0) {
    stop("unsupported file format: ", paste0(fs_u, collapse = ", "))
  } else if (length(fs_u) > 0) {
    m <- paste0(
      "unsupported file format: ",
      paste0(fs_u, collapse = ", "), "\n",
      "only export to: ",
      paste0(fs_s, collapse = ", ")
    )
    warning(m)
  }

  # graphic
  fs_g <- fs_s[fs_s %in% c("png", "svg")]
  # others
  fs_o <- fs_s[!(fs_s %in% c("png", "svg"))]

  # path without extension
  name_path <- tools::file_path_sans_ext(path)

  # export to musicxml file first
  if ("musicxml" %in% fs_s) {
    musicxml_path <- paste0(name_path, ".musicxml")
  } else {
    musicxml_path <- tempfile(fileext = ".musicxml")
  }
  writeLines(object, musicxml_path)

  # export to non-graphic file(s)
  for (f in fs_o) {
    non_graphic_path <- paste(name_path, f, sep = ".")
    musescore(musicxml_path, non_graphic_path)
  }

  # export to graphic file(s)
  # MuseScore splits long images, so export to tempdir() first,
  # then combine parts
  for (f in fs_g) {
    graphic_path <- paste(name_path, f, sep = ".")
    tmp_path <- tempfile(fileext = paste0(".", f))
    tmp_name <- tools::file_path_sans_ext(basename(tmp_path))
    musescore(musicxml_path, tmp_path, "-T 20")
    # all files in tempdir()
    tmp_files <- list.files(tempdir(), full.names = TRUE)
    # image parts
    ps <- tmp_files[grep(paste0(tmp_name, ".*\\.", f, "$"), tmp_files)]
    l_ps <- length(ps)
    # just copy and rename one-part image
    if (l_ps == 1) {
      file.copy(ps, graphic_path)
      # combine image parts
    } else if (l_ps > 1) {
      magick::image_write(
        magick::image_append(magick::image_read(ps), stack = TRUE),
        path = graphic_path, format = f
      )
    }
    # remove parts
    unlink(ps)
  }

  # remove musicxml file in tempdir()
  if (dirname(musicxml_path) == tempdir()) {
    unlink(musicxml_path)
  }
}


#' @title Show MusicXML as Score and Audio in RStudio, Rmd, or Browser
show.MusicXML <- function(object, to = c("score", "audio"), width = 200) {
  if (is.null(to)) {
    return(invisible(NULL))
  }

  # check if it is called in Rstudio, Rmd or R
  if (rstudioapi::isAvailable()) {
    env <- "rstudio"
  } else if (isTRUE(getOption('knitr.in.progress'))) {
    env <- "rmd"
  } else {
    env <- "other"
  }

  # set path
  if (env == "rmd") {
    # Rmd use relative path
    # get the Rmd file's name
    rmd_name <- tools::file_path_sans_ext(knitr::current_input())
    # generate a dir with the same name but the suffix "_gm"
    dir_name <- paste(rmd_name, "gm", sep = "_")
    # delete existing dir to avoid repeating generating files in it
    if (dir.exists(dir_name)) {
      unlink(dir_name, recursive = TRUE)
      dir.create(dir_name)
    } else {
      dir.create(dir_name)
    }
    # generate random name in case not only one call to show.MusicXML
    name_path <- file.path(dir_name, basename(tempfile()))
  } else {
    name_path <- tempfile()
    html_template <- paste(
      "<!DOCTYPE html>",
      "<html>",
      "<head>",
      '<meta charset="utf-8">',
      "</head>",
      "<body>",
      "{{{ content }}}",
      "</body>",
      "</html>",
      sep = "\n"
    )
  }

  # generate files
  if ("score" %in% to && !("audio" %in% to)) {
    image_path <- paste(name_path, "png", sep = ".")
    export.MusicXML(object, image_path)
  } else if ("audio" %in% to && !("score" %in% to)) {
    audio_path <- paste(name_path, "mp3", sep = ".")
    export.MusicXML(object, audio_path)
  } else {
    image_path <- paste(name_path, "png", sep = ".")
    audio_path <- paste(name_path, "mp3", sep = ".")
    export.MusicXML(object, image_path, "mp3")
  }

  if (env != "rmd") {
    html_path <- paste(name_path, "html", sep = ".")
    # convert absolute paths to relative
    if ("score" %in% to) {
      image_path <- basename(image_path)
    }
    if ("audio" %in% to) {
      audio_path <- basename(audio_path)
    }
  }

  # the content to show
  if ("score" %in% to && !("audio" %in% to)) {
    content <- htmltools::tags$p(
      htmltools::tags$img(src = image_path, width = width)
    )
  } else if ("audio" %in% to && !("score" %in% to)) {
    content <- htmltools::tags$p(htmltools::tags$audio(
      controls = NA,
      htmltools::tags$source(src = audio_path, type = "audio/mp3")
    ))
  } else {
    html_img <- htmltools::tags$p(
      htmltools::tags$img(src = image_path, width = width)
    )
    html_audio <- htmltools::tags$p(htmltools::tags$audio(
      controls = NA,
      htmltools::tags$source(src = audio_path, type = "audio/mp3")
    ))
    content <- htmltools::tags$div(html_img, html_audio)
  }

  # add files
  if (env == "rmd") {
    return(content)
  } else {
    # write to html file
    writeLines(
      whisker::whisker.render(html_template, list(content = content)),
      html_path
    )
    # open html file
    if (env == "rstudio") {
      rstudioapi::viewer(html_path)
    } else if (env == "other") {
      utils::browseURL(html_path)
    }
  }
}


show.musicxml <- function(path, ...) {
  m <- readLines(path)
  show.MusicXML(m, ...)
}
