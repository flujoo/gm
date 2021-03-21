library(mr)


test_that("export", {
  name_path <- tempfile()
  dir_path <- dirname(name_path)
  name <- basename(name_path)
  formats <- c("musicxml", "mp3", "png")

  m <- Music() + Meter(4, 4) + Line(list(90), list(4))

  tryCatch(
    {export(m, dir_path, name, formats)},
    error = function(e) skip("MuseScore not available")
  )

  files <- list.files(dir_path)

  for (format in formats) {
    file_name <- paste0(name, ".", format)
    expect_true(file_name %in% files)
  }
})
