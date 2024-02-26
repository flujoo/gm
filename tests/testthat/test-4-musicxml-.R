test_that("conversion of MusicXML to string works", {
  octave <- MusicXML("octave", 4)
  chord <- MusicXML("chord")
  pitch <- MusicXML("pitch", list(MusicXML("step", "G"), octave))

  note <- MusicXML(
    "note",
    list(pitch, chord, MusicXML("duration", 16)),
    list("default-x" = "10")
  )

  measure <- MusicXML("measure", note)

  out <- to_string(measure)

  expected <- paste(
    "<measure>",
    '  <note default-x="10">',
    "    <pitch>",
    "      <step>G</step>",
    "      <octave>4</octave>",
    "    </pitch>",
    "    <chord/>",
    "    <duration>16</duration>",
    "  </note>",
    "</measure>",
    sep = "\n"
  )

  expect_identical(out, expected)
})
