library(mr)


test_that("convert Element to MusicXML", {
  octave <- Element("octave", 4)
  # empty Element
  chord <- Element("chord")
  # two-level Element
  pitch <- Element(
    "pitch",
    list(Element("step", "G"), octave)
  )
  # complex Element
  note <- Element(
    "note",
    list(pitch, chord, Element("duration", 16)),
    list("default-x" = "10")
  )
  measure <- Element("measure", note)

  out <- print(measure, silent = TRUE)
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
  expect_equal(out, expected)
})
