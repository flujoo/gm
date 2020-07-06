library(mr)


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


test_that("convert Element to MusicXML", {
  out <- unclass(to_MusicXML.Element(measure))
  expected <- paste0(
    "<measure>\n",
    '  <note default-x="10">\n',
    "    <pitch>\n",
    "      <step>G</step>\n",
    "      <octave>4</octave>\n",
    "    </pitch>\n",
    "    <chord/>\n",
    "    <duration>16</duration>\n",
    "  </note>\n",
    "</measure>\n"
  )
  expect_equal(out, expected)
})
