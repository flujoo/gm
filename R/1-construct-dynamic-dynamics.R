# according to MuseScore
dynamics <- rbind(
  data.frame(marking = character(), velocity = integer()),

  list(marking = "pppppp", velocity = 1L  ),
  list(marking = "ppppp" , velocity = 5L  ),
  list(marking = "pppp"  , velocity = 10L ),
  list(marking = "ppp"   , velocity = 16L ),
  list(marking = "pp"    , velocity = 33L ),
  list(marking = "p"     , velocity = 49L ),

  list(marking = "mp"    , velocity = 64L ),
  list(marking = "mf"    , velocity = 80L ),

  list(marking = "f"     , velocity = 96L ),
  list(marking = "ff"    , velocity = 112L),
  list(marking = "fff"   , velocity = 126L),
  list(marking = "ffff"  , velocity = 127L),
  list(marking = "fffff" , velocity = 127L),
  list(marking = "ffffff", velocity = 127L),

  list(marking = "fp"    , velocity = 96L ),
  list(marking = "pf"    , velocity = 49L ),
  list(marking = "sf"    , velocity = 112L),
  list(marking = "sfz"   , velocity = 112L),
  list(marking = "sff"   , velocity = 126L),
  list(marking = "sffz"  , velocity = 126L),
  list(marking = "sfp"   , velocity = 112L),
  list(marking = "sfpp"  , velocity = 112L),
  list(marking = "rfz"   , velocity = 112L),
  list(marking = "rf"    , velocity = 112L),
  list(marking = "fz"    , velocity = 112L),

  list(marking = "m"     , velocity = 96L ),
  list(marking = "r"     , velocity = 112L),
  list(marking = "s"     , velocity = 112L),
  list(marking = "z"     , velocity = 80L ),
  list(marking = "n"     , velocity = 49L )
)
