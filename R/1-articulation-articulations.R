articulations <- rbind(
  data.frame(musescore = "accent", musicxml = "accent", symbol = ">"),

  c("staccato"       , "staccato"       , ".")          ,
  c("staccatissimo"  , "staccatissimo"  , "'")          ,
  c("tenuto"         , "tenuto"         , "-")          ,
  c("tenuto-staccato", "detached-legato", "-.")         ,
  c("marcato"        , "strong-accent"  , "^")          ,
  c("scoop"          , "scoop"          , NA_character_),
  c("plop"           , "plop"           , NA_character_),
  c("doit"           , "doit"           , NA_character_),
  c("fall"           , "falloff"        , NA_character_),
  c("stress"         , "stress"         , ",")          ,
  c("unstress"       , "unstress"       , "u")          ,
  c("soft accent"    , "soft-accent"    , "<>")
)
