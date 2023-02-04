duration_types <- rbind(
  data.frame(name = character(), abbr = character(), value = double()),

  list(name = "maxima" , abbr = "m"   , value = 32   ),
  list(name = "long"   , abbr = "l"   , value = 16   ),
  list(name = "breve"  , abbr = "b"   , value = 8    ),
  list(name = "whole"  , abbr = "w"   , value = 4    ),
  list(name = "half"   , abbr = "h"   , value = 2    ),
  list(name = "quarter", abbr = "q"   , value = 1    ),
  list(name = "eighth" , abbr = "8"   , value = 1/2  ),
  list(name = "16th"   , abbr = "16"  , value = 1/4  ),
  list(name = "32nd"   , abbr = "32"  , value = 1/8  ),
  list(name = "64th"   , abbr = "64"  , value = 1/16 ),
  list(name = "128th"  , abbr = "128" , value = 1/32 ),
  list(name = "256th"  , abbr = "256" , value = 1/64 ),
  list(name = "512th"  , abbr = "512" , value = 1/128),
  list(name = "1024th" , abbr = "1024", value = 1/256)
)
