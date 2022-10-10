duration_types <- data.frame(
  name = c(
    "maxima", "long", "breve", "whole", "half", "quarter", "eighth",
    "16th", "32nd", "64th", "128th", "256th", "512th", "1024th"
  ),
  abbr = c(
    "m", "l", "b", "w", "h", "q", "8", "16", "32", "64", "128", "256",
    "512", "1024"
  ),
  value = 2^(6 - 1:14)
)
