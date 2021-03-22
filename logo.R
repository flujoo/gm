library(ggplot2)
library(hexSticker)

p <- ggplot() +
  theme_void() +
  coord_cartesian(ylim = c(-2, 2))

for (i in -2:2) {
  p <- p + geom_hline(yintercept = i)
}

s <- sticker(
  subplot = p,
  s_y = 0.74,
  s_width = 2.5,

  h_color = "black",
  h_fill = "white",

  package = "gm",
  p_color = "black",
  p_size = 15,
  p_y = 1.185,

  filename = "./man/figures/logo.png"
)
