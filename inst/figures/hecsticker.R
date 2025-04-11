s <- sticker(
  "inst/figures/chives cartoon.png",

  # package name
  package = "chives",
  p_size = 18,
  p_x = 1,
  p_y = 1.55,
  p_color = "#242410",

  # hex border
  h_size = 1,

  #background color
  h_fill = "#A4D65B",
  h_color = "#242410",

  # cartoon
  s_x = 1,
  s_y = .85,
  s_width = .40,
  s_height = .288,

  spotlight = FALSE,
  white_around_sticker = FALSE,
  # export to
  filename = "inst/figures/chives-hexsticker.png"
)

plot(s)
