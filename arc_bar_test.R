library(ggplot2)
library(ggforce)
pie <- data.frame(
  state = c(
    '1 maandag',
    '2 dinsdag',
    '3 woensdag',
    '4 donderdag',
    '5 vrijdag',
    '6 zaterdag',
    '7 zondag'
  ),
  amount = c(1, 1, 1, 0.5, 1.5, 1, 1)
  # stringsAsFactors = FALSE
)

p <- ggplot() + theme_no_axes() + coord_fixed()

# draw 2 arcs
p + geom_arc_bar(
  aes(
    x0 = 0,
    y0 = 0,
    r0 = 1,
    r = 1.1,
    amount = amount,
    fill = state
  ),
  data = pie,
  stat = 'pie'
) +
  geom_arc_bar(
    aes(
      x0 = 0,
      y0 = 0,
      r0 = 0.2,
      r = 0.3,
      amount = amount,
      fill = state
    ),
    data = pie,
    stat = 'pie'
  )  
