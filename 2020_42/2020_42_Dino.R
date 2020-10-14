library(tidyverse)
library(gganimate)
library(showtext)

font_add_google(name = "Neuton",
                family = "Neuton")
font_add_google(name = "Lobster",
                family = "Lobster")
showtext_auto()

datasaurus <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv'
  )

datasaurus_summary <- datasaurus %>%
  group_by(dataset) %>%
  summarize(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x = sd(x),
    sd_y = sd(y),
    cor = cor(x, y, method = "pearson")
  )

datasaurus2 <- datasaurus %>%
  inner_join(datasaurus_summary, by = c("dataset" = "dataset"))

p <- ggplot(datasaurus2, aes(x = x, y = y, color = x)) +
  geom_point(size = 1.5, alpha = 0.8) +
  facet_wrap( ~ dataset, strip.position = "top") +
  scale_colour_viridis_c(option = "plasma") +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text = element_text(
      color = "white",
      family = "Neuton",
      size = 11
    ),
    axis.title = element_blank(),
    plot.title = element_text(
      color = "white",
      family = "Lobster",
      size = 40,
      hjust = 0.5,
      margin = margin(10, 0, 0, 0)
    ),
    plot.subtitle = element_text(
      color = "white",
      family = "Neuton",
      size = 14,
      hjust = 0.5,
      margin = margin(10, 5, 25, 5)
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(
      color = "white",
      family = "Neuton",
      size = 12
    ),
    plot.background = element_rect(fill = "#111111"),
    plot.margin = unit(c(1, 2, 2, 2), "lines")
  ) +
  labs(
    title = "Stats Only Reveal Part of the Story",
    subtitle = "Each of the plots below have nearly identical summary statistics, yet they all look significantly different.\nThis is why it's important to always visualize your data when analyzing it!\n
       X Mean: 54.3     |     Y Mean: 47.8     |     X SD: 16.8     |     Y SD: = 26.9     |     Corr.: -0.06"
  )

p2 <- p + geom_point(aes(group = seq_along(x))) +
  transition_reveal(x) +
  ease_aes('linear')

animate(
  p2,
  nframes = 40,
  fps = 5,
  end_pause = 5,
  rewind = TRUE,
  width = 900,
  height = 900,
  type = "cairo"
)

save_animation(last_animation(), file = "dino_tidytuesday.gif")

