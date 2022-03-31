library(tidyverse)
library(reactablefmtr)

# load bee colony stressor dataset from tidy tuesday
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

stressor %>%
  filter(year == 2020 & state %in% c('United States','California','Georgia','Texas','Idaho')) %>%
  group_by(state, stressor) %>%
  summarize(stress_pct = mean(stress_pct, na.rm = TRUE)) %>%
  pivot_wider(names_from = stressor, values_from = stress_pct) %>%
  select(c(State = state, Diseases = Disesases, Pesticides, `Other pests/parasites`, Other, Unknown)) %>%
  reactable(
    theme = no_lines(centered = TRUE),
    defaultColDef = colDef(align = 'center'),
    columns = list(
      Diseases = colDef(
        cell = bubble_grid(
          data = .,
          colors = '#084C61',
          number_fmt = scales::number,
          min_value = 0,
          max_value = 15
        )
      ),
      Pesticides = colDef(
        cell = bubble_grid(
          data = .,
          colors = '#DB504A',
          number_fmt = scales::number,
          min_value = 0,
          max_value = 15
        )
      ),
      `Other pests/parasites` = colDef(
        cell = bubble_grid(
          data = .,
          colors = '#E3B505',
          number_fmt = scales::number,
          min_value = 0,
          max_value = 15
        )
      ),
      Other = colDef(
        cell = bubble_grid(
          data = .,
          colors = '#4F6D7A',
          number_fmt = scales::number,
          min_value = 0,
          max_value = 15
        )
      ),
      Unknown = colDef(
        cell = bubble_grid(
          data = .,
          colors = '#56A3A6',
          number_fmt = scales::number,
          min_value = 0,
          max_value = 15
        )
      )
    )
  ) %>%
  add_title(html("Bee Colony Health Stressors <img src='https://svgsilh.com/svg/30649.svg' alt='Bee' width='40' height='40'>"),
                 margin = reactablefmtr::margin(t=0,r=0,b=3,l=0)) %>%
  add_subtitle("% of colonies affected by stressors during 2020",
               font_weight = 'normal',
               font_size = 20,
               margin = reactablefmtr::margin(t=0,r=0,b=6,l=0)) %>%
  add_source("Data: USDA, #TidyTuesday Week 2, 2022",
             margin = reactablefmtr::margin(t=7,r=0,b=0,l=0),
             font_style = "italic")
