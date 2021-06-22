library(tidyverse)
library(htmltools)
library(reactablefmtr)

# read in data
parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')

# remove dollar sign and convert to numeric
parks_df <- parks %>% 
  filter(year == 2020) %>% 
  mutate(spend_per_resident_data = parse_number(spend_per_resident_data)) %>% 
  select(city, spend_per_resident_data)

# national median spend/resident for 2020
parks_df %>% 
  summarize(median_spend = median(spend_per_resident_data))

# top 10 spend/resident
top10 <- parks_df %>% 
  top_n(10, spend_per_resident_data) %>% 
  mutate(spenders = "Top 10 spenders")

# join with bottom 10 spend/resident
parks_df %>% 
  top_n(-10, spend_per_resident_data) %>% 
  mutate(spenders = "Bottom 10 spenders") %>% 
  rbind(top10) %>% 
  mutate(spender_pal = case_when(
    spenders == "Top 10 spenders" ~ "#feb98d",
    spenders == "Bottom 10 spenders" ~ "#000000"
  )) %>% 
  arrange(desc(spend_per_resident_data)) %>% 
  # create table
  reactable(.,
            pagination = FALSE,
            theme = void(cell_padding = 1, header_font_size = 0, font_color = "#000000"),
            columns = list(
              spenders = colDef(show = FALSE),
              spender_pal = colDef(show = FALSE),
              city = colDef(maxWidth = 120, align = "right"),
              spend_per_resident_data = colDef(
                cell = data_bars(., fill_color_ref = "spender_pal", max_value = 450, number_fmt = scales::number_format(prefix = "$"))
              )
            )
  ) %>% 
  add_title(
    font_size = 20,
    "Park Spending Per Resident"
  ) %>% 
  add_subtitle(
    font_size = 16, font_weight = "normal",
    tags$div("Those who spent the most and least out of the 100 most populated cities. The national median is $89 per capita as of 2020.",
    tags$div(htmltools::tagAppendAttributes(shiny::icon("square"), style = "color: #000000; font-size: 12px"), 'Bottom 10 spenders',
             htmltools::tagAppendAttributes(shiny::icon("square"), style = "color: #feb98d; font-size: 12px"), 'Top 10 spenders'))
    ) %>% 
  add_source(
    font_size = 14, margin = 25,
    "Data: The Trust for Public Land | Original Viz: Bloomberg | Replicated Viz: {reactablefmtr}"
    ) 
