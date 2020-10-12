library(extrafont)
extrafont::loadfonts(device="win")
library(tidyverse)
library(ggtext)
library(ggforce)
library(pdftools)
library(cowplot)

tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

avg_seed_wins <- tournament %>% 
  filter(!is.na(seed)) %>% 
  group_by(seed) %>% 
  summarize(expected_wins = mean(tourney_w))

TOURNAMENT_DATA <- tournament %>% 
  mutate(tourney_finish = case_when(
    tourney_finish == "Champ" ~ "Champion",
    tourney_finish == "N2nd" ~ "Runner-up",
    tourney_finish == "NSF" ~ "Final Four",
    tourney_finish == "RF" ~ "Elite Eight",
    tourney_finish == "RSF" ~ "Sweet Sixteen",
    tourney_finish == "2nd" ~ "2nd Round",
    tourney_finish == "1st" ~ "1st Round",
    TRUE ~ "other"
  )) %>% 
  mutate(school = case_when(
    school == "Notre Dame" ~ "Notre\nDame",
    school == "Louisiana Tech" ~ "Louisiana\nTech",
    TRUE ~ school
  ))

### top 5 teams in terms of championship game appearances
most_championship_games <- TOURNAMENT_DATA %>% 
  group_by(school) %>% 
  mutate(championship_games = case_when(
    tourney_finish == "Champion" | tourney_finish == "Runner-up" ~ 1,
    TRUE ~ 0
  )) %>% 
  group_by(school) %>% 
  summarize(total_championship_games = sum(championship_games)) %>% 
  filter(total_championship_games > 0) %>% 
  arrange(desc(total_championship_games)) %>% 
  top_n(5, total_championship_games)

avg_seed_wins <- tournament %>% 
  filter(!is.na(seed)) %>% 
  group_by(seed) %>% 
  summarize(expected_wins = mean(tourney_w))

expectations <- TOURNAMENT_DATA %>% 
  filter(school %in% most_championship_games$school) %>% 
  inner_join(avg_seed_wins, by = c("seed" = "seed")) %>%
  group_by(school) %>%
  summarize(avg_games_played = mean(tourney_w) + mean(tourney_l),
            avg_actual_wins = mean(tourney_w),
            avg_expected_wins = mean(expected_wins)) %>%
  ungroup() %>% 
  mutate(avg_wins_vs_expectations = avg_actual_wins - avg_expected_wins) %>% 
  arrange(desc(avg_wins_vs_expectations))

top_5_avg_expectations <- expectations %>% 
  summarize(top_5_avg = mean(avg_wins_vs_expectations))

team_expectations <- TOURNAMENT_DATA %>% 
  filter(school %in% most_championship_games$school) %>% 
  inner_join(avg_seed_wins, by = c("seed" = "seed")) %>%
  inner_join(expectations, by = c("school" = "school")) %>% 
  mutate(wins_vs_expectations = tourney_w - expected_wins)

team_expectations$top_5_avg <- top_5_avg_expectations$top_5_avg

p <- ggplot(transform(team_expectations,
                 school = factor(school, levels = c("UConn", "Notre\nDame", "Tennessee", "Stanford", "Louisiana\nTech"))),
                 aes(x = year, y = wins_vs_expectations)) +
  facet_grid(school ~ ., switch = "y") +
  geom_hline(aes(yintercept = avg_wins_vs_expectations, color = school), size = 1.3, alpha = 0.8) +
  geom_hline(aes(yintercept = top_5_avg), linetype = "dashed", size = 0.8) +
  geom_line(aes(color = school)) +
  geom_point(size = 4.5, aes(color = school), alpha = 0.6) +
  geom_point(pch=21, fill=NA, size=7, colour=ifelse(team_expectations$tourney_finish == "Champion", "red", NA), stroke=1.5) +
  scale_color_manual(values = c("UConn" = "#0E1A3E",
                                "Notre\nDame" = "#217E51",
                                "Tennessee" = "#FF7D00",
                                "Stanford" = "#9A0D20",
                                "Louisiana\nTech" = "#6FB5EC")) +
  scale_x_continuous(breaks = c(1982,1990,2000,2010,2018),
                     labels = c("1982","1990","2000","2010","2018"),
                     position = "top", expand = (c(-0,0))) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 17, family = "Impact"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        strip.text.y.left = element_blank(),
        panel.grid.major.x = element_line(size = 1.1),
        plot.background = element_rect(fill = "#FFF8E7", color = "transparent")) +
  geom_text(x = 1980, y = 4, aes(label = school, color = school), size = 8, lineheight = 0.8, family = "Impact") +
  geom_text(x = 1980, aes(y = avg_wins_vs_expectations, label = round(avg_wins_vs_expectations,2), color = school), fontface = "bold",
            family = "Lucida Console", vjust = ifelse(team_expectations$avg_wins_vs_expectations > team_expectations$top_5_avg, -0.7, 1.7)) +
  geom_text(x = 1980, aes(y = top_5_avg, label = paste0("(",round(top_5_avg,2),")")), color = "black", fontface = "bold", family = "Lucida Console") +
  coord_cartesian(clip = "off", xlim = c(1982, 2018)) +
  labs(caption = "Visualization by: Kyle Cuilla . Data by: FiveThirtyEight") +
  theme(plot.margin = unit(c(10, 2.5, 1, 8), "lines"),
        plot.caption = element_text(hjust = -0.15, vjust = -1.5, size = 10, family = "Lucida Console"))

tourney_expectations_legend <- team_expectations %>% 
  filter(school == "UConn",
         year >= 1993 & year <= 1997)

p_legend1 <- ggplot(tourney_expectations_legend,
            aes(x = year, y = wins_vs_expectations)) +
  geom_hline(aes(yintercept = avg_wins_vs_expectations, color = school), size = 1.3, alpha = 0.8) +
  geom_hline(aes(yintercept = top_5_avg), linetype = "dashed", size = 0.8) +
  geom_line(aes(color = school)) +
  geom_point(size = 4.5, aes(color = school), alpha = 0.6) +
  geom_point(pch=21, fill=NA, size=7, colour=ifelse(tourney_expectations_legend$tourney_finish == "Champion", "red", NA), stroke=1.5) +
  scale_color_manual(values = c("UConn" = "#0E1A3E")) +
  annotate(geom = "text", x = 1996.25, y = 2.52, label = "= Won Championship", size = 4.5, color = "red", fontface = "bold", family = "Consolas") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#FFF8E7", color = "transparent"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        strip.text.y.left = element_blank()) +
  coord_cartesian(clip = "off", xlim = c(1993, 1997)) +
  theme(plot.margin = unit(c(1, 1, 1, 15), "lines"),
        axis.text.x = element_blank())

p_legend2 <- ggplot(tourney_expectations_legend,
                   aes(x = year, y = wins_vs_expectations)) +
  geom_hline(aes(yintercept = avg_wins_vs_expectations*1.9, color = school), size = 1.3, alpha = 0.8) +
  geom_hline(aes(yintercept = top_5_avg/5), linetype = "dashed", size = 0.8) +
  geom_point(pch=21, fill=NA, size=0, colour=ifelse(tourney_expectations_legend$tourney_finish == "Champion", "#FFF8E7", "#FFF8E7"), stroke=1.5) +
  scale_color_manual(values = c("UConn" = "#0E1A3E")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#FFF8E7", color = "transparent"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        strip.text.y.left = element_blank()) +
  geom_text(x = 1986, aes(y = avg_wins_vs_expectations, label = "Average tourney wins\n over expectation for each\n TEAM", color = school), size = 4, lineheight = 0.9, family = "Consolas",
            vjust = -0.3) +
  geom_text(x = 1986, aes(y = top_5_avg, label = "Average tourney wins\n over expectation for the\n TOP 5 TEAMS"), color = "black", size = 4, lineheight = 0.9, family = "Consolas", vjust = 0.8) +
  coord_cartesian(clip = "off", xlim = c(1990, 1995)) +
  theme(plot.margin = unit(c(0, 1, 1, 15), "lines"),
        axis.text.x = element_blank()) +
  geom_textbox(
    data = tibble(
      x = 1972,
      y = c(1.2, 0),
      label = c(
        "<b style='font-size:22pt'>GREAT EXPECTATIONS</b><br><br>The top five schools that have been to the most NCAA tournmaent championship games since 1982 and their wins vs expectations.<br><br>", 
        "<span style='color:#656565'>Note: Expected wins calculated as the average number of wins by seed since 1982.</span>"),
      v = c(0.6, 1.0)
    ),
    aes(x = x, y = y, label = label, vjust = v),
    width = unit(3.6, "inch"),
    color = "black",
    family = "Consolas",
    lineheight = 0.95,
    size = 4.2,
    fill = NA,
    box.colour = NA,
    hjust = 0
  ) +
  scale_size_area(max_size = 39 / 4, guide = F)

p_combined <- ggdraw(p) +
  draw_plot(p_legend1, x = 0.95, y = 1, width = 0.5, height = .15, hjust = 1, vjust = 1) +
  draw_plot(p_legend2, x = 0.62, y = 1, width = 0.35, height = .18, hjust = 1, vjust = 1)

tourney_seed <- TOURNAMENT_DATA %>% 
  filter(school %in% most_championship_games$school) %>% 
  mutate(seed = case_when(
    seed == 1 ~ "1",
    seed == 2 ~ "2",
    seed == 3 ~ "3",
    seed == 4 ~ "4",
    seed >= 5 ~ "5+"
  )) %>% 
  group_by(school, seed) %>% 
  summarize(n = n()) %>%
  ungroup() %>% 
  add_row(school = "Notre\nDame",
          seed = "3",
          n = 0)

tourney_results <- TOURNAMENT_DATA %>% 
  filter(school %in% most_championship_games$school) %>% 
  group_by(school, tourney_finish) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  add_row(school = "UConn",
          tourney_finish = "Runner-up",
          n = 0) %>% 
  add_row(school = "Louisiana\nTech",
          tourney_finish = "2nd Round",
          n = 0) 

p2 <- ggplot(transform(tourney_seed,
                       seed = factor(seed, levels = c("5+", "4", "3", "2", "1")),
                       school = factor(school, levels = c("UConn", "Notre\nDame", "Tennessee", "Stanford", "Louisiana\nTech")))) +
  geom_col(aes(x = seed, y = n, fill = school), alpha = 0.7, width = 0.7) +
  scale_fill_manual(values = c("UConn" = "#0E1A3E",
                               "Notre\nDame" = "#217E51",
                               "Tennessee" = "#FF7D00",
                               "Stanford" = "#9A0D20",
                               "Louisiana\nTech" = "#6FB5EC")) +
  coord_flip() +
  facet_grid(school ~ ., switch = "y") +
  geom_hline(yintercept = 23, size = 1) +
  geom_segment(aes(x = seed, y = 0, yend = max(n), xend = seed), color = "grey30") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#FFF8E7", color = "transparent"),
        panel.background = element_rect(fill = "#FFF8E7", color = "transparent"),
        panel.grid = element_blank(),
        axis.line.y = element_line(size = 1),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text.y.left = element_blank()) +
  theme(plot.margin = unit(c(12, 0, 2, 0), "lines"))

tourney_seed_legend <- tourney_seed %>% 
  filter(school == "UConn") %>% 
  mutate(n = c(8, 8, 8, 8, 8))

p2_legend <- ggplot(transform(tourney_seed_legend,
                              seed = factor(seed, levels = c("5+", "4", "3", "2", "1")))) +
  coord_flip() +
  facet_grid(school ~ ., switch = "y") +
  geom_hline(yintercept = 23, size = 1) +
  geom_segment(aes(x = seed, y = 0, yend = n*2.87, xend = seed), color = "grey85") +
  geom_text(data = .%>% filter(n == 8), aes(label = seed, x = seed, y = n), nudge_y = ifelse(tourney_seed_legend$seed == "5+", 3.7, 3), size = 4.5, color = "black", fontface = "bold", family = "Consolas") +
  ggtitle("Seed") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#FFF8E7", color = "transparent"),
        panel.background = element_rect(fill = "#FFF8E7", color = "transparent"),
        panel.grid = element_blank(),
        text = element_text(family = "Impact"),
        axis.line.y = element_line(size = 1),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text.y.left = element_blank(),
        plot.title = element_text(size = 13, hjust = 0.48))  +
  theme(plot.margin = unit(c(1, 0, 0, 0), "lines"))

p2 <- ggdraw(p2) +
  draw_plot(p2_legend, x = 1, y = 1, width = 1, height = .15, hjust = 1, vjust = 1) 


tourney_results <- TOURNAMENT_DATA %>%
  filter(school %in% most_championship_games$school) %>%
  mutate(seed = case_when(
    seed == 1 ~ "1",
    seed == 2 ~ "2",
    seed == 3 ~ "3",
    seed == 4 ~ "4",
    seed >= 5 ~ "5+"
  )) %>%
  mutate(tourney_finish = case_when(
    tourney_finish == "Champion" ~ "Champion",
    tourney_finish == "Runner-up" ~ "Runner-up",
    tourney_finish == "Final Four" ~ "Final Four",
    tourney_finish == "Elite Eight" ~ "Elite Eight",
    TRUE ~ "Sweet Sixteen/earlier"
  )) %>%
  group_by(school, seed, tourney_finish) %>%
  summarize(n = n()) %>%
  ungroup()

fct_levels <- c("UConn","Notre\nDame","Tennessee","Stanford","Louisiana\nTech")
fct_levels2 <- c("Champion", "Runner-up", "Final Four", "Elite Eight", "Sweet Sixteen/earlier")

tourney_results2 <- tourney_results %>%
  select(-c(seed)) %>% 
  gather_set_data(1:2) %>% 
   mutate_at(vars(school),
              funs(factor(., levels = fct_levels))) %>%
    mutate_at(vars(tourney_finish),
              funs(factor(., levels = fct_levels2)))

p3 <- ggplot(tourney_results2, aes(x = x, id = id, split = factor(y, levels = c("UConn", "Notre\nDame","Tennessee","Stanford","Louisiana\nTech",
                                                              "Champion","Runner-up","Final Four","Elite Eight","Sweet Sixteen/earlier")), value = n)) +
  geom_parallel_sets(aes(fill = school), alpha = 0.7, axis.width = -0.01,
                     n = 100, strength = 0.5) +
  geom_parallel_sets_axes(fill = "grey20", axis.width = -0.01) +
  scale_color_manual(values = c("UConn" = "#0E1A3E",
                                "Notre\nDame" = "#217E51",
                                "Tennessee" = "#FF7D00",
                                "Stanford" = "#9A0D20",
                                "Louisiana\nTech" = "#6FB5EC")) +
  scale_fill_manual(values = c("UConn" = "#0E1A3E",
                               "Notre\nDame" = "#217E51",
                               "Tennessee" = "#FF7D00",
                               "Stanford" = "#9A0D20",
                               "Louisiana\nTech" = "#6FB5EC")) +  
  annotate(geom = "text", x = 2.4, y = 170, label = "Champion", size = 6, family = "Impact") +
  annotate(geom = "text", x = 2.4, y = 142, label = "Runner-up", size = 6, family = "Impact") +
  annotate(geom = "text", x = 2.4, y = 112, label = "Final Four", size = 6, family = "Impact") +
  annotate(geom = "text", x = 2.4, y = 75, label = "Elite Eight", size = 6, family = "Impact") +
  annotate(geom = "text", x = 2.45, y = 25, label = "Sweet Sixteen\nor earlier", size = 6, lineheight = 0.8, family = "Impact") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FFF8E7", color = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(10, 3, 0, -6), "lines"))
  
plot_grid(p_combined, p2, p3, ncol = 3, rel_widths = c(4,0.5,1))

ggsave("2020_41_NCAA_Tourney.png", width = 21, height = 10, device = "png", type = "cairo")
