##### Tidy Tuesday 2022 - Week 28 #####
#### By: Stephan Teodosescu ####

library(tidyverse)
library(ggbump)
library(patchwork)
library(magick)
library(cowplot)
library(glue)
library(ggtext)
library(ggimage) # for working with logos
library(janitor)
library(lubridate)
library(zoo)
library(scales)
library(gt)
library(showtext)
library(camcorder)
library(readr)
library(gganimate)
devtools::install_github("thomasp85/transformr")

# Repo link: https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-07-12

## Load data
flights <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv") %>%
  janitor::clean_names()

## Data preparation

# table(flights$state_name)

# change Turkey's name as the formatting doesn't play nice with code
flights <- flights %>%
  mutate(state_name = case_when(
    state_name == "Türkiye" ~ "Turkey",
    TRUE ~ state_name
  ))

colnames(flights)

country_flights_by_year <- flights %>%
  select(year, state = state_name, flights = flt_tot_1) %>%
  group_by(year, state) %>%
  summarise(flights = sum(flights))

country_rank_by_year <- country_flights_by_year %>%
  group_by(year) %>%
  mutate(
    rank = row_number(desc(flights))
  ) %>%
  ungroup() %>%
  arrange(rank, year)

glimpse(country_rank_by_year)

max_rank <- 12

todays_top <- country_rank_by_year %>%
  filter(year == 2022, rank <= max_rank) %>%
  pull(state)

flights_bump_chart <- country_rank_by_year %>%
  filter(state %in% todays_top) %>%
  ggplot(aes(year, rank, col = state)) +
  geom_point(size = 2) +
  geom_bump(size = 1) +
  geom_text(
    data = country_rank_by_year %>%
      filter(year == 2016, state %in% todays_top),
    aes(label = state),
    hjust = 1,
    nudge_x = -0.1,
    fontface = "bold",
    family = "Outfit"
  )

flights_bump_chart


f1_constructors <- read.csv('./00_raw_data/F1_constructors_2023.csv') %>% 
  mutate(Race_number = 1:n()) %>% 
  select(-'Grand_Prix')
  
f1_constructors_long <- f1_constructors %>% 
  pivot_longer(!Race_number, names_to = 'F1_team', values_to = 'points')

f1_constructors_rank_by_race <- f1_constructors_long %>% 
  group_by(Race_number) %>% 
  mutate(
    rank = row_number(desc(points))
  ) %>%
  ungroup() %>%
  arrange(rank, Race_number) %>% 
  ungroup()

f1_constructors_rank_by_race <- f1_constructors_rank_by_race %>% 
  mutate(F1_team = case_when(F1_team == 'Alfa_Romeo' ~ 'Alfa Romeo',
                             F1_team == 'Alpha_Tauri' ~ 'Alpha Tauri',
                             F1_team == 'Alpine_F1_Team' ~ 'Alpine',
                             F1_team == 'Aston_Martin' ~ 'Aston Martin',
                             F1_team == 'Haas_F1_Team' ~ 'Haas',
                             F1_team == 'Red_Bull' ~ 'Red Bull',
                             TRUE ~ F1_team))

f1_constructors_rank_by_race %>%
  filter(Race_number == 1)

gg_record(
  dir = file.path(tempdir(),"recording"),
  device = "png",
  width = 4,
  height = 6.47,
  units = "in",
  dpi = 350
)

# gg_resize_film(
#   height = 4,
#   width = 6.47,
#   units = "in",
#   dpi = 350
# )

# geom_point(
#   aes(color=factor(cyl), fill=factor(carb)),
#   shape=21, size=4, stroke=4) +
#   scale_color_manual(values=alpha(rainbow(3), 0.2)) +
#   guides(fill=guide_legend(override.aes = list(color=NA)))
# 
# ggplot(df, aes(x=wk, y=ht))  +
#   geom_line(aes(color=group)) +
#   geom_point(aes(fill=factor(flag)), size=4, shape=21, stroke=0) +
#   scale_fill_manual(values=color_flag) +
#   scale_colour_manual(values=color_group) +
#   theme_classic() +
#   labs(fill="Flag", colour="Group")

# https://community.rstudio.com/t/ggplot-set-colors-separately-for-geom-point-and-geom-line-manually/13901

f1_constructors_bump_chart <- f1_constructors_rank_by_race %>%
  ggplot(aes(Race_number, rank, col = F1_team)) +
  geom_bump(aes(color = F1_team), size = 2.5, lineend = "round") +
  geom_point(aes(fill = F1_team), size = 0.7, shape = 21) +
  geom_text(
    data = f1_constructors_rank_by_race %>%
      filter(Race_number == 1),
    aes(label = F1_team),
    size = 20,
    hjust = 1,
    nudge_x = -0.5,
    fontface = "bold",
    family = "orbitron"
  ) +
  geom_text(
    data = f1_constructors_rank_by_race %>%
      filter(Race_number == 18),
    aes(label = rank),
    hjust = 0,
    nudge_x = 0.5,
    alpha = 0.7,
    size = 46,
    fontface = "bold",
    family = "orbitron"
  ) +
  annotate(
    "text",
    x = c(1,18),
    y = c(0.25, 0.25),
    label = c('Bahrain', 'USA - Austin'),
    hjust = c(0, 1),
    vjust = 1,
    size = 12,
    fontface = "bold",
    family = "outfit"
  ) +
  scale_y_reverse(position = "right", breaks = 1:100) +
  scale_color_manual(values = f1_constructors_colors_main) +
  scale_fill_manual(values = f1_constructors_colors_secondary) +
  coord_cartesian(xlim = c(-7.5, 21.5), ylim = c(11, 0.25), expand = F) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray50", color = "transparent"),
        plot.background = element_rect(fill = "gray50"),
        text = element_text(color = "floralwhite"),
        plot.title = element_text(size = 50, face = "bold"),
        plot.subtitle = element_text(size =40),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_markdown(
          size = 25, family = "orbitron", color = "#21435f",hjust = 0.5)) +
  labs(x = NULL,
       y = NULL,
       title = "Constructors - how it's going",
       subtitle = "F1 constructors battle 2023",
       caption="**Data** pitwall.app **| Plot** Allan James @allanjames1506") +
  theme(text = element_text(family = 'orbitron'))

f1_constructors_bump_chart 

ggsave('./03_plots/f1_constructors_bump_plot3.png', dpi = 350, height = 4, width = 6.47, units = 'in')

# 1. Set fonts----
## Loading Google fonts (https://fonts.google.com/)

# https://www.rdocumentation.org/packages/sysfonts/versions/0.8.8/topics/font_families_google
fonts <- font_families_google(db_cache = TRUE, handle = curl::new_handle())
match('Rubik', fonts)
fonts[1000:1200]

font_add_google("Outfit","outfit")
font_add_google("Rubik","rubik")
font_add_google("Rubik Mono One","rubik2")
font_add_google("Orbitron","orbitron")
font_add_google("Fira Sans","fira")
# font_add_google("Bitter","bit")
# font_add_google("Roboto", "roboto")
showtext_auto()

# https://teamcolorcodes.com
f1_constructors_colors_main <- c(
  "Alfa Romeo" = "#A42134",
  "Alpha Tauri" = "#20394C",
  "Alpine" = "#2173B8",
  "Aston Martin" = "#037A68",
  "Ferrari" = "#EF1A2D",
  "Haas" = "#E6002B",
  "McLaren" = "#FF8000",
  "Mercedes" = "#00A19B",
  "Red Bull" = "#FED502",
  "Williams" = "#00A0DE"
)

f1_constructors_colors_secondary <- c(
  "Alfa Romeo" = "#241F21",
  "Alpha Tauri" = "#FFFFFF",
  "Alpine" = "#FFFFFF",
  "Aston Martin" = "grey50",
  "Ferrari" = "#FFF200",
  "Haas" = "#FFFFFF",
  "McLaren" = "#000000",
  "Mercedes" = "#000000",
  "Red Bull" = "#EC1845",
  "Williams" = "#000000"
)
    
p <- ggplot(data = f1_constructors_rank_by_race, aes(Race_number, rank, col = F1_team)) +
  geom_bump(aes(color = F1_team), size = 2.5, lineend = "round") +
  #geom_point(aes(fill = F1_team), size = 0.7, shape = 21) +
  geom_text(
    data = f1_constructors_rank_by_race %>%
      filter(Race_number == 1),
    aes(label = F1_team),
    size = 20,
    hjust = 1,
    nudge_x = -0.5,
    fontface = "bold",
    family = "orbitron"
  ) +
  geom_text(
    data = f1_constructors_rank_by_race %>%
      filter(Race_number == 18),
    aes(label = rank),
    hjust = 0,
    nudge_x = 0.5,
    alpha = 0.7,
    size = 46,
    fontface = "bold",
    family = "orbitron"
  ) +
  annotate(
    "text",
    x = c(1,18),
    y = c(0.25, 0.25),
    label = c('Bahrain', 'USA - Austin'),
    hjust = c(0, 1),
    vjust = 1,
    size = 12,
    fontface = "bold",
    family = "outfit"
  ) +
  scale_y_reverse(position = "right", breaks = 1:100) +
  scale_color_manual(values = f1_constructors_colors_main) +
  scale_fill_manual(values = f1_constructors_colors_secondary) +
  coord_cartesian(xlim = c(-7.5, 21.5), ylim = c(11, 0.25), expand = F) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray50", color = "transparent"),
        plot.background = element_rect(fill = "gray50"),
        text = element_text(color = "floralwhite"),
        plot.title = element_text(size = 50, face = "bold"),
        plot.subtitle = element_text(size =40),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_markdown(
          size = 25, family = "orbitron", color = "#21435f",hjust = 0.5)) +
  labs(x = NULL,
       y = NULL,
       title = "Constructors - how it's going",
       subtitle = "F1 constructors battle 2023",
       caption="**Data** pitwall.app **| Plot** Allan James @allanjames1506") +
  theme(text = element_text(family = 'orbitron'))

f1_constructors_rank_by_race_2teams <- f1_constructors_rank_by_race %>% 
  filter(F1_team == c('McLaren', 'Aston Martin'))

p2 <- ggplot(data = f1_constructors_rank_by_race, aes(Race_number, rank, col = F1_team)) +
  geom_line(aes(color = F1_team, group = F1_team), size = 2.5, lineend = "round") +
  #geom_point(aes(fill = F1_team), size = 1.5, shape = 21) +
  #scale_color_manual(values = f1_constructors_colors_main) +
  scale_y_reverse(position = "right", breaks = 1:100) +
  scale_color_manual(values = f1_constructors_colors_main) +
  #scale_fill_manual(values = f1_constructors_colors_secondary) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray70", color = "transparent"),
        plot.background = element_rect(fill = "gray70"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=50, family = "orbitron"),
        axis.ticks.y = element_blank(),
        legend.text=element_text(size=40, family = "orbitron", colour = 'gray20'),
        legend.title=element_blank())

ggsave('./03_plots/f1_constructors_bump_plot4.png', dpi = 350, height = 4, width = 6.47, units = 'in')

p
p2

p2 + transition_time(Race_number)
