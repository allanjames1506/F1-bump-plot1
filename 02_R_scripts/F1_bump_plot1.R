# 1 Inspiration----
# 1.1* Bump charts----
# https://github.com/steodose/Tidy-Tuesday/blob/master/Week%2028_2022.R
# https://github.com/davidsjoberg/ggbump/wiki/geom_bump-with-flags
# https://r-charts.com/ranking/ggbump/?utm_content=cmp-true

# 1.2* Camcorder----
# https://github.com/thebioengineer/camcorder

# 1.3* gganimate()
# https://gganimate.com/index.html
# https://rstudio.github.io/cheatsheets/gganimate.pdf
# https://cloud.r-project.org/web/packages/gganimate/vignettes/gganimate.html
# https://anderfernandez.com/en/blog/how-to-create-animations-in-r-with-gganimate/

# 2 Libraries----

library(dplyr)
library(tidyr)
library(ggbump)
library(ggplot2)
#library(patchwork)
#library(magick)
#library(cowplot)
library(glue)
library(ggtext)
#library(janitor)
#library(lubridate)
#library(zoo)
#library(scales)
#library(gt)
library(showtext)
library(camcorder)
library(readr)
library(gganimate)
#library(gghighlight)
remotes::install_github("wilkelab/gridtext")

1# 3 Fonts----
# Loading Google fonts (https://fonts.google.com/)
# e.g. Orbitron font https://fonts.google.com/specimen/Orbitron

# https://www.rdocumentation.org/packages/sysfonts/versions/0.8.8/topics/font_families_google
#fonts <- font_families_google(db_cache = TRUE, handle = curl::new_handle())
#match('Rubik', fonts)
#fonts[800:1000]

font_add_google("Outfit","outfit")
font_add_google("Rubik","rubik")
font_add_google("Rubik Mono One","rubik2")
font_add_google("Orbitron","orbitron")
font_add_google("Fira Sans","fira")
font_add_google("Alfa Slab One", "alfa")
# font_add_google("Bitter","bit")
# font_add_google("Roboto", "roboto")
showtext_auto()

# 4 F1 colours----
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
  "Red Bull" = "#EC1845",
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

# 5 Camcorder----
gg_record(
  dir = file.path(tempdir(),"recording"),
  device = "png",
  width = 6.47,
  height = 4,
  units = "in",
  dpi = 350
)

gg_resize_film(
  height = 6,
  width = 9,
  units = "in",
  dpi = 350
)

# 6 Bump charts----
# *6.1 lines + points----

# read in data from pit wall app
# https://pitwall.app
# https://pitwall.app/seasons/2023-formula-1-world-championship
f1_constructors <- read.csv('./00_raw_data/F1_constructors_2023.csv') %>% 
  mutate(Race_number = 1:n()) %>% 
  select(-'Grand_Prix')

# pivot longer : F1 teams as one column  
f1_constructors_long <- f1_constructors %>% 
  pivot_longer(!Race_number, names_to = 'F1_team', values_to = 'points')

# rank teams per race
f1_constructors_rank_by_race <- f1_constructors_long %>% 
  group_by(Race_number) %>% 
  mutate(
    rank = row_number(desc(points))
  ) %>%
  ungroup() %>%
  arrange(rank, Race_number) %>% 
  ungroup()

# tidy up team names
f1_constructors_rank_by_race <- f1_constructors_rank_by_race %>% 
  mutate(F1_team = case_when(F1_team == 'Alfa_Romeo' ~ 'Alfa Romeo',
                             F1_team == 'Alpha_Tauri' ~ 'Alpha Tauri',
                             F1_team == 'Alpine_F1_Team' ~ 'Alpine',
                             F1_team == 'Aston_Martin' ~ 'Aston Martin',
                             F1_team == 'Haas_F1_Team' ~ 'Haas',
                             F1_team == 'Red_Bull' ~ 'Red Bull',
                             TRUE ~ F1_team))

# make chart
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

#f1_constructors_bump_chart 

ggsave('./03_plots/f1_constructors_bump_plot3.png', dpi = 350, height = 4, width = 6.47, units = 'in')

# *6.2 just lines----
f1_constructors_bump_chart_alt <- ggplot(data = f1_constructors_rank_by_race, aes(Race_number, rank, col = F1_team)) +
  geom_bump(aes(color = F1_team), size = 2.5, lineend = "round") +
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

#f1_constructors_bump_chart_alt

# 7 Animate----
# keep Grand Prix locations/countries variable
f1_constructors2 <- read.csv('./00_raw_data/F1_constructors_2023.csv') %>% 
  mutate(Race_number = 1:n()) %>% 
  mutate(Grand_Prix = case_when(Grand_Prix == 'USA_Austin' ~ 'USA - Austin',
                                TRUE ~ Grand_Prix))

# pivot F1 teams into one variable
f1_constructors_long2 <- f1_constructors2 %>% 
  pivot_longer(cols = 'Alfa_Romeo':'Williams', names_to = 'F1_team', values_to = 'points') 

# rank teams for race points
f1_constructors_rank_by_race2 <- f1_constructors_long2 %>% 
  group_by(Grand_Prix) %>% 
  mutate(
    rank = row_number(desc(points))
  ) %>%
  ungroup() %>%
  arrange(rank, Grand_Prix) %>% 
  ungroup()

# tidy up data e.g. team names
f1_constructors_rank_by_race2 <- f1_constructors_rank_by_race2 %>% 
  mutate(F1_team = case_when(F1_team == 'Alfa_Romeo' ~ 'Alfa Romeo',
                             F1_team == 'Alpha_Tauri' ~ 'Alpha Tauri',
                             F1_team == 'Alpine_F1_Team' ~ 'Alpine',
                             F1_team == 'Aston_Martin' ~ 'Aston Martin',
                             F1_team == 'Haas_F1_Team' ~ 'Haas',
                             F1_team == 'Red_Bull' ~ 'Red Bull',
                             TRUE ~ F1_team))

# make teams and locations factors
f1_constructors_rank_by_race2 %>% 
  mutate(F1_team = factor(F1_team, levels = c('Red Bull',
                                              'Mercedes',
                                              'Ferrari',
                                              'McLaren',
                                              'Aston Martin',
                                              'Alpine',
                                              'Haas',
                                              'Alfa Romeo',
                                              'Alpha Tauri',
                                              'Williams'
  )),
  Grand_Prix = factor(Grand_Prix, levels = c('Bahrain',
                                             'Saudi Arabian',
                                             'Australian',
                                             'Azerbaijan',
                                             'Miami',
                                             'Monaco',
                                             'Spanish',
                                             'Canadian',
                                             'Austrian',
                                             'British',
                                             'Hungarian',
                                             'Belgian',
                                             'Dutch',
                                             'Italian',
                                             'Singapore',
                                             'Japanese',
                                             'Qatar',
                                             'USA - Austin')))

# basic plot
a <- f1_constructors_rank_by_race2 %>%
  ggplot() +
  geom_col(aes(rank, points, fill = F1_team), alpha = 0.7) +
  geom_text(aes(rank, points, label = as.character(points)), hjust = -0.1, size = 18, fontface = 'bold', family = 'rubik', colour = 'floralwhite') +
  geom_text(aes(rank, y=0 , label = F1_team), hjust = "right", size = 16, fontface = 'bold', col = "#4d4d4d", family = 'orbitron') +
  geom_text(aes(x=10, y=max(points), label = Grand_Prix), hjust = 0.6, vjust = 1, alpha = 0.5,  col = "#b2182b", fontface = 'bold', family = 'rubik', size = 10) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  scale_y_continuous(  
    limits = c(-1200, 1200),  
    breaks = c(0, 200, 400, 600, 800, 1000)) +
  scale_fill_manual(values = f1_constructors_colors_main) +
  labs(caption="Data: pitwall.app | Plot Allan James @allanjames1506") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray70", color = "transparent"),
        plot.background = element_rect(fill = "gray70"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.caption = element_text(
          size = 9, family = "orbitron", color = "#21435f",hjust = 0.5)) 
a
# animate
anim_a <- a + transition_states(Race_number, transition_length = 3, state_length = 1) + 
  ease_aes("cubic-in")

animation_to_save <- anim_a  + exit_shrink()

anim_save("./04_gifs/third_saved_animation.gif", animation = animation_to_save, height = 400, width = 650)

# 8 Chequered flag----

d=data.frame(x1=c(1,3,1,5,4), x2=c(2,4,3,6,6), y1=c(1,1,4,1,3), y2=c(2,2,5,3,5), t=c('a','a','a','b','b'), r=c(1,2,3,4,5))
ggplot() + 
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black", alpha=0.5) +
  geom_text(data=d, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=4) 

d2 <- data.frame(x1 = c(1, 2, 3, 4, 5,
                        1, 2, 3, 4, 5,
                        1, 2, 3, 4, 5,
                        1, 2, 3, 4, 5,
                        1, 2, 3, 4, 5), x2 = c(2, 3, 4, 5, 6,
                                               2, 3, 4, 5, 6,
                                               2, 3, 4, 5, 6,
                                               2, 3, 4, 5, 6,
                                               2, 3, 4, 5, 6), y1 = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5), y2 = c(2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6), t=c('a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                                                                             'b', 'a', 'b', 'a', 'b',
                                                                                                                                                                                                                                             'a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                                                                             'b', 'a', 'b', 'a', 'b',
                                                                                                                                                                                                                                             'a', 'b', 'a', 'b', 'a'), r=c(1, 2, 3, 4, 5,
                                                                                                                                                                                                                                                                           6, 7, 8, 9, 10,
                                                                                                                                                                                                                                                                           11, 12, 13, 14, 15,
                                                                                                                                                                                                                                                                           16, 17, 18, 19, 20,
                                                                                                                                                                                                                                                                           21, 22, 23, 24, 25),
                 fill = c('white', 'black', 'white', 'black', 'white',
                          'black', 'white', 'black', 'white', 'black',
                          'white', 'black', 'white', 'black', 'white',
                          'black', 'white', 'black', 'white', 'black',
                          'white', 'black', 'white', 'black', 'white'))
ggplot() + 
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  scale_fill_identity() +
  geom_rect(data=d2, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), inherit.aes = FALSE, alpha=0.5) +
  geom_text(data=d2, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=20)

f1_constructors_rank_by_race2_chequred <- f1_constructors_rank_by_race2 %>% 
  mutate(stripe_x = factor(ifelse(rank %% 2 == 0, 1, 0)))

d3 <- data.frame(x1 = c(1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60), x2 = c(10, 20, 30, 40, 50, 60, 70,
                                                            10, 20, 30, 40, 50, 60, 70,
                                                            10, 20, 30, 40, 50, 60, 70,
                                                            10, 20, 30, 40, 50, 60, 70,
                                                            10, 20, 30, 40, 50, 60, 70,
                                                            10, 20, 30, 40, 50, 60, 70,
                                                            10, 20, 30, 40, 50, 60, 70), y1 = c(-0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5,
                                                                                                 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
                                                                                                 5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5,
                                                                                                 8.5, 8.5, 8.5, 8.5, 8.5, 8.5, 8.5,
                                                                                                 11.5, 11.5, 11.5, 11.5, 11.5, 11.5, 11.5,
                                                                                                 14.5, 14.5, 14.5, 14.5, 14.5, 14.5, 14.5,
                                                                                                 17.5, 17.5, 17.5, 17.5, 17.5, 17.5, 17.5), y2 = c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
                                                                                                                                                   5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5,
                                                                                                                                                   8.5, 8.5, 8.5, 8.5, 8.5, 8.5, 8.5,
                                                                                                                                                   11.5, 11.5, 11.5, 11.5, 11.5, 11.5, 11.5,
                                                                                                                                                   14.5, 14.5, 14.5, 14.5, 14.5, 14.5, 14.5,
                                                                                                                                                   17.5, 17.5, 17.5, 17.5, 17.5, 17.5, 17.5,
                                                                                                                                                   20.5, 20.5, 20.5, 20.5, 20.5, 20.5, 20.5), t=c('a', 'b', 'a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                                  'b', 'a', 'b', 'a', 'b', 'a', 'b',
                                                                                                                                                                                                  'a', 'b', 'a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                                  'b', 'a', 'b', 'a', 'b', 'a', 'b',
                                                                                                                                                                                                  'a', 'b', 'a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                                  'b', 'a', 'b', 'a', 'b', 'a', 'b',
                                                                                                                                                                                                  'a', 'b', 'a', 'b', 'a', 'b', 'a'), r=c(1, 2, 3, 4, 5, 6, 7, 
                                                                                                                                                                                                                                          8, 9, 10, 11, 12, 13, 14, 
                                                                                                                                                                                                                                          15, 16, 17, 18, 19, 20, 21, 
                                                                                                                                                                                                                                          22, 23, 24, 25, 26, 27, 28,
                                                                                                                                                                                                                                          29, 30, 31, 32, 33, 34, 35,
                                                                                                                                                                                                                                          36, 37, 38, 39, 40, 41, 42,
                                                                                                                                                                                                                                          42, 44, 45, 46, 47, 48, 49), fill = c('#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', 
                                                                                                                                                                                                                                                                                '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', 
                                                                                                                                                                                                                                                                                '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d',
                                                                                                                                                                                                                                                                                '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000',
                                                                                                                                                                                                                                                                                '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d',
                                                                                                                                                                                                                                                                                '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000',
                                                                                                                                                                                                                                                                                '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d'))



# 9 Highlight----

a_highlight1 <- f1_constructors_rank_by_race2 %>%
  ggplot() +
  #gghighlight(max(points) >230) +
  geom_col(aes(rank, points, fill = F1_team), alpha = 0.7) +
  geom_text(aes(rank, points, label = as.character(points)), hjust = -0.1, size = 18, fontface = 'bold', family = 'rubik', colour = 'floralwhite') +
  geom_text(aes(rank, y=0 , label = F1_team), hjust = "right", size = 16, fontface = 'bold', col = "#4d4d4d", family = 'orbitron') +
  geom_text(aes(x=10, y=max(points), label = Grand_Prix), hjust = 0.6, vjust = 1, alpha = 0.5,  col = "#b2182b", fontface = 'bold', family = 'rubik', size = 10) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  scale_y_continuous(  
    limits = c(-1200, 1200),  
    breaks = c(0, 200, 400, 600, 800, 1000)) +
  scale_fill_manual(values = f1_constructors_colors_main) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray70", color = "transparent"),
        plot.background = element_rect(fill = "gray70"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

a_highlight1

# animate
anim_a_highlight1 <- a_highlight1 + transition_states(Race_number, transition_length = 3, state_length = 1) + 
  ease_aes("cubic-in")

animation_to_save_highlight1 <- anim_a_highlight1  + exit_shrink()

anim_save("./04_gifs/first_saved_animation_highlight1.gif", animation = animation_to_save_highlight1, height = 400, width = 650)



# 10 Mexico GP----

# data https://www.statsf1.com/en/2023/mexico-city/tour-par-tour.aspx

McLaren_colours_main <- c('#FF8000', '#00843D')
McLaren_colours_secondary <- c('#00843D', '#FF8000')

mexico_NOR_PIA <- read.csv('./00_raw_data/mexico_2023_NOR_PIA.csv') %>% 
  pivot_longer(cols ='Lando':'Oscar', names_to = 'driver', values_to = 'lap_position') %>% 
  mutate(driver = factor(driver, levels = c('Lando', 'Oscar')))

mex_point_graph <- mexico_NOR_PIA %>%
  ggplot(aes(x = Lap, y = lap_position, size = lap_position, colour = driver, group = driver)) + 
  geom_step(size = 2) +
  geom_point(shape=21, stroke = 4, fill = '#FFCD00') +
  geom_text(aes(Lap, lap_position, label = as.character(lap_position), hjust = ifelse(lap_position > 10, -0.5, 0.5)), vjust = 0.5, size = 12, fontface = 'bold', family = 'rubik', colour = 'grey40') +
  #geom_text(aes(x=50, y=max(lap_position), label = Lap_id), hjust = 0, vjust = 0, alpha = 0.5,  col = "#b2182b", fontface = 'bold', family = 'orbitron', size = 30) +
  geom_text(aes(Lap , y = -1.5, label = as.character(Lap_id)), data = . %>% 
              filter(Lap %% 2 == 1), hjust = 0.5, size = 12, fontface = 'bold', col = "#4d4d4d", family = 'rubik') +
  #geom_rect(data=d3, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), inherit.aes = FALSE, alpha=0.1) +
  scale_colour_manual(values = McLaren_colours_main) +
  scale_fill_identity() +
  scale_size(range = c(40, 5)) +
  #scale_x_continuous(limits = c(0, 75)) +
  scale_y_reverse(breaks = c(1, 4, 8, 12, 16, 20), labels=c("1" = "1st", "4" = "4th", "8" = "8th", "12" = "12th", "16" = "16th", "20" = "20th"), position = 'right') +
  #scale_y_discrete(labels=c("1" = "1st", "4" = "4th", "8" = "8th", "12" = "12th", "16" = "16th", "20" = "20th")) +
  coord_cartesian(xlim = c(-10, 80), ylim = c(22, -2.5), expand = F) +
  theme_minimal() + 
  theme(plot.margin = unit(c(20, 100, 20, 40), "pt"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray90", color = "transparent"),
        plot.background = element_rect(fill = "gray90"),
        text = element_text(color = "#47c7fc"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(family = 'rubik', face = 'bold', colour = '#4d4d4d', size = 32),
        plot.title = element_markdown(size = 40, family = "alfa", face = "bold", hjust = 0.5),
        plot.subtitle = element_markdown(size = 20, family = "alfa", face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 20, family = "alfa",hjust = 0.5)) +
  labs(x = NULL,
       y = NULL,
       title = "<span style = 'color:#006341'>Mexico </span><span style = 'color:#FFFFFF'> Grand </span><span style = 'color:#C8102E'>Prix </span>",
       #subtitle = "<span style = 'color:#FF8000'> LN4 </span><br><span style = 'color:#00843D'> OP81 </span>",
       caption = "Data: statsF1.com. | Allan James (@allanjames1506)") +
  annotate(
    "text",
    x = c(1,1),
    y = c(15, 5),
    label = c('LN4', 'OP81'),
    hjust = c(1, 1),
    vjust = c(1, 1),
    size = 10,
    colour = c('#FF8000', '#00843D'),
    fontface = "bold",
    family = "alfa"
  )

mex_point_graph

anim_mex <- mex_point_graph +
  transition_reveal(Lap) 

#anim_mex2 <- animate(anim_mex, end_pause = 30)

anim_save("./04_gifs/first_saved_animation_anim_mex.gif", anim_mex, height = 600, width = 800)

title_text <- "**<span style = 'color:#006341'>Me</span><span style = 'color:#FFFFFF'>xi</span><span style = 'color:#C8102E'>co</span>** <span style = 'color:#4d4d4d'>Grand Prix</span>"
subtitle_text <- "<span style = 'color:#FF8000'>LN4:</span> and <span style = 'color:#E69F00'>OP81</span> race postion over 71 laps"
caption_text <- "<span style = 'font-size:6pt;color:grey60'>Data: statsF1.com. | Allan James (@allanjames1506)."

mex_point_graph + 
  plot_annotation(title = title_text, subtitle = subtitle_text,
                                  caption = caption_text, theme = theme(
    plot.title = element_markdown(
      margin = margin(b = 0.4, unit = 'cm'),
      # 0.4cm margin at bottom of title
      size = 40
    )))




