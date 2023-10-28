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

# 3 Fonts----
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
# gg_record(
#   dir = file.path(tempdir(),"recording"),
#   device = "png",
#   width = 6.47,
#   height = 4,
#   units = "in",
#   dpi = 350
# )
# 
# gg_resize_film(
#   height = 6,
#   width = 9,
#   units = "in",
#   dpi = 350
# )

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

# animate
anim_a <- a + transition_states(Race_number, transition_length = 3, state_length = 1) + 
  ease_aes("cubic-in")

animation_to_save <- anim_a  + exit_shrink()

anim_save("./04_gifs/third_saved_animation.gif", animation = animation_to_save, height = 400, width = 650)

 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
my_theme <- theme_classic(base_family = "Times") +
    theme(axis.text.y = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    theme(axis.line.y = element_blank()) +
    theme(legend.background = element_rect(fill = "gainsboro")) +
    theme(plot.background = element_rect(fill = "gainsboro")) +
    theme(panel.background = element_rect(fill = "gainsboro"))
  
f1_constructors3 <- read.csv('./00_raw_data/F1_constructors_2023.csv') %>% 
  mutate(Race_number = 1:n()) 
  
f1_constructors_long3 <- f1_constructors3 %>% 
  pivot_longer(cols = 'Alfa_Romeo':'Williams', names_to = 'F1_team', values_to = 'points') %>% 
  select(-Race_number)
  
f1_constructors_rank_by_race3 <- f1_constructors_long3 %>% 
  group_by(Grand_Prix) %>% 
  mutate(rank = row_number(desc(points))) %>%
  ungroup() %>%
  arrange(rank, Grand_Prix) %>% 
  ungroup()
  
f1_constructors_rank_by_race3 <- f1_constructors_rank_by_race3 %>% 
  mutate(F1_team = case_when(F1_team == 'Alfa_Romeo' ~ 'Alfa Romeo',
                             F1_team == 'Alpha_Tauri' ~ 'Alpha Tauri',
                             F1_team == 'Alpine_F1_Team' ~ 'Alpine',
                             F1_team == 'Aston_Martin' ~ 'Aston Martin',
                             F1_team == 'Haas_F1_Team' ~ 'Haas',
                             F1_team == 'Red_Bull' ~ 'Red Bull',
                             TRUE ~ F1_team))
  
f1_constructors_rank_by_race3
  
f1_constructors_rank_by_race3 %>% 
  mutate(F1_team = factor(F1_team, levels = c('Red Bull',
                                              'Mercedes',
                                              'Ferrari',
                                              'McLaren',
                                              'Aston Martin',
                                              'Alpine',
                                              'Haas',
                                              'Alfa Romeo',
                                              'Alpha Tauri',
                                              'Williams')),
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
                                                    'USA'))) %>% 
  ggplot(aes(x= rank, y=points)) +
  geom_chicklet(radius = grid::unit(15, 'mm'), fill = 'skyblue') +
  theme_minimal()
  #geom_col(aes(rank, points, fill = F1_team, colour = F1_team)) +
  #ggchicklet:::geom_rrect(radius = grid:: unit(1, "mm"))
#
    aes(xmin = 0 ,  
        xmax = points *1) +  
    aes(ymin = rank - .45,
        ymax = rank + .45,
        y = rank) +
    facet_wrap(~ Grand_Prix) +
    #ggchicklet:::geom_rrect(r = unit(0.5, 'npc')) +
    ggchicklet:::geom_rrect(radius = grid:: unit(1, "mm")) +
    #geom_rect(alpha = 0.7) +
    aes(fill = F1_team) + 
    scale_fill_manual(values = f1_constructors_colors_main) +
    scale_x_continuous(
      limits = c(-500, 800),
      breaks = c(0, 250, 500, 750)) +
    geom_text(col = "gray13",
              hjust = "right",
              aes(label = F1_team),
              x = -50)
  #coord_flip()+
  #scale_y_continuous(trans = "reverse") 
  #+
  #scale_y_reverse()
  #+
  #scale_fill_viridis_d(option = "magma",direction = -1) +
  #coord_flip(clip = "off", expand = FALSE) +
  #scale_y_reverse(limits=c(800, -1000), expand=c(0,0)) +
  #scale_y_continuous(limits = c(-1000, 800), breaks = c(0, 200, 400, 600, 800)) +
  scale_x_continuous(
    limits = c(-200, 800),
    breaks = c(0, 250, 500, 750)) +
    geom_text(col = "gray13",
              hjust = "right",
              aes(label = F1_team),
              x = -50) +  
    #scale_y_reverse() +
    labs(fill = NULL) +
    labs(x = 'points') +
    labs(y = "") +  
    my_theme ->
    my_plot
  
  my_plot
  
  my_plot + 
    facet_null() +
    scale_x_continuous(
      limits = c(-200, 800),
      breaks = c(0, 250, 500, 750)) +
    geom_text(x = 750 , y = -10,
              family = "sans",
              aes(label = as.character(Race_number)),
              size = 30, col = "grey18") +
    aes(group = F1_team) +
    gganimate::transition_time(Race_number)
  
  ggsave('./03_plots/f1_constructors_bump_plot6.png', dpi = 350, height = 4, width = 6.47, units = 'in') 
