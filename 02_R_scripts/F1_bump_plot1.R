
library(dplyr)
library(tidyr)
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
#devtools::install_github("thomasp85/transformr")
install.packages("ggchicklet", repos = "https://cinc.rud.is")

# Repo link: https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-07-12

# ## Load data
# flights <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv") %>%
#   janitor::clean_names()
# 
# ## Data preparation
# 
# # table(flights$state_name)
# 
# # change Turkey's name as the formatting doesn't play nice with code
# flights <- flights %>%
#   mutate(state_name = case_when(
#     state_name == "Türkiye" ~ "Turkey",
#     TRUE ~ state_name
#   ))
# 
# colnames(flights)
# 
# country_flights_by_year <- flights %>%
#   select(year, state = state_name, flights = flt_tot_1) %>%
#   group_by(year, state) %>%
#   summarise(flights = sum(flights))
# 
# country_rank_by_year <- country_flights_by_year %>%
#   group_by(year) %>%
#   mutate(
#     rank = row_number(desc(flights))
#   ) %>%
#   ungroup() %>%
#   arrange(rank, year)
# 
# glimpse(country_rank_by_year)
# 
# max_rank <- 12
# 
# todays_top <- country_rank_by_year %>%
#   filter(year == 2022, rank <= max_rank) %>%
#   pull(state)
# 
# flights_bump_chart <- country_rank_by_year %>%
#   filter(state %in% todays_top) %>%
#   ggplot(aes(year, rank, col = state)) +
#   geom_point(size = 2) +
#   geom_bump(size = 1) +
#   geom_text(
#     data = country_rank_by_year %>%
#       filter(year == 2016, state %in% todays_top),
#     aes(label = state),
#     hjust = 1,
#     nudge_x = -0.1,
#     fontface = "bold",
#     family = "Outfit"
#   )
# 
# flights_bump_chart

# 1. Set fonts----
## Loading Google fonts (https://fonts.google.com/)

# https://www.rdocumentation.org/packages/sysfonts/versions/0.8.8/topics/font_families_google
fonts <- font_families_google(db_cache = TRUE, handle = curl::new_handle())
match('Rubik', fonts)
fonts[800:1000]

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
  width = 8,
  units = "in",
  dpi = 350
)

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
  geom_line(size = 2.5, lineend = "round") +
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

geom_text(
  data = f1_constructors_rank_by_race %>%
    filter(Race_number == 1),
  aes(label = F1_team),
  size = 20,
  hjust = 1,
  nudge_x = -0.5,
  fontface = "bold",
  family = "orbitron"
)

# glimpse(f1_constructors_rank_by_race)
# mutate(
#   Species = ifelse(
#     Species == "setosa",
#     "<span style = 'font-size:10pt'>setosa</span>",
#     glue("<span style = 'font-size:20pt'>{Species}</span>")
#   )

animate_F1_constructors <- f1_constructors_rank_by_race %>%
  mutate(rank = case_when(rank == 1 ~ "<span style = 'font-size:25pt'>1</span>",
                          rank == 2 ~ "<span style = 'font-size:18pt'>1</span>",
                          rank == 3 ~ "<span style = 'font-size:15pt'>1</span>",
                          rank == 4 ~ "<span style = 'font-size:10pt'>1</span>",
                          TRUE ~ glue("<span style = 'font-size:9pt'>{rank}</span>"))) %>%
  ggplot() +
  geom_col(aes(rank, points, fill = F1_team)) +
  geom_text(aes(rank, points, label = as.character(points)), hjust = -0.1) +
  geom_text(aes(rank, y=0 , label = F1_team), hjust = 1.1, size = 20, fontface = 'bold', family = 'orbitron') +
  geom_text(aes(x=10, y=max(points) , label = as.factor(Race_number)), vjust = 0.2, alpha = 0.5,  col = "gray", size = 20) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
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
        legend.position = "none",
        theme(axis.text.y = element_markdown())) +
    transition_states(Race_number, state_length = 2, transition_length = 2)
# +
#     enter_fade() +
#     exit_fade() +
#     ease_aes('quadratic-in-out')
# #+
#   # theme(
#   #   panel.grid = element_blank(), 
#   #   legend.position = "none",
#   #   axis.ticks.y = element_blank(),
#   #   axis.title.y = element_blank(),
#   #   axis.text.y = element_blank(),
#   #   plot.margin = margin(1, 4, 1, 3, "cm")
#   # ) +
#   transition_states(Race_number, state_length = 2, transition_length = 2) +
#   enter_fade() +
#   exit_fade() + 
#   ease_aes('quadratic-in-out')
  

animate_F1_constructors2 <- f1_constructors_rank_by_race %>%
  mutate(rank = case_when(rank == 1 ~ "<span style = 'font-size:50pt'>1</span>",
                               rank == 2 ~ "<span style = 'font-size:36pt'>1</span>",
                               rank == 3 ~ "<span style = 'font-size:30pt'>1</span>",
                               rank == 4 ~ "<span style = 'font-size:20pt'>1</span>",
                               TRUE ~ glue("<span style = 'font-size:18pt'>{rank}</span>"))) %>%
  ggplot() +
  geom_col(aes(rank, points, fill = F1_team)) + 
  coord_flip() + 
  theme(axis.text.y = element_markdown())

animate_F1_constructors2

animate_F1_constructors3 <- f1_constructors_rank_by_race %>%
  ggdotchart(rank, points,
             color = 'F1_team',
             sorting = "descending",
             add = "segments",
             rotate = TRUE,
             dot.size = 6,
             label = round(f1_constructors_rank_by_race$points),
             font.label = list(color = "white", 
                               size = 9, 
                               vjust = 0.5),
             ggtheme = theme_pubr())

ggsave('./03_plots/f1_constructors_bump_plot5.png', dpi = 350, height = 4, width = 6.47, units = 'in')

animate(animate_F1_constructors, width = 1000, height = 600, fps = 25, duration = 15, rewind = FALSE)


animate_F1_constructors4 <- f1_constructors_rank_by_race %>%
  ggplot() +
  #ggdotchart(x=rank, y= points) +
  geom_col(aes(rank, points, fill = F1_team)) +
  geom_text(aes(rank, points, label = as.character(points)), hjust = -0.1) +
  geom_text(aes(rank, y=0 , label = F1_team), hjust = 1.1, size = 20, fontface = 'bold', family = 'orbitron') +
  geom_text(aes(x=10, y=max(points) , label = as.factor(Race_number)), vjust = 0.2, alpha = 0.5,  col = "gray", size = 20) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  scale_y_continuous(  
    limits = c(-1000, 800),  
    breaks = c(0, 200, 400, 600, 800)) +
  #scale_y_reverse(position = "left", breaks = 1:100)
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
        legend.position = "none") +
  transition_states(Race_number, state_length = 2, transition_length = 2)

animate_F1_constructors4

+
  scale_y_reverse(position = "right", breaks = 1:100) +
  scale_color_manual(values = f1_constructors_colors_main) +
  scale_fill_manual(values = f1_constructors_colors_secondary) +
  coord_cartesian(xlim = c(-7.5, 21.5), ylim = c(11, 0.25), expand = F)


my_theme <- theme_classic(base_family = "Times") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro")) +
  theme(panel.background = element_rect(fill = "gainsboro"))

my_theme2 <- theme_minimal() +
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

f1_constructors2 <- read.csv('./00_raw_data/F1_constructors_2023.csv') %>% 
  mutate(Race_number = 1:n()) %>% 
  mutate(Grand_Prix = case_when(Grand_Prix == 'USA_Austin' ~ 'USA - Austin',
                                TRUE ~ Grand_Prix))

f1_constructors_long2 <- f1_constructors2 %>% 
  pivot_longer(cols = 'Alfa_Romeo':'Williams', names_to = 'F1_team', values_to = 'points') %>% 
  select(-Race_number)

f1_constructors_rank_by_race2 <- f1_constructors_long2 %>% 
  group_by(Grand_Prix) %>% 
  mutate(
    rank = row_number(desc(points))
  ) %>%
  ungroup() %>%
  arrange(rank, Grand_Prix) %>% 
  ungroup()

f1_constructors_rank_by_race2 <- f1_constructors_rank_by_race2 %>% 
  mutate(F1_team = case_when(F1_team == 'Alfa_Romeo' ~ 'Alfa Romeo',
                             F1_team == 'Alpha_Tauri' ~ 'Alpha Tauri',
                             F1_team == 'Alpine_F1_Team' ~ 'Alpine',
                             F1_team == 'Aston_Martin' ~ 'Aston Martin',
                             F1_team == 'Haas_F1_Team' ~ 'Haas',
                             F1_team == 'Red_Bull' ~ 'Red Bull',
                             TRUE ~ F1_team))

f1_constructors_rank_by_race2

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
                                                    'USA - Austin'))) %>% 
  ggplot() +
  aes(xmin = 0 ,  
      xmax = points *1) +  
  aes(ymin = rank - .45,
      ymax = rank + .45,
      y = rank) +
  facet_wrap(~ Grand_Prix) +
  #ggchicklet:::geom_rrect(r = unit(0.5, 'npc')) +
  ggchicklet:::geom_rrect(radius = grid:: unit(1, "mm"), alpha = 0.5) +
  #geom_rect(alpha = 0.7) +
  aes(fill = F1_team) +
  scale_fill_manual(values = f1_constructors_colors_main) +
  #scale_color_manual(values = f1_constructors_colors_secondary) +
  scale_x_continuous(
    limits = c(-500, 800),
    breaks = c(0, 250, 500, 750)) +
  geom_text(col = "gray13",
            hjust = "right",
            aes(label = F1_team),
            x = -50)+
  scale_y_reverse() +
  labs(fill = NULL) +
  labs(x = 'points') +
  labs(y = "") +  
  my_theme2 ->
  my_plot2

my_plot2

my_plot2 + 
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
