library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

pbp_20 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

pitt_epa <- pbp_20 %>%
  filter(posteam == "PIT") %>%
  group_by(posteam, defteam, week) %>%
  summarize(epa_per_play = mean(epa, na.rm = T)) %>%
  arrange(week) %>%
  ungroup() %>%
  mutate(game_num = row_number())

pitt_epa <- pitt_epa %>%
  left_join(teams_colors_logos, by = c('defteam' = 'team_abbr'))

pitt_epa %>%
  ggplot() +
  theme_minimal() + 
  scale_colour_identity() +
  geom_line(aes(x=game_num, y=epa_per_play, color = "blue")) +
  geom_image(aes(x=game_num, y = epa_per_play, image = team_logo_espn), size = 0.05, asp = 16/9) +
  labs(
  x = "Game Number",
  y = "Offensive EPA Per Play",
  title = "The 2020 Pittsburgh Steelers Offensive EPA Per Play by Game",
  subtitle = "The logo is the team they were playing against",
  caption = "By Tej Seth | @mfbanalytics"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_text(size = 8),
    axis.title.y = element_text(size = 14)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave('lions_epa.png', dpi = 300)
  