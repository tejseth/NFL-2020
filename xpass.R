library(tidyverse)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(na.tools)
library(ggimage)
library(nflfastR)
library(gt)
library(mgcv)
library(scales)
library(ggforce)
library(remotes)
library(ggtext)
library(bayesboot)
library(ggthemes)

seasons <- 2018:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

pbp_rp <- pbp_rp %>%
  mutate(season = substr(old_game_id, 1, 4))

pbp_rp <- pbp_rp %>%
  mutate(
    posteam = case_when(
      posteam == 'OAK' ~ 'LV',
      posteam == 'SD' ~ 'LAC',
      posteam == 'STL' ~ 'LA',
      TRUE ~ posteam
    )
  )

pbp_rp <- pbp_rp %>%
  mutate(
    defteam = case_when(
      defteam == 'OAK' ~ 'LV',
      defteam == 'SD' ~ 'LAC',
      defteam == 'STL' ~ 'LA',
      TRUE ~ defteam
    )
  )

passes <- pbp_rp %>%
  filter(pass == 1) %>%
  filter(!is.na(pass_oe)) %>%
  filter(!is.na(epa)) %>%
  group_by(passer) %>%
  mutate(passes = n()) %>%
  filter(passes > 1500) %>%
  ungroup()

passes <- passes %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

passes %>%
  ggplot( aes(x=xpass, y=epa, group=passer, color=team_color)) +
  geom_smooth(se = FALSE) +
  scale_color_identity(aesthetics =  c("fill", "color")) +
  theme_fivethirtyeight() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Expected Pass %",
       y = "EPA/Play",
       title = "Mahomes is Best When the Defense Knows He's Passing",
       subtitle = "2018-2020, min. of 1,500 passes since then",
       caption = "By Tej Seth | @mfbanalytics") +
  annotate("text", x = 0.85, y = 0.35, label = "P.Mahomes", size = 5) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.position = "right"
  ) 
ggsave('mahomes.png', width = 13, height = 10, dpi = 300)
  
