install.packages("nfl4th")

if (!require("remotes")) install.packages("remotes")
remotes::install_github("guga31bb/nfl4th")

library(nfl4th)
library(tidyverse)
library(gt)
library(dplyr)
library(bayesboot)
library(ggplot2)

data <- nfl4th::load_4th_pbp(2020)

fourth_down <- data %>%
  filter(season == 2020, !is.na(go_boost)) %>%
  arrange(go_boost) %>%
  select(go, posteam, defteam, week, qtr, ydstogo, score_differential, go_boost, desc, vegas_wp)

go <- fourth_down %>%
  filter(go_boost > 0.5) %>%
  filter(vegas_wp > .05) 

lst = list()
teams = unique(go$posteam)
for (team in teams){
  go_team = go %>% filter(posteam == team)
  b <- bayesboot(as.vector(go_team$go), mean)
  s = summary(b)
  mean_go = s$value[1]
  lci = s$value[3]
  uci = s$value[4]
  df = data.frame('mean'=mean_go,'LCI'=lci,'UCI'=uci)
  lst[[team]] = df
}
df = dplyr::bind_rows(lst)
df$posteam = teams
df = df %>% arrange(mean)

df <- df %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

theme_tej <- theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  )

df <- df %>%
  mutate(x_value = 5,
         rank = row_number()) 
  

fourth_down <- df %>%
  ggplot(aes(x=factor(posteam, level = posteam),y=mean)) + 
  geom_pointrange(aes(ymin=(LCI),
                      ymax=(UCI), color = team_color), fatten = 2.5, size = 1.5)+
  scale_color_identity(aesthetics =  c("fill", "color")) +
  geom_image(aes(y = x_value, x = rank, image = team_logo_espn), size = 0.03, asp = 16/9) +
  coord_flip()+
  labs(x = "Team",
       y = "Go Rate",
       title = "Bayesian Bootstrapping for Team's 4th Down Decisions in 2020",
       subtitle = "Each team's go rate when they are recommended to go for it, win prob. between 5% and 95%",
       caption = "By Tej Seth | @mfbanalytics | @benbbaldwin") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_tej 
fourth_down
ggsave('fourth_down2.png', dpi=300, height=12, width=16)

