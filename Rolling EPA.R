library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(zoo)
library(gganimate)


pbp_20 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

pbp_rp <- pbp_20 %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

########################################################################
team_colors_logos2 = teams_colors_logos %>% 
  select(team_abbr, team_color, team_logo_wikipedia, team_logo_espn, team_color2) %>%
  unnest(team_logo_espn) %>%
  group_by(team_abbr) %>%
  slice(1) %>% 
  ungroup()

off_epa <- pbp_rp %>%
  group_by(posteam) %>%
  summarize(off_epa = mean(epa, na.rm=T)) %>%
  arrange(desc(off_epa)) %>%
  ungroup() %>%
  mutate(Rank = row_number()) %>% 
  mutate(TeamRank = paste0(posteam, " #", Rank)) %>%
  filter(!is.na(posteam))

team = "DET"
ma_plays = 200

team_off <- pbp_rp %>%
  filter(posteam == team) %>%
  filter(!is.na(epa)) %>%
  mutate(cu_epa = cummean(epa),
         ma_epa=rollapply(epa,ma_plays,mean,align='right',fill=NA),
         play_count = row_number(),
         week_team = paste0("WK", week, " ", defteam))

team_off_play_start <- team_off %>%
  group_by(week_team) %>%
  slice(1) %>%
  select(defteam, week_team, play_count) %>%
  rename(play_start = play_count,
         team = defteam)

team_off_play_stop <- team_off %>%
  group_by(week_team) %>%
  filter(row_number() == n()) %>%
  select(week_team, play_count) %>%
  rename(play_stop = play_count)

team_off_start_stop = team_off_play_start %>%
  left_join(team_off_play_stop, by = "week_team") %>%
  mutate(midpoint = (play_start + play_stop)/2)

play_count = max(team_off$play_count)

team_off_start_stop <- team_off_start_stop %>% 
  left_join(team_colors_logos2, by = c("team" = "team_abbr")) %>% 
  mutate(color = replace_na(team_color,"gray")) %>%
  select(team, week_team, play_start, play_stop, midpoint, color)

team_colors <- as.character(team_off_start_stop$color)
names(team_colors) <- as.character(team_off_start_stop$team)

team_off_start_stop <- team_off_start_stop %>% 
  left_join(team_colors_logos2, by = c("team" = "team_abbr"))

signature = "By Tej Seth | @mfbanalytics"

graph_team_off <- ggplot() +
  geom_rect(data = team_off_start_stop, aes(xmin = play_start, xmax = play_stop, fill = team, ymin = -.5, ymax = .75), color = "gray90", alpha=0.6) +
  geom_rect(data = team_off_start_stop, aes(xmin = play_start, xmax = play_stop, ymin = .65, ymax = 0.85), color = "gray90", fill = "white") +
  scale_fill_manual(values = team_colors) +
  geom_hline(yintercept = quantile(off_epa$off_epa), linetype = 2, color = "gray20", alpha = .8) +
  geom_hline(yintercept = 0, linetype = 1, color = "gray20", alpha = .2) +
  geom_image(data=team_off_start_stop,aes(x=midpoint,y=.73,image=team_logo_espn), asp = 16/9, size = .05) +
  annotate(x = -2, y = quantile(off_epa$off_epa)[1], geom = "text", size = 3, hjust = "right", vjust = 0, label = off_epa %>% slice(n()) %>% pull(posteam)) +
  annotate(x = -2, y = quantile(off_epa$off_epa)[2], geom = "text", size = 3, hjust = "right", vjust = 0, label = "25%ile") +
  annotate(x = -2, y = quantile(off_epa$off_epa)[3], geom = "text", size = 3, hjust = "right", vjust = 0, label = "Median") +
  annotate(x = -2, y = quantile(off_epa$off_epa)[4], geom = "text", size = 3, hjust = "right", vjust = 0, label = "75%ile") +
  annotate(x = -2, y = quantile(off_epa$off_epa)[5], geom = "text", size = 3, hjust = "right", vjust = 0, label = off_epa %>% slice(1) %>% pull(posteam)) +
  geom_line(data = team_off, aes(x = play_count, y = ma_epa), color = "white", size = 3) +
  geom_line(data = team_off, aes(x = play_count, y = ma_epa), size = 1.25) +
  theme_minimal() + theme(panel.grid = element_blank()) + theme(legend.position = "none") +
  ylab("EPA Per Play") + xlab("Number of Plays") +
  labs(title = paste0("Detroit Lions' Offensive EPA | ",ma_plays,"-Play Moving Average"),
       caption = paste0("Chart by ",signature," using code from @cfbNate")) +
  coord_cartesian(xlim = c(-20, play_count),  
                  clip = 'off')

graph_team_off

ggsave(graph_team_off, filename = paste0("russ_died.png"), 
       dpi = 300, type = "cairo", width = 10, height = 7, units = "in")


team_off %>%
  ggplot( aes(x=play_count, y=ma_epa)) +
  geom_line(data = team_off, aes(x = play_count, y = ma_epa), color = "white", size = 3) +
  geom_line(data = team_off, aes(x = play_count, y = ma_epa), size = 1.25) +
  annotate(x = -2, y = quantile(off_epa$off_epa)[1], geom = "text", size = 3, hjust = "right", vjust = 0, label = off_epa %>% slice(n()) %>% pull(posteam)) +
  annotate(x = -2, y = quantile(off_epa$off_epa)[2], geom = "text", size = 3, hjust = "right", vjust = 0, label = "25%ile") +
  annotate(x = -2, y = quantile(off_epa$off_epa)[3], geom = "text", size = 3, hjust = "right", vjust = 0, label = "Median") +
  annotate(x = -2, y = quantile(off_epa$off_epa)[4], geom = "text", size = 3, hjust = "right", vjust = 0, label = "75%ile") +
  annotate(x = -2, y = quantile(off_epa$off_epa)[5], geom = "text", size = 3, hjust = "right", vjust = 0, label = off_epa %>% slice(1) %>% pull(posteam)) +
  labs(x = "Play Count",
       y = "Rolling EPA/Play",
       title = "Detroit Lions 200 Play Rolling EPA/Play Average",
       caption = "By Tej Seth | @mfbanalytics") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "None") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  transition_reveal(play_count)

anim_save("lions_rolling.gif")


