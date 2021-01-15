library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(zoo)

pbp_20 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

pbp_rp <- pbp_20 %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_filtered <- pbp_rp %>%
  filter(wp > 0.05 & wp < 0.95)

pass_rate <- pbp_filtered %>%
  filter(down==1 | down==2) %>%
  group_by(posteam) %>%
  summarize(pass_rate = mean(pass))

passers <- pbp_filtered %>%
  group_by(passer_player_name, posteam) %>%
  summarize(passes = n(),
            epa = mean(epa, na.rm=T),
            cpoe = mean(cpoe, na.rm=T)) %>%
  filter(passes > 150) %>%
  filter(!is.na(passer_player_name))

cook <- merge(passers, pass_rate, by="posteam")

cook <- cook %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

transparent <- function(img) {
  magick::image_fx(img, expression = "0.5*a", channel = "alpha")
}

p <- cook %>%
  ggplot(aes(x = pass_rate, y = epa)) +
  geom_hline(yintercept = mean(cook$epa), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(cook$pass_rate), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = team_logo_espn), size = cook$passes / 9000, asp = 16 / 9) +
  geom_text_repel(aes(label=passer_player_name), size=4, point.padding = 0.25) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  labs(x = "Early Down Pass Rate",
       y = "EPA Per Play",
       title = "Quarterbacks That Are Cooking and Those Who Are Door Dashing",
       subtitle = "Win probability between 10% and 90%, min. of 150 passses",
       caption = "By Tej Seth | @mfbanalytics") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "None") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

p + 
  annotate("text", x = 0.64, y = -0.3, label = "Put out the flames", color="blue") +
  annotate("text", x = 0.65, y = 0.37, label = "They're cooking!", color="blue") +
  annotate("text", x = 0.48, y = 0.40, label = "They should be cooking", color="blue") +
  annotate("text", x = 0.48, y = -0.29, label = "They're Door Dashing \n for good reason", color="blue")
  
ggsave('cook4.png', dpi=300)


top_half_defenses <- pbp_rp %>%
  filter(defteam == "LA" | defteam == "PIT" | defteam == "WAS" | defteam == "NO"
         | defteam == "SF" | defteam == "BAL" | defteam == "MIA" | defteam == "BUF"
         | defteam == "GB" | defteam == "IND" | defteam == "TB" | defteam == "SEA"
         | defteam == "ARI" | defteam == "LAC" | defteam == "NYG" | defteam == "PHI")

passers <- pbp_rp %>%
  group_by(passer_player_name, posteam) %>%
  summarize(passes = n(),
            epa = mean(epa, na.rm=T),
            cpoe = mean(cpoe, na.rm=T)) %>%
  filter(passes > 100) %>%
  filter(!is.na(passer_player_name))

top_half_passers <- top_half_defenses %>%
  group_by(passer_player_name, posteam) %>%
  summarize(passes = n(),
            top_epa = mean(epa, na.rm=T),
            top_cpoe = mean(cpoe, na.rm=T)) %>%
  filter(passes > 30) %>%
  filter(!is.na(passer_player_name))

top_half_passers <- merge(passers, top_half_passers, by = c("passer_player_name", "posteam"))

top_half_passers <- top_half_passers %>%
  filter(passes.x > 225) %>%
  filter(passes.y > 50)

top_half_passers <- top_half_passers %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

p2 <- top_half_passers %>%
  ggplot(aes(x = epa, y = top_epa)) +
  geom_hline(yintercept = mean(top_half_passers$top_epa), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(top_half_passers$epa), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = team_logo_espn), size = 0.03, asp = 16 / 9, alpha = 0.3) +
  geom_text_repel(aes(label=passer_player_name), size=4, point.padding = 0.25) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  labs(x = "EPA Per Dropback",
       y = "EPA Per Dropback Against Top Defenses",
       title = "Quarterbacks Against Top-Half Defenses vs. Every Defense",
       subtitle = "Win probability between 5% and 95%, min. of 200 passses",
       caption = "By Tej Seth | @mfbanalytics") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "None") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 
p2

ggsave('fitzmagic.png', dpi=300)

air_yards <- pbp_rp %>%
  group_by(passer, air_yards) %>%
  summarize(passes = n(),
            total_epa = sum(epa, na.rm = T)) %>%
  filter(!is.na(passer)) %>%
  filter(!is.na(air_yards))

air_yards <- air_yards %>%
  left_join(passers, by = c("passer" = "passer_player_name"))

air_yards <- air_yards %>%
  filter(!is.na(posteam))

air_yards %>%
  ggplot(aes(air_yards, passer, fill = passes)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "RdYlGn"), trans = "sqrt") +
  theme_minimal() + theme(panel.grid = element_blank())


