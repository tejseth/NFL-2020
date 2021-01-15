library(tidyverse)
library(ggrepel)
library(ggimage)
library(ggtext)
library(mgcv)
library(scales)
library(ggforce)
library(nflfastR)


pbp_20 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

big_data <- pbp_20 %>%
  filter(!is.na(posteam) & !is.na(epa), season == 2020)

xpass <- big_data %>%
  add_xpass() %>%
  filter(!is.na(pass_oe))

pass_stats <- xpass %>%
  filter(pass == 1) %>%
  group_by(posteam) %>% 
  summarize(pass_plays = n(),
            pass_epa = mean(epa, na.rm = T))

run_stats <- xpass %>%
  filter(rush == 1) %>%
  group_by(posteam) %>%
  summarize(run_plays = n(),
            run_epa = mean(epa, na.rm = T))

xpass_stats <- xpass %>%
  group_by(posteam) %>%
  summarize(epa_per_play = mean(epa, na.rm = T),
            plays = n(),
            xpass_avg = mean(xpass, na.rm = T),
            pass_oe_avg = mean(pass_oe, na.rm = T))

xpass_stats <- xpass_stats %>%
  left_join(run_stats, by = "posteam")

xpass_stats <- xpass_stats %>%
  left_join(pass_stats, by = "posteam")

xpass_stats <- xpass_stats %>%
  mutate(xp_passes = 0.85 * plays,
         xp_runs = (1 - 0.85) * plays)

xpass_stats <- xpass_stats %>%
  mutate(total_epa = (xp_passes * pass_epa) + (xp_runs * run_epa),
         xp_epa = total_epa / plays)

xpass_stats <- xpass_stats %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

xpass_stats %>%
  ggplot(aes(x = epa_per_play, y = xp_epa)) +
  geom_hline(yintercept = mean(xpass_stats$xp_epa), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(xpass_stats$epa_per_play), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = team_logo_espn), size = 0.04, asp = 16 / 9, alpha = 0.3) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  labs(x = "Normal EPA Per Play",
       y = "EPA Per Play At Average Expected Pass Rate",
       title = "What if Every NFL Team Passed on 85% of Their Plays",
       subtitle = "Just for fun: assuming each time has the same EPA/play with high pass rates",
       caption = "By Tej Seth | @mfbanalytics") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "None") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 
ggsave('xp1.png', dpi = 300)

xpass_stats <- xpass_stats %>%
  arrange(desc(epa_per_play)) %>%
  mutate(rank = row_number())

xpass_stats %>%
  ggplot() +
  geom_link(
    mapping = aes(x = epa_per_play, y = rank, xend = xp_epa, yend = rank, size = 2, color = team_color)
  ) +
  theme_bw() +
  scale_colour_identity() +
  geom_image(aes(x = epa_per_play, y = rank, image = team_logo_espn), size = 0.04, asp = 16/9, alpha = 0.3) +
  geom_image(aes(x = xp_epa, y = rank, image = team_logo_espn), size = 0.04, asp = 16/9, alpha = 1.0) +
  labs(
    x = "EPA Per Play",
    y = "",
    title = "What if Every NFL Team Passed at their Expected Rate?",
    subtitle = "If a team's darker logo moves to the right, their offense would have been better and vice versa",
    caption = "By Tej Seth | @mfbanalytics using code from @benbbaldwin"
  ) +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, size = 12),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.border= element_blank()
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10))
ggsave('xp2.png', dpi = 300)


