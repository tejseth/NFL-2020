def_epa_by_quarter <- pbp_rp %>%
  filter(qtr < 5) %>%
  group_by(defteam, qtr) %>%
  summarize(
    def_plays = n(),
    def_epa_per_play = sum(epa) / def_plays)

off_epa_by_quarter <- pbp_rp %>%
  filter(qtr < 5) %>%
  group_by(posteam, qtr) %>%
  summarize(
    off_plays = n(),
    off_epa_per_play = sum(epa) / off_plays) 

names(off_epa_by_quarter)[names(off_epa_by_quarter) == "posteam"] <- "team"
names(def_epa_by_quarter)[names(def_epa_by_quarter) == "defteam"] <- "team"

epa_by_quarter <- merge(off_epa_by_quarter, def_epa_by_quarter, merge=c("team", "qtr"))

epa_by_quarter <- epa_by_quarter %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

epa_by_quarter %>%
  ggplot(aes(x = off_epa_per_play, y = def_epa_per_play)) +
  geom_hline(yintercept = mean(epa_by_quarter$def_epa_per_play), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(epa_by_quarter$off_epa_per_play), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  labs(x = "Offensive EPA/Play",
       y = "Defensive EPA/Play",
       title = "Each Team's EPA Per Play By Quarter Through Week 17",
       subtitle = "",
       caption = "By Tej Seth | @mfbanalytics") +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(~qtr)
ggsave('week_17.png', dpi=300)

######################################################
def_epa_by_down <- pbp_rp %>%
  filter(down < 4) %>%
  group_by(defteam, down) %>%
  summarize(
    def_plays = n(),
    def_epa_per_play = sum(epa) / def_plays)

off_epa_by_down <- pbp_rp %>%
  filter(qtr < 4) %>%
  group_by(posteam, down) %>%
  summarize(
    off_plays = n(),
    off_epa_per_play = sum(epa) / off_plays) 

names(off_epa_by_down)[names(off_epa_by_down) == "posteam"] <- "team"
names(def_epa_by_down)[names(def_epa_by_down) == "defteam"] <- "team"

epa_by_down <- merge(off_epa_by_down, def_epa_by_down, merge=c("team", "qtr"))

epa_by_down <- epa_by_down %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

epa_by_down %>%
  ggplot(aes(x = off_epa_per_play, y = def_epa_per_play)) +
  geom_hline(yintercept = mean(epa_by_quarter$def_epa_per_play), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(epa_by_quarter$off_epa_per_play), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  labs(x = "Offensive EPA/Play",
       y = "Defensive EPA/Play",
       title = "Each Team's EPA Per Play By Down",
       subtitle = "",
       caption = "By Tej Seth | @mfbanalytics") +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_grid(down ~ .)
ggsave('week17_2.png', dpi = 300)
