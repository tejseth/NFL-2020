#if you don't have these packages installed already, run lines 3-6 without the '#'

#install.packages("tidyverse")
#install.packages("ggrepel")
#install.packages("ggimage")
#install.packages("nflfastR")

#loading in packages once they're installed
library(tidyverse) 
library(ggrepel) 
library(ggimage)
library(nflfastR)
library(ggthemes)
library(dplyr)
library(na.tools)
library(gt)

#creating our own theme
theme_reach <- function() {
  theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  )
}

#load in NFL play-by-play data
pbp <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

#look at all the columns
names(pbp)

#cleans the data
pbp_rp <- pbp %>% filter(pass==1 | rush==1)

#get offensive stats
team_off <- pbp_rp %>%
  filter(!is.na(yards_gained)) %>% #filters out any plays that are missing data
  group_by(posteam) %>% #groups each of the 32 teams
  summarize(off_ypp = mean(yards_gained)) #gets the average yards gained per play

#get defensive stats
team_def <- pbp_rp %>%
  filter(!is.na(yards_gained)) %>% 
  group_by(defteam) %>%
  summarize(def_ypp = mean(yards_gained))

#joining the offensive and defensive stats together
teams <- team_off %>%
  left_join(team_def, by = c("posteam" = "defteam"))

#adding in team colors and logos
teams <- teams %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

teams %>%
  ggplot(aes(x = off_ypp, y = def_ypp)) +
  #horizontal line with mean off_ypp
  geom_hline(yintercept = mean(teams$off_ypp), color = "blue", linetype = "dashed", alpha=0.5) +
  #vertical line with mean def_ypp
  geom_vline(xintercept =  mean(teams$def_ypp), color = "blue", linetype = "dashed", alpha=0.5) +
  #add the logos onto the plot
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16 / 9) +
  #adding on labels
  labs(x = "Offensive Yards Per Play",
       y = "Defensive Yards Per Play",
       title = "NFL Teams Yards Per Play in 2020",
       caption = "Data: nflfastR") +
  theme_reach() +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave('nfl_ypp.png', width = 14, height = 10, dpi = 300) #saving your graph

############################################################################

#now let's see which teams have the biggest difference between their passing and running
pass_stats <- pbp_rp %>%
  filter(!is.na(yards_gained)) %>% 
  filter(!is.na(pass_oe)) %>%
  filter(pass == 1) %>%
  group_by(posteam) %>%
  summarize(pass_ypp = mean(yards_gained)) 

run_stats <- pbp_rp %>%
  filter(!is.na(yards_gained)) %>% 
  filter(rush == 1) %>%
  group_by(posteam) %>% 
  summarize(run_ypp = mean(yards_gained))

#this time since we did group_by with posteam both times it'll join automatically
pass_run_stats <- pass_stats %>%
  left_join(run_stats)

pass_run_stats <- pass_run_stats %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

pass_run_stats <- pass_run_stats %>%
  mutate(pass_minus_run = pass_ypp - run_ypp) %>% #mutate makes a new column
  arrange(desc(pass_minus_run)) %>% #putting the teams with the biggest diff at the top
  mutate(rank = row_number())

tab_data <- pass_run_stats %>%
  select(rank, team_wordmark, pass_ypp, run_ypp, pass_minus_run)

tab_data <- tab_data %>%
  mutate_if(is.numeric, ~round(., 1))

ypp_diff <- tab_data %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(vars(team_wordmark)),
    fn = function(x){
      web_image(
        url = x,
        height = px(20)
      )
    }
  ) %>% 
  cols_label(
    rank = "Rank",
    team_wordmark = "Team",
    pass_ypp = "Pass YPP",
    run_ypp = "Rush YPP",
    pass_minus_run = "Diff.") %>%
  data_color(
    columns = vars(pass_minus_run),
    colors = scales::col_numeric(
      palette = c("#af8dc3", "#f7f7f7", "#7fbf7b"),
      domain = c(0, 3.5)
    )
  ) %>% 
  tab_header(
    title = "Each Team's Difference Between Pass and Rush YPP",
    subtitle = "YPP = Yards per Play"
  ) %>%
  tab_options(
    column_labels.background.color = "white",
    column_labels.font.weight = "bold",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    table.font.size = 16,
    heading.align = "middle",
  ) %>%
  opt_table_font(
    font = list(
      default_fonts()
    )
  ) 

gtsave(ypp_diff, "ypp_diff.png")

###########################################################################

#time to do clustering analysis!
library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(ggsci) 
library(broom)
library(igraph)

X <- pass_run_stats %>%
  select(pass_ypp, run_ypp, pass_minus_run)

set.seed(222) # set seed to ensure reproduceability b/c k-means relies on random states for initialization 
MAX_K <- 20 # max number of clusters
sse <- c() # vector to hold SSE of each model

for (k in 1:MAX_K) {
  algo_k <- kmeans(X, centers=k, nstart=22, iter.max=20) # k-means algorithm
  sse <- c(sse, algo_k$tot.withinss) # get SSE
} 

#seeing the rolling SEE
tibble(k = 1:MAX_K, SSE_difference = sse-2*lead(sse)+lead(sse, 2)) %>%
  dplyr::filter(k<MAX_K-1) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  labs(x = "K", y = "SSE Rolling 2-Unit Difference", title = "A Clear Picture") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() + theme_reach() + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

#we should create 4 clusters because that's where it really levels off

set.seed(22)
# re-run K-Means with 4 clusters
K <- 4
kmeans4 <- kmeans(X, centers=K, nstart=22, iter.max=20)
km_centers <- as.data.frame(kmeans4$centers) # SCALED cluster centers/means

km_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                        'Cluster 4') 

km_centers <- km_centers %>%
  rename(c('PYPP'='pass_ypp', 'RYPP'='run_ypp', # give predictors a shorter name for plotting
           'DIFF'='pass_minus_run')) %>% 
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') # pivot data to make plotting easier

team_clusters <- tibble(cluster=kmeans4$cluster, team=pass_run_stats$posteam)

km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster)) + 
  geom_point() + # plot points
  scale_color_brewer(palette="Paired") + # color points
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ Cluster, ncol=2) + # create seperate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Visualizing K-Means Cluster Makeups") + 
  theme_minimal() + theme_reach() + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())  

#cluster 1 has the lowest difference and rember how philly was in the bottom of our plot!

team_clusters <- team_clusters %>%
  left_join(pass_run_stats, by = c("team" = "posteam"))

team_clusters %>%
  ggplot(aes(x = pass_ypp, y = run_ypp)) +
  geom_hline(yintercept = mean(team_clusters$run_ypp), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(team_clusters$pass_ypp), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_point(size = 3, color = team_clusters$cluster, alpha = .8) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=team)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Pass YPP",
       y = "Rush YPP",
       title = "Clustering Analysis of the YPP of NFL Teams",
       caption = "By Tej Seth") +
  #uses the black and white ggplot theme
  theme_reach() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave('ypp_clustering.png', width = 14, height = 10, dpi = 300)




