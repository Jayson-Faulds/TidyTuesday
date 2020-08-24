library(ggplot2)
library(tidyverse)

# read in the data
chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

# clean the data
chopped <- chopped %>% select(season, episode_rating) %>%
  filter(season <= 40) %>%
  group_by(season) %>%
  summarise(season_avg = mean(episode_rating, na.rm = TRUE))

# make a zero variable for the step plot
chopped$true_season <- chopped$season - 1

# group the seasons by 10: there should be 4 groups of 10
chopped <- chopped %>% 
  mutate(season_batch = case_when(
    between(season, 1, 10) ~ 1,
    between(season, 11, 20) ~ 2,
    between(season, 21, 30) ~ 3,
    between(season, 31, 40) ~ 4
  ))

# get the mean rating for the batches
chopped <- chopped %>%
  group_by(season_batch) %>%
  mutate(batch_avg = mean(season_avg))

# building the step plot
chopped$season_batch <- factor(chopped$season_batch)
hues = seq(15, 375, length = 5)
colors <- hcl(h = hues, l = 65, c = 100)[1:4]

chopped %>% ggplot(aes(x = season, y = season_avg)) +
  geom_step(aes(y = batch_avg, x = true_season), size = 1) + 
  geom_segment(aes(yend = batch_avg, xend = season), linetype = 2) +
  geom_point(aes(color = season_batch)) +
  geom_segment(
    data = chopped %>% filter(season_batch == "1"),
    aes(
      x = min(true_season),
      xend = max(season),
      y = batch_avg,
      yend = batch_avg
    ),
    size = 2,
    color = colors[1]
  ) +
  geom_segment(
    data = chopped %>% filter(season_batch == "2"),
    aes(
      x = min(true_season),
      xend = max(season),
      y = batch_avg,
      yend = batch_avg
    ),
    size = 2,
    color = colors[2]
  ) +
  geom_segment(
    data = chopped %>% filter(season_batch == "3"),
    aes(
      x = min(true_season),
      xend = max(season),
      y = batch_avg,
      yend = batch_avg
    ),
    size = 2,
    color = colors[3]
  ) +
  geom_segment(
    data = chopped %>% filter(season_batch == "4"),
    aes(
      x = min(true_season),
      xend = max(season),
      y = batch_avg,
      yend = batch_avg
    ),
    size = 2,
    color = colors[4]
  ) +
  ggtitle('Looking at IMDB ratings for each Chopped season')

# we can see the average rating per season decline over time, but still remain respectably high
# the variance in season ratings also increases over time, with the last batch being pretty volatile




















