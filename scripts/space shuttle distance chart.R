library(ggplot2)
library(dplyr)
library(png)
library(cowplot)

# set working directory
setwd("~/R/Tidy Tuesday")

# read in the data
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

# keep US cases, factor the mission title
astronauts <- astronauts[astronauts$nationality == 'U.S.', ]
astronauts$mission_title <- factor(astronauts$mission_title)
summary(astronauts$mission_title)
levels(astronauts$mission_title)
length(levels(astronauts$mission_title))

# there's a bunch of weird mission titles that are just numbers, like "2", "18", etc.
# it looks like some kind of error that we can fix by reassigning it to equal the ascend shuttle name
astronauts$mission_title <- astronauts$ascend_shuttle
class(astronauts$mission_title)
astronauts$mission_title <- factor(astronauts$mission_title)
levels(astronauts$mission_title)
length(levels(astronauts$mission_title)) # 245 different American missions

# Group the data, only keep the mission name, year, and distance traveled
# NOTE: we do not actually have distance traveled, we are just pretending hours_mission is distance because this is for practice
grouped_astronauts <- astronauts %>%
  group_by(mission_title) %>%
  summarize(distance = mean(hours_mission)) %>%
  left_join(astronauts[, c(15, 16)], by = 'mission_title') %>%
  arrange(year_of_mission)

grouped_astronauts <- distinct(grouped_astronauts)

# I'm going to hand-select data, 1 from 60's, 1 from 70's, 2 from 80's, 2 from 90's, 2 from 2000's, 2 from 2010's
mission_list <- c('Gemini 4', 'Apollo 14', 'STS-4', 'STS-28', 'STS-46', 'STS-91', 'STS-111', 'STS-126', 'Soyuz TMA-06M', 'Soyuz MS-15')
remaining_missions <- grouped_astronauts[grouped_astronauts$mission_title %in% mission_list, ]
remaining_missions$year_of_mission <- factor(remaining_missions$year_of_mission)

# building a theme to somewhat resemble outer space
mytheme <- theme(panel.background = element_rect(fill = 'black'),
                 panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 plot.background = element_rect(fill = 'black'),
                 axis.title = element_text(color = 'white'),
                 axis.text = element_text(color = 'white'),
                 plot.title = element_text(color = 'white'))

# import an image of Earth
earth <- readPNG('earth_image.png')

# build the chart
myplot <- ggplot(remaining_missions, aes(x = year_of_mission, y = distance)) +
  geom_bar(stat = 'identity', color = 'white', fill = 'white', width = 0.2) +
  mytheme +
  labs(title = 'Length of American shuttle missions over time') +
  ylim(-1500, 4000)

# attempt to get the earth underneath the plot
# For some reason I cannot get the earth image to stretch, no matter what I do
ggdraw() +
  draw_plot(myplot) +
  draw_image(earth, scale = 0.2, x = -0.3, y = -0.3)

















