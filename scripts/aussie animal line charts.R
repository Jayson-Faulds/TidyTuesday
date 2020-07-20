library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)

# read in the data
animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')

# some data cleaning
animal_outcomes$animal_type <- factor(animal_outcomes$animal_type)
animal_outcomes$outcome <- factor(animal_outcomes$outcome)
levels(animal_outcomes$outcome)

# keep reclaimed/rehomed rows
animal_outcomes <- animal_outcomes[animal_outcomes$outcome == 'Reclaimed' | animal_outcomes$outcome == 'Rehomed', ]

# aggregate the data
animal_outcomes_new <- animal_outcomes %>% group_by(year, animal_type) %>% summarise(NSW = sum(NSW), QLD = sum(QLD), 
                                                    VIC = sum(VIC), Total = sum(Total))
# filter data a bit
keep <- c('Dogs', 'Cats', 'Livestock')
animal_outcomes_new <- animal_outcomes_new[animal_outcomes_new$animal_type %in% keep, ]

# make the data long
gathered_data <- gather(animal_outcomes_new, key = 'region', value = 'num_animals', -c(year, animal_type)) %>% arrange(year)
gathered_data$region <- factor(gathered_data$region, levels = c('NSW', 'QLD', 'VIC', 'Total'))
gathered_data$region <- revalue(gathered_data$region, c('NSW' = 'New South Wales', 'QLD' = 'Queensland', 'VIC' = 'Victoria', 'Total' = 'Total'))

# make the plot
gathered_data %>% ggplot(aes(x = year, y = num_animals, color = animal_type)) +
  geom_line(size = 1.3) +
  facet_grid(~region) +
  labs(x = 'Year', y = 'Number of animals housed', title = 'Number of animals re-housed over time') +
  theme(legend.title = element_blank())
  





















