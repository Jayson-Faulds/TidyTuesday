library(dplyr)
library(treemap)

# read in the data
plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

#
plants$total_threat <- rowSums(plants[6:16])

# keep what we want and make the aggregation
plants <- plants %>% select(continent, group, total_threat) %>%
  group_by(continent, group) %>%
  summarise(frequency = n(), average_threat = mean(total_threat))
plants

# build the treemap
treemap(plants, index = c('continent', 'group'), vSize = 'frequency', vColor = 'average_threat',
        title = 'Frequency and threat levels of extinction by continent and group', type = 'value')

# honestly the treemap is pretty ugly but I think that's because of the severe imbalance in plant groups that went extinct
# Flowering plants were by far the most frequently extinct plant species

plants <- plants %>% group_by(group) %>%
  summarise(total_count = sum(frequency))
plants

# in fact, its almost 95% of all extinct species
471/colSums(plants[2])
