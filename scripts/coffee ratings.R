library(dplyr)
library(ggplot2)

# read in the data
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

# brief look at the coffee ratings
summary(coffee_ratings$total_cup_points)
coffee_ratings <- coffee_ratings[coffee_ratings$total_cup_points > 0, ]
summary(coffee_ratings$total_cup_points)
sum(is.na(coffee_ratings$total_cup_points))

# factor our categoricals
coffee_ratings$species <- factor(coffee_ratings$species)
coffee_ratings$country_of_origin <- factor(coffee_ratings$country_of_origin)
sum(is.na(coffee_ratings[, c(2, 4)]))
coffee_ratings <- coffee_ratings[!is.na(coffee_ratings$species), ]
coffee_ratings <- coffee_ratings[!is.na(coffee_ratings$country_of_origin), ]
sum(is.na(coffee_ratings[, c(2, 4)]))

# fix the altitude column a bit
coffee_ratings <- coffee_ratings[!is.na(coffee_ratings$altitude_mean_meters), ]
sum(is.na(coffee_ratings$altitude_mean_meters))
summary(coffee_ratings$altitude_mean_meters)
coffee_ratings <- coffee_ratings[coffee_ratings$altitude_mean_meters < 190000, ]
summary(coffee_ratings$altitude_mean_meters)

# Get the average rating and the count by species and country
coffee_type_place <- coffee_ratings %>%
  group_by(country_of_origin, species) %>%
  summarise(points = mean(total_cup_points), number_of_strands = n(), avg_height = mean(altitude_mean_meters)) %>%
  select(points, country_of_origin, species, number_of_strands, avg_height)

summary(coffee_type_place$avg_height)

# Only keep Arabica variants
coffee_type_place <- coffee_type_place[coffee_type_place$species == 'Arabica', ]
coffee_type_place$species <- NULL
coffee_type_place <- coffee_type_place[coffee_type_place$avg_height < 4000, ]

# map each country to its continent
africa <- c('Burundi', 'Cote d?Ivoire', 'Ethiopia', 'Kenya', 'Laos', 'Malawi', 'Mauritius', 'Rwanda',
            'Tanzaniza, United Republic of', 'Uganda', 'Zambia')
south_america <- c('Brazil', 'Colombia', 'Ecuador', 'Peru')
north_central_america <- c('Costa Rica', 'El Salvador', 'Guatemala', 'Haiti', 'Honduras', 'Mexico', 'Panama',
                           'United States', 'United States (Hawaii)', 'United States (Puerto Rico)')

coffee_type_place$continent <- ifelse(coffee_type_place$country_of_origin %in% africa, 'Africa', 
                                      ifelse(coffee_type_place$country_of_origin %in% south_america, 'South America', 
                                             ifelse(coffee_type_place$country_of_origin %in% north_central_america, 'North/Central America', 'Asia')))

# make a scatterplot with regression line, size of bubbles is the number of strands from that country, color is region
ggplot(coffee_type_place, aes(x = avg_height, y = points)) +
  geom_point(aes(x = avg_height, y = points, size = number_of_strands, color = continent)) +
  geom_smooth(method = 'lm', se = FALSE, color = '#619CFF') +
  labs(title = 'Does height and location affect coffee bean quality?', x = 'Average Height',
       y = 'Total Cup Points', size = 'Number of Strands', color = 'Continent')


















