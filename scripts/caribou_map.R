# read in the data
individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

# some basic explorations/cleaning for individuals
sapply(individuals, function(x) sum(is.na(x)))
sapply(individuals, function(x) sum(is.na(x))/nrow(individuals)) # almost everything is missing
summary(individuals)

# factor the character variables
individuals$sex <- factor(individuals$sex)
individuals$death_cause <- factor(individuals$death_cause)
individuals$study_site <- factor(individuals$study_site)
individuals$deploy_off_type <- factor(individuals$deploy_off_type)
individuals$life_stage <- factor(individuals$life_stage)
summary(individuals)

# find the number of completed trips
individuals$full_trip <- ifelse(!is.na(individuals$deploy_on_longitude) & !is.na(individuals$deploy_off_longitude), 1, 0)
sum(individuals$full_trip) # we have 51 caribou that have made a full trip
complete_trips <- individuals[individuals$full_trip == 1, ]

# get the coordinates for the map window
min(complete_trips$deploy_on_longitude)
min(complete_trips$deploy_off_longitude)
west_point <- -123.6159
max(complete_trips$deploy_on_longitude)
max(complete_trips$deploy_off_longitude)
east_point <- -119.8035
min(complete_trips$deploy_on_latitude)
min(complete_trips$deploy_off_latitude)
south_point <- 54.61527
max(complete_trips$deploy_on_latitude)
max(complete_trips$deploy_off_latitude)
north_point <- 55.94577
mapbox <- c(west_point, south_point, east_point, north_point)

# make the map
library(ggmap)
canada <- get_map(location = mapbox, source = 'stamen', maptype = 'toner')
ggmap(canada)

# add the deploy_on coordinates
ggmap(canada) +
  geom_point(data = complete_trips, aes(x = deploy_on_longitude, y = deploy_on_latitude), color = 'white') +
  geom_point(data = complete_trips, aes(x = deploy_off_longitude, y = deploy_off_latitude), color = 'turquoise2')

# let's look at the path of one caribou
locations$month <- months(locations$timestamp)
locations$day <- format(locations$timestamp, format = '%d')
GR_C01_route <- locations[locations$animal_id == 'GR_C01', ]
GR_C01_route <- GR_C01_route[GR_C01_route$day == '15', ]
GR_C01_route$season <- factor(GR_C01_route$season)

# get the new mapbox
min(GR_C01_route$longitude)
west_point <- -122.85
max(GR_C01_route$longitude)
east_point <- -122.25
min(GR_C01_route$latitude)
south_point <- 56.2
max(GR_C01_route$latitude)
north_point <- 56.4
mapbox <- c(west_point, south_point, east_point, north_point)
newmap <- get_map(location = mapbox, source = 'stamen', maptype = 'toner')

# plot the route
ggmap(newmap) + 
  geom_point(data = GR_C01_route[38:nrow(GR_C01_route), ], aes(x = longitude, y = latitude, pch = season, color = season), size = 2) +
  geom_path(data = GR_C01_route[38:nrow(GR_C01_route), ], aes(x = longitude, y = latitude), size = 1) +
  labs(title = 'Path of Caribou GR_C01')

ggmap(newmap) + 
  geom_point(data = GR_C01_route, aes(x = longitude, y = latitude, pch = season, color = season), size = 2) +
  geom_path(data = GR_C01_route, aes(x = longitude, y = latitude), size = 1) +
  labs(title = 'Path of Caribou GR_C01')

test <- get_map('British Colombia')






