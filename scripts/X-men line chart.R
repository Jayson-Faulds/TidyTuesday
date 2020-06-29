library(ggplot2)
library(dplyr)

# read in the data
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')
issue_collaborators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/issue_collaborators.csv')

# basic exploration
head(characters)
unique(characters$character)

# idea: plot the characters that get into the most trouble (hurt, damaged, etc.)
# idea: plot time on x-axis, number of "edgy" events on y-axis, see if the comic got progressively more intense or something
# maybe go with the second plot, and color/shape the data points based on who the editor is

# some more exploration
unique(characters$issue) # issues go from 97 to 280
unique(issue_collaborators$issue) # this one goes from 97 to 279

# drop a bunch of variables that aren't very edgy
characters <- subset(characters, select = -c(character, redressed, expresses_reluctance_to_fight, 
                                             hand_holding_with_which_character, dancing_with_which_character,
                                             flying_with_another_character, arm_in_arm_with_which_character,
                                             hugging_with_which_character, carrying_with_which_character,
                                             shared_room_domestically_with_which_character,
                                             shower_number_of_panels_shower_lasts, bath_number_of_panels_bath_lasts,
                                             depicted_eating_food, special_notes))

# look at the data types
summary(characters)

# get rid of NAs and convert to integers
characters$initiates_physical_conflict <- ifelse(is.na(characters$initiates_physical_conflict), 0, 1)
characters$on_a_date_with_which_character <- ifelse(is.na(characters$on_a_date_with_which_character), 0, 1)
characters$kiss_with_which_character <- ifelse(is.na(characters$kiss_with_which_character), 0, 1)
characters$physical_contact_other <- ifelse(is.na(characters$physical_contact_other), 0, 1)
characters$shared_bed_with_which_character <- ifelse(is.na(characters$shared_bed_with_which_character), 0, 1)
characters$explicitly_states_i_love_you_to_whom <- ifelse(is.na(characters$explicitly_states_i_love_you_to_whom), 0, 1)
characters$shared_undress <- ifelse(is.na(characters$shared_undress), 0, 1)
sum(is.na(characters))

# fix one of the rows that made our numbers a little crazy
characters$number_of_kills_non_humans[characters$number_of_kills_non_humans > 100] <- 10

# get the sum of edgy events
characters$total_edgy <- rowSums(characters[, 2:20])
agg_chars <- aggregate(characters$total_edgy, by = list(issue = characters$issue), FUN = sum)

# need to join the characters and issue_collaborators dataframes
df <- inner_join(agg_chars, issue_collaborators, on = ('ssue'))
df <- df[, 1:3]
df <- unique(df)
df$editor_in_chief <- factor(df$editor_in_chief)

ggplot(df, aes(x = issue, y = x, color = editor_in_chief, shape = editor_in_chief)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(title = 'Edginess of the X-men Comic Over Time', y = 'Number of "Edgy" Occurrences', x = 'Issue No.') +
  scale_color_discrete(name = 'Editor in Chief') +
  scale_shape_discrete(name = 'Editor in Chief')

### Data Reference

# Thanks to Malcolm Barret for providing the data courtesy of The Claremont Project (@ClaremontRun)
