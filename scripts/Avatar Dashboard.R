library(dplyr)
library(ggplot2)
library(gridExtra)

# read in the data
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

# some filtering
avatar <- avatar %>% select(book, book_num, chapter, chapter_num, imdb_rating) %>% unique()

# input a missing value
avatar$imdb_rating[avatar$chapter == 'The Siege of the North, Part 2'] <- 9.7

# aggregate the dataset by book
avatar2 <- avatar %>% group_by(book) %>% summarise(mean_book = mean(imdb_rating))

# line chart of IMDB ratings over time
avatar$episode <- 1:nrow(avatar)
avatar$book <- factor(avatar$book)
hues = seq(15, 375, length = 4)
colors <- hcl(h = hues, l = 65, c = 100)[1:3]
p1 <- avatar %>% ggplot(aes(x = episode, y = imdb_rating, color = book, fill = book)) +
  geom_line() +
  geom_point() +
  xlab('Episode') + ylab('Imdb Rating') +
  theme_minimal() + ylim(6, 10) +
  scale_color_manual(breaks = c('Water', 'Earth', 'Fire'), values = rev(colors), name = NULL) +
  scale_fill_manual(breaks = c('Water', 'Earth', 'Fire'), values = rev(colors), name = NULL) +
  ggtitle('Imdb ratings as the show progresses')
p1

# bar chart of average ratings by book
p2 <- avatar2[c(3, 1, 2), ] %>% ggplot(aes(x = book, y = mean_book, fill = book)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(mean_book, 2)), hjust = -0.2, size = 4) +
  scale_fill_manual(breaks = c('Water', 'Earth', 'Fire'), values = rev(colors), name = NULL) +
  scale_x_discrete(limits = c('Fire', 'Earth', 'Water')) + theme_minimal() +
  ggtitle('Average Imdb ratings by book') + xlab('') + ylab('') +
  ylim(0, 10) + coord_flip()
p2

# combine the two graphs into a dashboard
grid.arrange(p1, p2, ncol = 1)











