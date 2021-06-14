#tidytuesdayR::use_tidytemplate()

library(tidyverse)
library(scales)
library(skimr)
library(tidytext)
library(snakecase)

tt <- tidytuesdayR::tt_load("2021-04-20")

netflix <- tt$netflix_titles %>% 
  separate(duration, c("duration", "unit"), sep = " ", convert = TRUE)


theme_set(theme_light())

#---- This dataset consists of tv shows and movies available on Netflix as of 2019. ----

skim(netflix)

netflix %>% 
  count(type)

netflix %>% 
  count(release_year) %>% 
  arrange(-n)

netflix %>% 
  summarise(min(release_year), max(release_year))


#----

netflix %>% 
  ggplot(aes(release_year)) +
  geom_histogram(color = "#69869C", fill  = "#C3CED7", binwidth = 1, center = 0) +
  facet_wrap(~type) +
  scale_x_continuous(n.breaks = 10) + 
  labs(x = NULL, y = NULL, 
       title = "Number of Netflix titles by release year")




title_frequency <- netflix %>% 
  count(release_year, type) %>% 
  arrange(-n) %>% 
  group_by(type) %>% 
  mutate(percent = n/sum(n)) 

title_frequency %>% 
  ggplot(aes(release_year, percent, color = type)) + 
  geom_line() +
  geom_point(size = 0.5) +
  scale_x_continuous(n.breaks = 8) + 
  scale_y_continuous(labels = percent_format()) +
  labs(x = NULL, y = "Percentage of titles by type", color = "Type",
       title = "Number of Netflix titles by release year")


#----

country_movies <- netflix %>% 
  count(country) %>% 
  separate_rows(country, sep = ", ") %>% 
  drop_na() %>% 
  group_by(country) %>% 
  summarise(total = sum(n)) %>%
  mutate(percent = total/sum(total)) %>% 
  slice_max(order_by = total, prop = 0.4)


country_movies %>% 
  ggplot(aes(reorder(country, percent), percent)) +
  geom_col(color = "#69869C", fill  = "#C3CED7") +
  coord_flip() +
  scale_y_continuous(n.breaks = 8, labels = percent_format()) +
  labs(x = NULL, y = NULL, 
       title = "Proportion of movies produced by each country",
       subtitle = paste0("Since the vast marjority of countries ",
       "present values close to 0, only the top 40% are shown"))


#----

netflix %>% 
  filter(type == "Movie") %>% 
  mutate(decade = 10 * (release_year %/% 10),
         decade = as.character(decade)) %>% 
  # Decade is not a continuous variable, so change it from numeric to character.
  # Otherwise, it is needed to pass the "group" argument to geom_boxplot().
  select(decade, duration, title) %>% 
  ggplot(aes(decade, duration)) +
  geom_boxplot() +
  labs(x = NULL, y = "Duration (in minutes)") +
  ggtitle("Films duration and the decades they were released in")


#----

netflix %>% 
  unnest_tokens(word, description) %>% 
  anti_join(stop_words, by = "word") %>% # Discards English stop words.
  count(type, word, sort = TRUE) %>% 
  pivot_wider(names_from = type, values_from = n) %>% 
  rename_with(snakecase::to_snake_case) %>% 
  mutate(total = movie + tv_show) %>% 
  slice_max(order_by = total, prop = 0.001) %>% 
  ggplot(aes(movie, tv_show)) +
  geom_point() +
  geom_text(aes(label = word), hjust = -0.25, vjust = 0, color = "#69869C")




  
