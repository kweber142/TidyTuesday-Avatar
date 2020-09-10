# Packages and data for analysis

library(tidyverse)
library(readr)
library(tvthemes)
library(ggimage)
library(knitr)
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
scenes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')

avatar$character <- str_replace_all(avatar$character, "King Bumi", "Bumi")
avatar$character <- str_replace_all(avatar$character, "Avatar Roku", "Roku")
avatar$character <- str_replace_all(avatar$character, "Aang:", "Aang")

# Cleaninng up data to analyze who is talked about more: momo or appa

a1 <- avatar %>%
  filter(!is.na(character_words)) %>%
  unnest_tokens(word, character_words) %>%
  count(word) %>%
  filter(word %in% c("momo", "appa")) %>%
  mutate(word = str_to_title(word), Animal = 0)

# Creating the plot

ggplot(a1, aes(Animal, n, fill = word)) +
  geom_col(position = "fill") +
  geom_text(aes(label = n), position = position_fill(vjust = 0.5), color = "#DBC5A0",
            size = 5) +
  xlim(-1,1) +
  coord_flip() +
  theme_avatar(legend.text.size = 10) +
  theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 10),
        plot.subtitle = element_text(size = 9)
        ) +
  labs(fill = NULL,
       title = "Avatar: The Last Airbender",
       subtitle = "Appa and Momo Name Frequency") +
  scale_fill_manual(values = c("#C24841", "#8B5B45")) +
  scale_y_reverse()
    




