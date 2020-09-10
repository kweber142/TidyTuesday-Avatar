# Getting the data ready

library(tidyverse)
library(readr)
library(tvthemes)
library(knitr)
library(tidytext)
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
scenes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')

avatar$character <- str_replace_all(avatar$character, "King Bumi", "Bumi")
avatar$character <- str_replace_all(avatar$character, "Avatar Roku", "Roku")
avatar$character <- str_replace_all(avatar$character, "Aang:", "Aang")

# Cleaning up and organizing the data in a table
# Analyzing how many times each of 7 main characters were mentioned

a2 <- avatar %>%
  filter(!is.na(character_words)) %>%
  unnest_tokens(word, character_words) %>%
  count(word) %>%
  filter(word %in% c("aang", "katara", "sokka", "iroh", "zuko", "toph", "azula", "uncle")) %>%
  mutate(word = str_to_title(word)) %>%
  pivot_wider(names_from = word, values_from=n) %>% 
  mutate(`Uncle Iroh`= Iroh+Uncle) %>%  # Zuko often refers to Iroh as "Uncle", so I decided to combine "uncle" and "iroh" for "Uncle Iroh"
  select(-c(Uncle, Iroh)) %>% 
  pivot_longer(cols = Aang:`Uncle Iroh`, names_to = "Character", values_to = "Instances") %>%
  arrange(desc(Instances))

# Creating an Avatar-themed bar graph to show the results

ggplot(a2, aes(fct_reorder(Character, Instances), Instances, fill = Character)) +
  geom_col() +
  coord_flip() +
  scale_fill_avatar() +
  theme_avatar() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10)
  ) +
  labs(fill = NULL,
      title = "Avatar: the Last Airbender",
      subtitle = "Frequency of Character Names")






