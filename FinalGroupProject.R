library(tidyverse)
library(urbnmapr)
library(janitor)
library(ggplot2)
library(skimr)
library(sf)

calories <- read_csv('data/fastfood_calories.csv') %>%
  clean_names() %>%
  mutate(restaurant=str_replace_all(tolower(restaurant), '[^\\w]', '')) %>%
  filter(
    !is.na(fiber),
    !is.na(protein),
    restaurant %in% c('mcdonalds', 'burgerking', 'tacobell', 'subway')) %>%
  select(-x1, -vit_a, -vit_c, -calcium, -salad)

restaurants <- read_csv('data/FastFoodRestaurants.csv') %>%
  clean_names() %>%
  mutate(
    state=if_else(province == 'Co Spgs', 'CO', province),
    name=str_replace_all(tolower(name), '[^\\w]', '')) %>%
  filter(name %in% unique(calories$restaurant)) %>%
  select(-address, -country, -keys, -postal_code, -province, -websites)

abbrevs <- read_csv('data/state-abbrevs.csv') %>%
  clean_names()

population <- read_csv('data/us_pop.csv') %>%
  clean_names() %>%
  left_join(abbrevs, by='state') %>%
  select(-state) %>%
  rename(state=abbreviation)

# Q2
calories %>%
  ggplot() +
  geom_density(aes(x=calories)) +
  facet_wrap(~restaurant)

# Q4
calories_per_restaurant <- calories %>%
  group_by(restaurant) %>%
  summarise(median_calories=median(calories))

restaurant_with_calories <- restaurants %>%
  inner_join(calories_per_restaurant, by=c('name'='restaurant')) %>%
  group_by(state, median_calories) %>%
  count() %>%
  mutate(total_calories=n*median_calories) %>%
  group_by(state) %>%
  summarise(total_calories=sum(total_calories)) %>%
  inner_join(population, by='state') %>%
  mutate(total_calories=total_calories/population) %>%
  select(-population)

get_urbn_map('states', sf=T) %>%
  inner_join(restaurant_with_calories, by=c('state_abbv'='state')) %>%
  ggplot(aes(fill=total_calories)) +
  geom_sf(colour='#ffffff')
