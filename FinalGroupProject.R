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

# Q2
calories %>%
  ggplot() +
  geom_density(aes(x=calories)) +
  facet_wrap(~restaurant)

# Q4
calories_per_restaurant <- calories %>%
  group_by(restaurant) %>%
  summarise(median_calories=median(calories))

calories_per_restaurant <- restaurants %>%
  inner_join(calories_per_restaurant, by=c('name'='restaurant')) %>%
  group_by(state, name) %>%
  summarise(total_calories=n()*median_calories)
  
get_urbn_map('states', sf=T) %>%
  inner_join(calories_per_restaurant, by=c('state_abbv'='state')) %>%
  ggplot(aes(fill=total_calories)) +
  geom_sf()
