library(ggwordcloud)
library(tidyverse)
library(urbnmapr)
library(janitor)
library(ggplot2)
library(skimr)
library(sf)

calories <- read_csv('data/fastfood_calories.csv') %>%
  clean_names() %>%
  rename(total_prot=protein) %>%
  mutate(restaurant=str_replace_all(tolower(restaurant), '[^\\w]', '')) %>%
  filter(
    !is.na(fiber),
    !is.na(total_prot),
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

# Q2 - No CSV output
calories %>%
  ggplot() +
  geom_density(aes(x=calories)) +
  facet_wrap(~restaurant)

# Q4 - Not useful
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

# Q6 - CSV output
balance_scores <- calories %>%
  mutate(
    score_carb=4*total_carb/calories-0.5,
    score_fat=9*total_fat/calories-0.3,
    score_prot=4*total_prot/calories-0.2,
    balance_score=score_prot-(score_carb+score_fat),
    balance_score=round(balance_score*100, 1)) %>%
  mutate(item_clean=str_replace_all(tolower(item), '[^\\s\\w]', '')) %>%
  mutate(restaurant=case_when(
    restaurant=='mcdonalds'~'McDonald\'s',
    restaurant=='burgerking'~'Burger King',
    restaurant=='tacobell'~'Taco Bell',
    restaurant=='subway'~'Subway')) %>%
  select(restaurant, item, item_clean, calories, balance_score)

balance_scores %>%
  ggplot(aes(x=restaurant, y=balance_score)) +
  geom_boxplot()

balance_scores %>%
  slice_max(order_by=balance_score, n=5)

balance_scores %>%
  slice_min(order_by=balance_score, n=5)

balance_scores %>%
  write_csv('data/balance_scores.csv')

# Q7 - No CSV output
for(r in c('mcdonalds', 'burgerking', 'tacobell', 'subway')) {
  words <- calories %>%
    filter(restaurant==r) %>%
    select(item) %>%
    mutate(item=str_replace_all(tolower(item), '[^\\s\\w]', '')) %>%
    pull()
  
  as.data.frame(table(unlist(strsplit(words, ' ')))) %>%
    rename(item=Var1, n=Freq) %>%
    filter(str_length(item)>1, n>1) %>%
    ggplot(aes(label=item, size=n)) +
    geom_text_wordcloud()
}

#Vegetarian 
calories_veg <- calories %>% 
  mutate(item=str_replace_all(tolower(item), '[^\\s\\w]',''),
        wo = ifelse(grepl('wo chicken', item), 1, 0),
         veggie = ifelse(grepl('veggie', item), 1, 0),
        veggies = ifelse(grepl('veggies', item), 1, 0),
        cheesepizza = ifelse(grepl('cheese pizza', item),1,0),
        cheesequesadilla = ifelse(grepl('cheese quesadilla', item),1,0),
        cheeserollup = ifelse(grepl('cheese rollup', item),1,0),
        bean_burrito = ifelse(grepl('bean burrito', item),1,0),
        tostada = ifelse(grepl('spicy tostada', item),1,0),
        gardensalad = ifelse(grepl('side garden salad', item),1,0),
         total_veg = wo + veggie + veggies + cheesepizza + cheesequesadilla + cheeserollup + bean_burrito + tostada + gardensalad) %>%
  group_by(restaurant) %>% 
  summarize(count = n(),
            sum_veg = sum(total_veg), 
            perc = sum_veg/count * 100) 
calories_veg

#Pescetarian 
calories_pesc <- calories %>% 
  mutate(item=str_replace_all(tolower(item), '[^\\s\\w]',''),
         wo = ifelse(grepl('wo chicken', item), 1, 0),
         veggie = ifelse(grepl('veggie', item), 1, 0),
         veggies = ifelse(grepl('veggies', item), 1, 0),
         cheesepizza = ifelse(grepl('cheese pizza', item),1,0),
         cheesequesadilla = ifelse(grepl('cheese quesadilla', item),1,0),
         cheeserollup = ifelse(grepl('cheese rollup', item),1,0),
         bean_burrito = ifelse(grepl('bean burrito', item),1,0),
         tostada = ifelse(grepl('spicy tostada', item),1,0),
         gardensalad = ifelse(grepl('side garden salad', item),1,0),
         seafood = ifelse(grepl('seafood', item),1,0),
         tuna = ifelse(grepl('tuna', item),1,0),
         fish = ifelse(grepl('fish', item),1,0),
         lobster = ifelse(grepl('lobster', item),1,0),
         total_pesc = wo + veggie + veggies + cheesepizza + cheesequesadilla + cheeserollup + bean_burrito + tostada + gardensalad + seafood + tuna + fish + lobster) %>%
  group_by(restaurant) %>% 
  summarize(count = n(),
            sum_pesc = sum(total_pesc), 
            perc = sum_pesc/count * 100)
calories_pesc
