load("cars_df.RData")

library(tidyverse)

# save(list = c("cars_df"), file = "cars_df.RData")

cars_df %>% 
  glimpse()

cars_cleaned_df <- cars_df %>% 
  janitor::clean_names() %>% 
  transmute(
    # price = str_remove_all(vetelar, "\\D"),
    price_eur = str_remove_all(vetelar_eur, "\\D"),
    year = gsub(pattern = "/.*", replacement = "", evjarat),
    fuel = uzemanyag,
    distance = kilometerora_allasa,
    distance = str_remove_all(distance, "\\D"),
    gear_type = sebessegvalto_fajtaja,
    gear_type = str_detect(gear_type, "Automata"),
    max_ppl = szallithato_szem_szama,
    max_ppl = str_remove_all(max_ppl, "\\D")
  ) %>% 
  mutate_at(vars(price_eur, year, distance, max_ppl), 
            as.numeric)

cars_cleaned_df %>% 
visdat::vis_miss()

cars_cleaned_df %>% 
  replace_na(list(distance = 0)) # replace missing values with an explicit value

cars_cleaned_df %>% 
  mutate(
    distance = ifelse(is.na(distance), mean(distance, na.rm = TRUE), distance)
  )

cars_cleaned_df %>% 
  mutate(
    distance = ifelse(is.na(distance), median(distance, na.rm = TRUE), distance)
  )

cars_cleaned_df %>% 
  mutate_if(
    is.numeric, ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  )

mice::mice(cars_cleaned_df, method = "rf") %>% 
  complete() %>% 
  tibble()

m <- mean(cars_cleaned_df$price_eur, na.rm = T)
s <- sd(cars_cleaned_df$price_eur, na.rm = T)

ggplot(cars_cleaned_df) +
  aes(price_eur) +
  geom_density(aes(color = "True value")) +
  stat_function(geom = "line", fun = dnorm, args = list(m, s))

# simulation























  
  
  
  
  
  
  
  
  
  
  







