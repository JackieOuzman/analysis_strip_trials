
library(tidyverse)
library(ggplot2)
#soil testing data This is filtered data we have removed the data that looked suss, but we have still processed it!

df1 <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/for_econmics/GM_t.test_details_rec_rates2019.csv")
df2 <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/for_econmics/GM_t.test_details_rec_rates2020.csv")

df2<- df2 %>%  select(-Rec_N_jax)

df1 <- df1 %>%  mutate(year = 2019)
df2 <- df2 %>%  mutate(year = 2020)

names(df1)
names(df2)


df <- bind_rows(df1,df2)

names(df)
## how many paddocks did this project have?

for_plotting <- df %>% 
  distinct(Paddock_ID, .keep_all = TRUE ) %>% 
  group_by( year, Strip_Type, rainfall_class) %>% 
  summarise(Paddock_ID_zones = n())

for_plotting

for_plotting %>% 
  ggplot(aes(x = rainfall_class, number_zones))+
  geom_col()+
  facet_wrap(Strip_Type~year)
  
for_plotting_wide <- for_plotting %>% 
  pivot_wider(names_from = c(year, Strip_Type),
  values_from = number_zones)


## how many different rates for each zone

df <- df %>% mutate(Paddock_ID_Rate = paste0(Paddock_ID, "_", Rate))
names(df)

how_many_diff_rates_1 <- df %>% 
  distinct(Paddock_ID_Rate,year, .keep_all = TRUE ) %>% # keep only unquie paddoks
  group_by( Zone_ID, Rate, year, Strip_Type, rainfall_class) %>% 
  summarise(Count_rates = n())

how_many_diff_rates_1

how_many_diff_rates <- how_many_diff_rates_1 %>% 
  group_by(year, Strip_Type, rainfall_class) %>% 
  summarise(Count_rates = n())

how_many_diff_rates

how_many_diff_rates_wide <- how_many_diff_rates %>% 
  pivot_wider(names_from = c(year, Strip_Type),
              values_from = Count_rates)
how_many_diff_rates_wide
