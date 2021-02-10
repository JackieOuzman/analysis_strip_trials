
library(tidyverse)
for_tables <- read_csv( "W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/economic_analysis2020_2021/results_for_table.csv")


### not sure how to do this???
str(for_tables)
test <- for_tables %>% 
  filter(	 paddock_ID_Type == "31721_N Strip"&
             rate_name_order != "medium") %>% 
  select( yield, se, Total_sum_N_content, rate_name_order, paddock_ID_Zone )
  #select(ID_analysis, yield, se, Total_sum_N_content, rate_name_order, zone, paddock_ID_Zone )
str(test)

## turn the data frame into wide format.
test_wide <- pivot_wider(test, 
                         id_cols = paddock_ID_Zone,
                         names_from =rate_name_order,
                         values_from = c(yield, se)
                         )



## add the difference calulation to see if the added fertilier has a increase in yield
str(test_wide)
test_wide <- test_wide %>% 
  mutate(yield_response = yield_high - yield_low)
