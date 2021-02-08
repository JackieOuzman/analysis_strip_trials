
library(tidyverse)
for_tables <- read_csv( "W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/economic_analysis2020_2021/results_for_table.csv")


### not sure how to do this???
str(for_tables)
test <- for_tables %>% 
  filter(	 paddock_ID_Type == "31721_N Strip"&
             rate_name_order != "medium") %>% 
  select(ID_analysis, yield, se, Total_sum_N_content, rate_name_order, zone )


## turn the data frame into wide format.
test_wide <- pivot_wider(test, 
                         names_from =rate_name_order, 
                         values_from = yield)
