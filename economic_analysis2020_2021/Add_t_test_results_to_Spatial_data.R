
library(tidyverse)


t_test_results <- read_csv( "C:/Users/ouz001/working_from_home/soil_testing/Streamline/output/t_test_merged.csv")
spatial_data_no_yld <-read_csv("W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/economic_analysis2020_2021/fert_app_select_clm.csv")


str(t_test_results)  
str(spatial_data_no_yld)

#tidy up clm and make a unique ID
test_t_test_results <- t_test_results %>% 
  select(-X1, -X) %>% 
  mutate(ID_analysis = paste0(paddock_ID_Type, "_", Rate))
str(test_t_test_results)    




test_spatial_data_no_yld <- spatial_data_no_yld %>% 
  filter((GSP !="Alt GSP") %>% 
           replace_na(TRUE)) %>%  
  select(-X1) %>% 
  mutate(ID_analysis = paste0(Paddock_ID, "_", Strip_Type, "_",Rate))
str(test_spatial_data_no_yld)

test_spatial_data_no_yld <- test_spatial_data_no_yld %>% 
  select(-Paddock_ID,
         -Rate,
         - GSP,
         -Strip_Type,
         -Start_Fert,
         -Top_Dress)
          ) 


## join the df toghter
join <- left_join(test_t_test_results, test_spatial_data_no_yld)
str(join)

join <- join %>% relocate(ID_analysis, .before = Rate)
