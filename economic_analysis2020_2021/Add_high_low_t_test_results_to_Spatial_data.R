
library(tidyverse)


t_test_results <- read_csv( "C:/Users/ouz001/working_from_home/soil_testing/Streamline/output/high_low_comparision/hign_low_t_test_merged.csv")
spatial_data_no_yld <-read_csv("W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/economic_analysis2020_2021/fert_app_select_clm.csv")



str(t_test_results)
#tidy up clm and make a unique ID
t_test_results <- t_test_results %>% 
  dplyr::select(-X1, -X, -ID_analysis_zone_temp)  %>% 
  mutate(ID_analysis_1 = paste0(Zone_ID, "_",Strip_Type,"_", comparison))
         #ID_analysis = paste0(paddock_ID, "_",Strip_Type))
    



str(spatial_data_no_yld)
spatial_data_no_yld <- spatial_data_no_yld %>% 
  filter((GSP !="Alt GSP") %>% 
           replace_na(TRUE)) %>%  
  dplyr::select(-X1) 



spatial_data_no_yld <- spatial_data_no_yld %>% 
  dplyr::select(Paddock_ID,
                av_rain,
                
                more_info) %>% 
  distinct(Paddock_ID, .keep_all = TRUE)
          


str(spatial_data_no_yld)
str(t_test_results)


## join the df toghter
join <- left_join(t_test_results, spatial_data_no_yld, by= c("paddock_ID" = "Paddock_ID" ))




write.csv(join, "W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/economic_analysis2020_2021/results_for_table_high_low.csv")
str(join)

### ricks table

## for P trials count
high_low_P_N_trials <- join %>% 
  group_by(Strip_Type, comparison, yld_response) %>% 
  summarise(count = n())
  

high_low_P_N_trials
          