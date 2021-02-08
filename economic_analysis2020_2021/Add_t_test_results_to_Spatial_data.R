
library(tidyverse)


t_test_results <- read_csv( "C:/Users/ouz001/working_from_home/soil_testing/Streamline/output/t_test_merged.csv")
spatial_data_no_yld <-read_csv("W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/economic_analysis2020_2021/fert_app_select_clm.csv")




#tidy up clm and make a unique ID
t_test_results <- t_test_results %>% 
  select(-X1, -X) 
    




spatial_data_no_yld <- spatial_data_no_yld %>% 
  filter((GSP !="Alt GSP") %>% 
           replace_na(TRUE)) %>%  
  select(-X1) %>% 
  mutate(ID_analysis = paste0(Paddock_ID, "_", Strip_Type, "_",Rate))


spatial_data_no_yld <- spatial_data_no_yld %>% 
  select(-Paddock_ID,
         -Rate,
         - GSP,
         -Strip_Type,
         -Start_Fert,
         -Top_Dress)
          


## join the df toghter
join <- left_join(t_test_results, spatial_data_no_yld)


join <- join %>% relocate(ID_analysis, .before = Rate)

write.csv(join, "W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/economic_analysis2020_2021/results_for_table.csv")

          