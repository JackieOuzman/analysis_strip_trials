
library(tidyverse)

baseDir <- file.path("C:","Users", "ouz001", "working_from_home","soil_testing",  "Streamline", "output", "high_low_comparision" )
outputDir <- file.path("C:","Users", "ouz001", "working_from_home","soil_testing",  "Streamline")
baseDir
file_list <- paste0(baseDir, "/",list.files(baseDir, ".csv", full.names = FALSE))
file_list


setwd(baseDir)
file_list <- list.files()
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file)
    dataset<-rbind(dataset, temp_dataset)
    
    rm(temp_dataset)
  }
}

names(dataset)

dataset <- dataset %>% 
  mutate(ID_analysis_zone_temp = paste0(Zone_ID, "_", comparison )) %>% 
  distinct(ID_analysis_zone_temp, .keep_all = TRUE)




























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
