### spatial data with no yield data
### rainfall
### P and N content
## for this to work I need to have all the output files sitting in one folder.
### probably should add a step in here which looks for results_grower_....SegID_Zone.csv files and copies them over


library(tidyverse)

## yield results
## need to be merged into a single file
#I need to move this into a folder with all the ttest outputs


baseDir <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "Yield_data",
    "2020",
    "processing",
    "r_outputs",
    "rec_rate_comparision"
  )
outputDir <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "Yield_data",
    "2020",
    "processing",
    "r_outputs",
    "merged_comparision_output"
  )
baseDir
file_list <- paste0(baseDir, "/",list.files(baseDir, ".csv", full.names = FALSE))
file_list



#list of clm headings that I want
clm_headings <- c(
  "X" ,
  "Zone_ID",
  "Zone",
  "comparison",
  "yld_response",
  "higher_than_rec_rate_p",
  "lower_than_rec_rate_p" ,
  "rec_rate_p",
  "rec_rate_p_vs_higher" ,
  "rec_rate_p_vs_lower",
  "se_comp_rec_rate_high_p",
  "se_comp_rec_rate_low_p",
  "Significant",
  "P_value",
  "paddock_ID",
  "Strip_Type",
  "input_file",
  "rate_low",
  "rate_medium",
  "rate_high",
  "rate_very_high",
  "higher_than_rec_rate_p_label",
  "lower_than_rec_rate_p_label",
  "rec_rate_p_label"
  
)




setwd(baseDir)
file_list <- list.files()
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file)
    dataset[clm_headings[!(clm_headings %in% colnames(dataset))]] = 'NA'
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file)
    temp_dataset[clm_headings[!(clm_headings %in% colnames(temp_dataset))]] = 'NA'
    dataset<-rbind(dataset, temp_dataset)
    
    rm(temp_dataset)
  }
}

names(dataset)

dataset <- dataset %>% 
  mutate(ID_analysis_zone_temp = paste0(Zone_ID, "_", comparison )) %>% 
  distinct(ID_analysis_zone_temp, .keep_all = TRUE)


### saved the merged dataframe


write.csv(dataset,paste0(outputDir, "/rec_rate_low_high_comparision_t_test_merged_3e.csv") )
