### spatial data with no yield data
### rainfall
### P and N content
## for this to work I need to have all the output files sitting in one folder.
### probably should add a step in here which looks for results_grower_....SegID_Zone.csv files and copies them over


library(tidyverse)

rm(dataset, temp_dataset, baseDir, file, file_list, outputDir)



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
    "GSP"
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
  "X",
  "Zone_ID" ,
  "yld_response" ,
  "Alt.GSP" ,
  "GSP" ,
  "GSP_vs_Alt_GSP",
  "se_comp_GSP_AltGSP",
  "P_value" ,
  "Mean_diff", 
  "zone",   
  "rounded", 
  "Significant" ,
  "Zone" ,             
  "comparison" ,  
  "paddock_ID",  
  "Strip_Type" , 
  "input_file" ,
  "comparison.y" 
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

dataset <- dataset %>% 
  mutate(Significant_practical = case_when(Significant == "significant"  & rounded > 0.1 ~ "significant",
                                           Significant == "significant"  & rounded <= 0.1 ~ "not significant",
                                           Significant == "not significant" ~ "not significant",
                                           is.na(Significant) ~ "NA" ,
                                           TRUE ~ "NA"))
### saved the merged dataframe

write.csv(dataset,paste0(outputDir, "/GSP_AltGSP_t_test_merged_3c.csv") )




#############################################################################



