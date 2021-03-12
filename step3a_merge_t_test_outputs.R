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
    "grower_results"
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
  mutate(ID_analysis_zone_temp = paste0(Zone_ID, "_", Rate )) %>% 
  distinct(ID_analysis_zone_temp, .keep_all = TRUE)

### add in sig and yield resposne
names(dataset)
unique(dataset$Significant)
dataset <- dataset %>% 
  mutate(Significant_practical = case_when(Significant == "significant"  & rounded > 0.1 ~ "significant",
                                           Significant == "not significant" ~ "not significant",
                                           is.na(Significant) ~ "NA" ,
                                           TRUE ~ "not significant"))
names(dataset)
check <- dataset %>%  dplyr::select("Significant", "rounded", "Significant_practical")
check
### saved the merged dataframe in 2 spots
outputDir
write.csv(dataset,paste0(outputDir, "/t_test_merged_3a.csv") )
write.csv(dataset,paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files", "/step3a_t_test_merged.csv") )
