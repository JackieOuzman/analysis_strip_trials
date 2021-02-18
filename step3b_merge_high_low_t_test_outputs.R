### spatial data with no yield data
### rainfall
### P and N content
## for this to work I need to have all the output files sitting in one folder.
### probably should add a step in here which looks for results_grower_....SegID_Zone.csv files and copies them over


library(tidyverse)





## yield results
## need to be merged into a single file
#I need to move this into a folder with all the ttest outputs


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


### saved the merged dataframe
baseDir
write.csv(dataset,paste0(baseDir, "/hign_low_t_test_merged.csv") )
