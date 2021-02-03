### spatial data with no yield data
### rainfall
### P and N content


base_data <- read_csv("W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/economic_analysis2020_2021/fert_app_select_clm.csv")


## yield results
## need to be merged into a single file
#I need to move this into a folder with all the ttest outputs


baseDir <- file.path("C:","Users", "ouz001", "working_from_home","soil_testing",  "Streamline", "output" )
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


