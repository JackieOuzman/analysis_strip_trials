### spatial data with no yield data
### rainfall
### P and N content
## for this to work I need to have all the output files sitting in one folder.
### probably should add a step in here which looks for results_grower_....SegID_Zone.csv files and copies them over


library(tidyverse)

rm(dataset)



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


clm_headings <- c(
  "X",
  "Rate",
  "yield",
  "n",
  "sd",
  "se",
  "PtCount_tally",
  "zone",
  "rate_name_order",
  "rate_name",
  "P_value",
  "Mean_diff",
  "rounded",
  "Significant",
  "Details",
  "Start_Fert",
  "Top_Dress",
  "Zone",
  "Zone_ID",
  "paddock_ID_Type",
  "input_file"
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

# dataset <- dataset %>% 
#   mutate(ID_analysis_zone_temp = paste0("Zone_",Zone_ID, "_Rate_", Rate,"_paddock_", paddock_ID_Type, "-", Strip_Type )) %>% 
#   distinct(ID_analysis_zone_temp, .keep_all = TRUE)

### add in sig and yield resposne
names(dataset)
dataset <- dataset %>% 
  mutate(ID_analysis_zone_temp = paste0(Zone_ID, "_", Rate )) %>% 
  distinct(ID_analysis_zone_temp, .keep_all = TRUE)

dataset <- dataset %>% 
  mutate(Significant_practical = case_when(Significant == "significant"  & rounded > 0.1 ~ "significant",
                                           Significant == "significant"  & rounded <= 0.1 ~ "not significant",
                                           Significant == "not significant" ~ "not significant",
                                           is.na(Significant) ~ "NA" ,
                                           TRUE ~ "NA"))
### saved the merged dataframe in 2 spots
outputDir
write.csv(dataset,paste0(outputDir, "/t_test_merged_3a.csv") )
write.csv(dataset,paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files", "/step3a_t_test_merged.csv") )

##############################################################################################################################
#Ummm not saving as expected
#results_grower_33731_P Strip_C_Yld_SegID_Zone.csv
#results_grower_511022_N Strip_Long_N_Yld_SegID_Zone.csv



file_list
file1 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/grower_results/results_grower_33731_P Strip_C_Yld_SegID_Zone.csv"
#falls over at 511022_P
file2 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/grower_results/results_grower_511022_P Strip_Long_P_Yld_SegID_Zone.csv"




dataset_1 <- read.csv(file1)
dataset_1[clm_headings[!(clm_headings %in% colnames(dataset_1))]] = 'NA'
names(dataset_1)


dataset_2 <- read.csv(file2)
dataset_2[clm_headings[!(clm_headings %in% colnames(dataset_2))]] = 'NA'
names(dataset_2)


test<-rbind(dataset_1, dataset_2)

