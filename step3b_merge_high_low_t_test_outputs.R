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
    "high_low_comparision"
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
  "low" ,
  "medium",
  "high" ,
  "high_vs_low",
  "high_vs_medium",
  "medium_vs_low",
  "Significant",
  "P_value",
  "rounded",
  "paddock_ID",
  "Strip_Type",
  "input_file",
  "rate_low",
  "rate_medium",
  "rate_high",
  "rate_very_high",
  "rate_very_low",
  "se_comp_high_low" ,
  "se_comp_medium_low",
  "se_comp_high_medium"
  
  
)


setwd(baseDir)
file_list <- list.files()
file_list
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
  mutate(ID_analysis_zone_temp = paste0(Zone_ID, "_", comparison, "_", Strip_Type )) %>% 
  distinct(ID_analysis_zone_temp, .keep_all = TRUE)

dataset <- dataset %>% 
  mutate(Significant_practical = case_when(Significant == "significant"  & rounded > 0.1 ~ "significant",
                                           Significant == "significant"  & rounded <= 0.1 ~ "not significant",
                                           Significant == "not significant" ~ "not significant",
                                           is.na(Significant) ~ "NA" ,
                                           TRUE ~ "NA"))

### saved the merged dataframe
outputDir
write.csv(dataset,paste0(outputDir, "/hign_low_t_test_merged_3b.csv") )



### Note working for ?? 31133
file_list
file1 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/high_low_comparision/high_low_comp_31111_P Strip.csv" 
#Alistair Tippett paddocks
file2 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/high_low_comparision/high_low_comp_31115_N Strip.csv"

file3 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/high_low_comparision/high_low_comp_31133_P Strip.csv"
file4 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/high_low_comparision/high_low_comp_31136_N Strip.csv"
file5 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/high_low_comparision/high_low_comp_31211_P Strip.csv" 
file6 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/high_low_comparision/high_low_comp_31214_P Strip.csv" 
file7 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/high_low_comparision/high_low_comp_31215_N Strip.csv"
file8 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/high_low_comparision/high_low_comp_31216_P Strip.csv"

dataset_1 <- read.csv(file1)
dataset_1[clm_headings[!(clm_headings %in% colnames(dataset_1))]] = 'NA'
names(dataset_1)


dataset_2 <- read.csv(file2)
dataset_2[clm_headings[!(clm_headings %in% colnames(dataset_2))]] = 'NA'
names(dataset_2)


test<-rbind(dataset_1, dataset_2)

dataset_3 <- read.csv(file3)
dataset_3[clm_headings[!(clm_headings %in% colnames(dataset_3))]] = 'NA'
names(dataset_3)  
test<-rbind(test, dataset_3)


dataset_4 <- read.csv(file4)
dataset_4[clm_headings[!(clm_headings %in% colnames(dataset_4))]] = 'NA'
names(dataset_4)  
test<-rbind(test, dataset_4)

dataset_5 <- read.csv(file5)
dataset_5[clm_headings[!(clm_headings %in% colnames(dataset_5))]] = 'NA'
names(dataset_5)  
test<-rbind(test, dataset_5)

dataset_6 <- read.csv(file6)
dataset_6[clm_headings[!(clm_headings %in% colnames(dataset_6))]] = 'NA'
names(dataset_6)  
test<-rbind(test, dataset_6)

dataset_7 <- read.csv(file7)
dataset_7[clm_headings[!(clm_headings %in% colnames(dataset_7))]] = 'NA'
names(test)
names(dataset_7)  
test<-rbind(test, dataset_7)

dataset_8 <- read.csv(file8)
dataset_8[clm_headings[!(clm_headings %in% colnames(dataset_8))]] = 'NA'
names(test)
names(dataset_8)  
test<-rbind(test, dataset_8)
