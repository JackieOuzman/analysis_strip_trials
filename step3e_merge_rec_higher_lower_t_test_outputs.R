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
  "rounded",
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
  "rec_rate_p_label",
  "zone"
  
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
                                           Significant == "significant"  & rounded < 0.1 ~ "not significant",
                                           Significant == "not significant" ~ "not significant",
                                           is.na(Significant) ~ "NA" ,
                                           TRUE ~ "NA"))


### saved the merged dataframe


write.csv(dataset,paste0(outputDir, "/rec_rate_low_high_comparision_t_test_merged_3e.csv") )



### It not working why?


file1 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_33111_P Strip.csv"
file2 <-"W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_52321_P Strip.csv"



# file3 <-"W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_33132_P Strip.csv"
# file4 <-"W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_33142_P Strip.csv"
# file5 <-"W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_52214_P Strip.csv"
# file6 <-"W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_52216_P Strip.csv"
# file7 <-"W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_52231_P Strip.csv"
# file8 <-"W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_52241_P Strip.csv"
# file9 <-"W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_52244_P Strip.csv"
# 



dataset_1 <- read.csv(file1)
dataset_1[clm_headings[!(clm_headings %in% colnames(dataset_1))]] = 'NA'
names(dataset_1)


dataset_2 <- read.csv(file2)
dataset_2[clm_headings[!(clm_headings %in% colnames(dataset_2))]] = 'NA'
names(dataset_2)

dataset_3 <- read.csv(file3)
dataset_3[clm_headings[!(clm_headings %in% colnames(dataset_3))]] = 'NA'
names(dataset_3)

#I was missing a clm added now
dataset_6 <- read.csv(file6)
dataset_6[clm_headings[!(clm_headings %in% colnames(dataset_6))]] = 'NA'
names(dataset_6)
