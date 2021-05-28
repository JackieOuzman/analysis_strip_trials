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
file_list_N <- paste0(baseDir, "/",list.files(baseDir, "N Strip.csv", full.names = FALSE))
file_list_N

file_list_P <- paste0(baseDir, "/",list.files(baseDir, "P Strip.csv", full.names = FALSE))
file_list_P

#list of clm headings that I want
clm_headings_P <- c(
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
  "rate_high_high",
  "higher_than_rec_rate_p_label",
  "lower_than_rec_rate_p_label",
  "rec_rate_p_label",
  "zone",
  "rate_very_low"
  # "higher_than_rec_rate_n",
  # "lower_than_rec_rate_n",
  # "rec_rate_n",
  # "rec_rate_n_vs_higher",
  # "rec_rate_n_vs_lower",
  # "se_comp_rec_rate_high_n",
  # "se_comp_rec_rate_low_n",
  # "higher_than_rec_rate_n_label",
  # "lower_than_rec_rate_n_label",
  # "rec_rate_n_label"
  
  
)




setwd(baseDir)
file_list <- list.files()
file_list
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file)
    dataset[clm_headings_P[!(clm_headings_P %in% colnames(dataset))]] = 'NA'
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file)
    temp_dataset[clm_headings_P[!(clm_headings_P %in% colnames(temp_dataset))]] = 'NA'
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


write.csv(dataset,paste0(outputDir, "/rec_rate_low_high_comparision_t_test_merged_3e.csv") )




##########################   For N #########################################

rm(dataset)

baseDir_N <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "Yield_data",
    "2020",
    "processing",
    "r_outputs",
    "rec_rate_comparision_N"
  )

#list of clm headings that I want
clm_headings_N <- c(
  "X" ,
  "Zone_ID",
  "Zone",
  "comparison",
  "yld_response",
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
  "rate_high_high",
  "zone",
  "higher_than_rec_rate_n",
  "lower_than_rec_rate_n",
  #"lower_than_rec_rate_n",
  "Mean_diff",
  
  
  "rec_rate_n",
  "rec_rate_n_vs_higher",
  "rec_rate_n_vs_lower",
  #"rec_rate_n_vs_lower",
  
  "se_comp_rec_rate_high_n",
  "se_comp_rec_rate_low_n",
  "higher_than_rec_rate_n_label",
  #"higher_than_rec_rate_n_label",
  "lower_than_rec_rate_n_label",
  #"lower_than_rec_rate_n_label",
  
  "rec_rate_n_label",
  "rate_very_low"
)

#View(clm_headings_N)

setwd(baseDir_N)
file_list <- list.files()
file_list
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file)
    dataset[clm_headings_N[!(clm_headings_N %in% colnames(dataset))]] = 'NA'
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file)
    temp_dataset[clm_headings_N[!(clm_headings_N %in% colnames(temp_dataset))]] = 'NA'
    dataset<-rbind(dataset, temp_dataset)
    
    rm(temp_dataset)
  }
}


#View(dataset)
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


write.csv(dataset,paste0(outputDir, "/rec_rate_low_high_comparision_t_test_merged_3e_N.csv") )










### It not working why?
file_list


file1 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision_N/rec_rate_comp_52351_N Strip.csv"
file2 <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision_N/rec_rate_comp_52355_N Strip.csv"
file3 <-"W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision_N/rec_rate_comp_52356_N Strip.csv"
file4 <-"W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision_N/rec_rate_comp_524102_N Strip.csv"





setwd(baseDir_N)
getwd()
dataset_1 <- read.csv(file1)
dataset_1[clm_headings_N[!(clm_headings_N %in% colnames(dataset_1))]] = 'NA'
names(dataset_1)


dataset_2 <- read.csv(file2)
dataset_2[clm_headings_N[!(clm_headings_N %in% colnames(dataset_2))]] = 'NA'
names(dataset_2)


test<-rbind(dataset_1, dataset_2)
test$Zone_ID


dataset_3 <- read.csv(file3)
dataset_3[clm_headings_N[!(clm_headings_N %in% colnames(dataset_3))]] = 'NA'
names(dataset_3)

test<-rbind(test, dataset_3)

# dataset_3 <- read.csv(file3)
# dataset_3[clm_headings[!(clm_headings %in% colnames(dataset_3))]] = 'NA'
# names(dataset_3)

#I was missing a clm added now
# dataset_6 <- read.csv(file6)
# dataset_6[clm_headings[!(clm_headings %in% colnames(dataset_6))]] = 'NA'
# names(dataset_6)
