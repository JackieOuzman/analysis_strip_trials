#Collating zone analsyis results

#1. install the lib that I will be using for this work

libs <- c("dplyr", "tidyverse", "stringr",
          "ggplot2", "readxl",
          "PairedData", "cowplot", "grid", 
          "RGraphics", 
          "gridExtra", "rdrop2", "readr")

install.libraries <- function(lib=NULL){
  new <- lib[!(lib %in% installed.packages()[, "Package"])]
  if (length(new)){   
    install.packages(new, dependencies = TRUE)
  }
} 

load.libraries <- function(lib=NULL){
  sapply(libs, require, character.only = TRUE)
}

install.libraries(libs)
load.libraries(libs)

#bring in th zone analysis results for each site.
#what are the files that have been finished?
getwd()
path_finished_wk <- "W:/value_soil_testing_prj/Yield_data/finished/"
#path_finished_wk <- "C:/Users/ouz001/Desktop/soil_testing_proj_full/finished/"
setwd(path_finished_wk)

 my_list_non_landmark_files <- grep(list.files(path = path_finished_wk),
                                      pattern = "^[Landmark]", inv=T, value=T)

 my_list_Vic_Ind <- grep(my_list_non_landmark_files,
                                    pattern = "Vic_Ind", value = T)
 my_list_SA_Ind <- grep(my_list_non_landmark_files,
                         pattern = "SA_Ind", value = T)
 
 my_list_MSF <- grep(my_list_non_landmark_files,
                        pattern = "MSF", value = T)
 
 
 
 my_list_non_landmark_files
 my_list_Vic_Ind
 my_list_SA_Ind
 my_list_MSF
 
 
 
 
 # my_list_MSF_sub <- my_list_MSF[1:10]
 # # ####Some files are missing paddock info so I need to add these now - oops!
 #  modify <- read.csv("MSF_Michael_Moodie_Todd_McDonald_Nulty_Kirbys__zones.csv")
 #  modify <- modify %>% 
 #    mutate(Paddock_tested = "Nulty_Kirbys")
 #  write_csv(modify,"MSF_Michael_Moodie_Todd_McDonald_Nulty_Kirbys__zones.csv")
 
 
 MSF <- 
   do.call("rbind", 
           lapply(my_list_MSF, #this is a list of files
                  function(x) 
                    read.csv(paste(path_finished_wk, x, sep=''), 
                             stringsAsFactors = FALSE)))
 
 
Vic_Ind <- 
  do.call("rbind", 
          lapply(my_list_Vic_Ind, #this is a list of files
                 function(x) 
                   read.csv(paste(path_finished_wk, x, sep=''), 
                            stringsAsFactors = FALSE)))

SA_Ind <- 
  do.call("rbind", 
          lapply(my_list_SA_Ind, #this is a list of files
                 function(x) 
                   read.csv(paste(path_finished_wk, x, sep=''), 
                            stringsAsFactors = FALSE)))

names(Vic_Ind) #paddock should be Paddock_tested
names(SA_Ind) #paddock should be Paddock_tested
names(MSF)


# 
# ####Some files are missing paddock info and other info so I need to add these now - oops!

 Vic_Ind <- Vic_Ind %>% 
   mutate(Details = "not confirmed yet",
          Starter_Feriliser = "not confirmed yet",
          Topdress = "not supplied yet")
 Vic_Ind <- Vic_Ind %>% 
   rename(
     Paddock_tested = Paddock)
          

 SA_Ind <- SA_Ind %>% 
   mutate(Details = "not confirmed yet",
          Starter_Feriliser = "not confirmed yet",
          Topdress = "not supplied yet")
 SA_Ind <- SA_Ind %>% 
   rename(
     Paddock_tested = Paddock)
 
 MSF <- MSF %>% 
   mutate(Details = "not confirmed yet",
          Starter_Feriliser = "not confirmed yet",
          Topdress = "not supplied yet")



Non_Landmark <- rbind(Vic_Ind, SA_Ind, MSF)



write.csv(Non_Landmark, paste0(path_finished_wk,
                       "complied",
                       "/",
                       "Non_Landmark",
                       Sys.Date(),
                       "_For_TM.csv"))

#############################################################################################################################

### access file that has paddock code and chcek that these are correct.
getwd()
Non_and_Landmark_2020_06_04_paddock_code_only <- read_csv("complied/Landmark_and_non_Landmark2020-06-04_For_TM_paddock_code_only.csv")


Non_and_Landmark_2020_06_04_paddock_code_only <- mutate(Non_and_Landmark_2020_06_04_paddock_code_only,
                                                       temp_ID = paste0(str_sub(Non_and_Landmark_2020_06_04_paddock_code_only$Contact,1,),
                                                                        "_",
                                                                        str_sub(Non_and_Landmark_2020_06_04_paddock_code_only$Farmer,1,),
                                                                        "_",
                                                                        str_sub(Non_and_Landmark_2020_06_04_paddock_code_only$Paddock_tested,1,),
                                                                        "_",
                                                                        str_sub(Non_and_Landmark_2020_06_04_paddock_code_only$Zone,1,)
                                                       ))

#Append to results and check
Non_landmark_results <- read_csv("W:/value_soil_testing_prj/Yield_data/finished/complied/Non_Landmark2020-06-23_For_TM.csv")
Non_landmark_results <- mutate(Non_landmark_results,
                           temp_ID = paste0(str_sub(Non_landmark_results$Contact,1,),
                                            "_",
                                            str_sub(Non_landmark_results$Farmer,1,),
                                            "_",
                                            str_sub(Non_landmark_results$Paddock_tested,1,),
                                            "_",
                                            str_sub(Non_landmark_results$Zone,1,)
                           ))


str(Non_and_Landmark_2020_06_04_paddock_code_only)
str(Non_landmark_results)

# Non_landmark_results_temp <- dplyr::select(Non_landmark_results,
#                                         "Organisation_T" =Organisation,
#                                         "Zone_T" = Zone,
#                                         "Contact_T" =Contact,
#                                         "Farmer_T" = Farmer, 
#                                         "Paddock_tested_T" = Paddock_tested  ,
#                                         temp_ID)
#  
#  str(Non_and_Landmark_2020_06_04_paddock_code_only)
#  str(Non_landmark_results_temp)
#  Non_landmark_results <- left_join(Non_landmark_results_temp, Non_and_Landmark_2020_06_04_paddock_code_only)
# 
#  write.csv(Non_landmark_results, paste0(path_finished_wk,
#                         "complied",
#                         "/",
#                         "Non_landmark_results_check",
#                         Sys.Date(),
#                         "_For_JO.csv"))


#all looks good now I can do this for non landmark results
str(Non_and_Landmark_2020_06_04_paddock_code_only)
str(Non_landmark_results)
Non_landmark_results <- left_join(Non_landmark_results, Non_and_Landmark_2020_06_04_paddock_code_only)


str(Non_landmark_results)

Non_landmark_results <- dplyr::select(Non_landmark_results, -X1, -X, -temp_ID )

### add in the soil test results
NP_database_31032020_SA <- read_excel("C:/Users/ouz001/Dropbox/GRDC_Soil_Plant_Testing_Database/NP_database_31032020_SA.xlsx")
#str(NP_database_31032020_SA)
NP_database_31032020_SA<-
  dplyr::select(NP_database_31032020_SA,
                "Paddock_code" =  `Paddock code`, Colwell,
                DGT,
                PBI ,
                `Total N`,
                `Colwell rec rate`,
                `DGT rec rate`    
  )
#remove the row that is missing..
NP_database_31032020_SA <-filter(NP_database_31032020_SA, Paddock_code != "NA")  

## join to non landmark results

str(NP_database_31032020_SA)
str(Non_landmark_results)
Non_landmark_results_soil_and_pair <- left_join(Non_landmark_results, NP_database_31032020_SA, by= c('Paddock code' = "Paddock_code"))

#order the clms

str(Non_landmark_results_soil_and_pair)
Non_landmark_results_soil_and_pair <- Non_landmark_results_soil_and_pair %>%
  dplyr::select('Paddock code', Organisation, 
                Contact,
                Farmer,
                Paddock_tested,
                Zone,
                everything())



############################################################
## Add in info about the fertiliser applied

str(Non_landmark_results_soil_and_pair)
#Non_landmark_results_soil_and_pair <- dplyr::select(Non_landmark_results_soil_and_pair, -Details, -Starter_Feriliser, -Topdress )
Non_landmark_results_soil_and_pair <- dplyr::select(Non_landmark_results_soil_and_pair, -Details, -Starter_Feriliser, -Topdress )
# unique(Non_landmark_results_soil_and_pair$rate_name)
# unique(Non_landmark_rates_applied$rate_name)

########adjust the clm rate_name to only keep Growers_rate

Non_landmark_results_soil_and_pair$rate_name <- recode(Non_landmark_results_soil_and_pair$rate_name, Grower_rate = "Grower_rate", .default = NA_character_)

#bring in the file with the rates 
Non_landmark_rates_applied <- read_excel("complied/Non_landmark_rates_applied.xlsx")
# unique(Non_landmark_results_soil_and_pair$rate_name)
# unique(Non_landmark_rates_applied$rate_name)
str(Non_landmark_results_soil_and_pair)
str(Non_landmark_rates_applied)
#remove some clms to stop the duplication
Non_landmark_rates_applied <- Non_landmark_rates_applied %>% 
  dplyr::select("Paddock code", Details, Starter_Feriliser, Topdress , Rates, rate_name, comments, Trial)


#join by paddock code, rate and rate name (only keeping Grower_rate)

Non_landmark_results_soil_and_pair_rates <- left_join(Non_landmark_results_soil_and_pair, 
                                                      Non_landmark_rates_applied, by= c("Paddock code", "Rates", "rate_name"))
str(Non_landmark_results_soil_and_pair_rates)


write.csv(
  Non_landmark_results_soil_and_pair_rates,
  paste0(
    path_finished_wk,
    "complied",
    "/",
    "Non_landmark_results_soil_and_pair_rates",
    Sys.Date(),
    "_For_TM.csv"
  )
)



