#Collating zone analsyis results

#1. install the lib that I will be using for this work

libs <- c("dplyr", "tidyr", 
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

path_finished_wk <- "W:/value_soil_testing_prj/Yield_data/finished/"
setwd(path_finished_wk)

file_names1 <- list.files(path = path_finished_wk,
                          pattern = ".csv") 
print(file_names1)

################################
#now which ones do I want to merge?


org_contact_farmer <- "Landmark_Kris_Dixon_6_Heightons_1"

####Some files are missing paddock info so I need to add these now - oops!
file_names2 <- list.files(path = path_finished_wk,
                          pattern=glob2rx(paste0("*",org_contact_farmer,"*", ".csv")))

print(file_names2)

modify <- read.csv("Landmark_Steve_Richmond_5_Clarke_Bros_1_30_below_house__zones.csv")
modify <- modify %>% 
  mutate(Paddock_tested = "30_below_house")
write_csv(modify,"Landmark_Steve_Richmond_5_Clarke_Bros_1_30_below_house__zones.csv" )


###This is for when we have paddock and zone in the data analysis file.
#########################################################################################################


file_names2 <-
  c(
   "Landmark_Steve_Richmond_5_AC_Jacka_2_Heads__zones.csv")#,
  #   "Landmark_Kris_Dixon_6_Heightons_1_McKenzie__zones.csv",
  #   "Landmark_Kris_Dixon_6_Heightons_1_Bellas__zones.csv")#,
  #   "Landmark_Tom_Cooper_9_A_P_Robertson_1_McPhees__zones.csv",
  #   "Landmark_Tom_Cooper_9_A_P_Robertson_1_Bird__zones.csv",
  #   "Landmark_Tom_Cooper_9_A_P_Robertson_1_Affecks__zones.csv"),
  #   "Landmark_Matt_Nihill_7_GRG_Weeks_1_Reids_mount__zones.csv",
  #   "Landmark_Matt_Nihill_7_GRG_Weeks_1_Reids_mid__zones.csv")#,
  #   "Landmark_Steve_Richmond_5_Clarke_Bros_1_30_below_house__zones.csv")
  # )


# file_names2 <- list.files(path = path_finished_wk,
#                           pattern=glob2rx(paste0("*",org_contact_farmer,"*", ".csv")))
print(file_names2)
# read in each .csv file in file_list and rbind them into a data frame called data 
data <- 
  do.call("rbind", 
          lapply(file_names2, 
                 function(x) 
                   read.csv(paste(path_finished_wk, x, sep=''), 
                            stringsAsFactors = FALSE)))

###########################################################################################################
Landmark_Steve_Richmond_5_AC_Jacka_2_Heads <- data
Landmark_Kris_Dixon_6_Heightons_1 <- data
Landmark_Tom_Cooper_9_A_P_Robertson_1 <- data
Landmark_Matt_Nihill_7_GRG_Weeks_1 <- data
Landmark_Steve_Richmond_5_Clarke_Bros_1 <- data

### Bring in all that is run into one df

dim(Landmark_Steve_Richmond_5_AC_Jacka_2_Heads)#16
dim(Landmark_Kris_Dixon_6_Heightons_1)#16
dim(Landmark_Tom_Cooper_9_A_P_Robertson_1)#16
dim(Landmark_Matt_Nihill_7_GRG_Weeks_1)#16
dim(Landmark_Steve_Richmond_5_Clarke_Bros_1)#16


results_15_05_2020 <- rbind(Landmark_Steve_Richmond_5_AC_Jacka_2_Heads, 
                            Landmark_Kris_Dixon_6_Heightons_1,
                            Landmark_Tom_Cooper_9_A_P_Robertson_1,
                            Landmark_Matt_Nihill_7_GRG_Weeks_1,
                            Landmark_Steve_Richmond_5_Clarke_Bros_1
                            )


write.csv(results_15_05_2020, "Landmark_15_05_2020.csv")

