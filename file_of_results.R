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

org_contact_farmer <- "MSF_Michael_Moodie_Todd_McDonald_Pole_Norms"

####Some files are missing paddock info so I need to add these now - oops!
file_names2 <- list.files(path = path_finished_wk,
                          pattern=glob2rx(paste0("*",org_contact_farmer,"*", ".csv")))

print(file_names2)
modify <- read.csv("MSF_Michael_Moodie_Todd_McDonald_Pole_Norms__zones1a Rate 0 to 74 and zone 2 to 4.csv")
modify <- modify %>% 
  mutate(Paddock_tested = "Norms")
write_csv(modify,"MSF_Michael_Moodie_Todd_McDonald_Pole_Norms__zones1a Rate 0 to 74 and zone 2 to 4.csv" )


###This is for when we have paddock and zone in the data analysis file.
#########################################################################################################

file_names2 <- list.files(path = path_finished_wk,
                          pattern=glob2rx(paste0("*",org_contact_farmer,"*", ".csv")))
print(file_names2)
# read in each .csv file in file_list and rbind them into a data frame called data 
data <- 
  do.call("rbind", 
          lapply(file_names2, 
                 function(x) 
                   read.csv(paste(path_finished_wk, x, sep=''), 
                            stringsAsFactors = FALSE)))

###########################################################################################################
Landmark_Claire_Gutsche_6_Lister_Trading_1 <- data
Landmark_Hamish_Verco_7_Ballinger_1 <- data
Landmark_Steve_Richmond_5 <- data
MSF_Michael_Moodie_Todd_McDonald_Anderson <- data
MSF_Michael_Moodie_Todd_McDonald_Hunt <- data
MSF_Michael_Moodie_Todd_McDonald_Pole_Norms <- data

## this one will need some work
MSF_Michael_Moodie_Todd_McDonald_Pole_Georges <- data
str(MSF_Michael_Moodie_Todd_McDonald_Pole_Georges)
View(MSF_Michael_Moodie_Todd_McDonald_Pole_Georges)
#make an ID clm
MSF_Michael_Moodie_Todd_McDonald_Pole_Georges <- MSF_Michael_Moodie_Todd_McDonald_Pole_Georges %>% 
  mutate(ID = paste0(Paddock_tested, Zone, Rates))
#only keep unique id clms
MSF_Michael_Moodie_Todd_McDonald_Pole_Georges <- MSF_Michael_Moodie_Todd_McDonald_Pole_Georges %>% 
  distinct(ID, .keep_all = TRUE)
View(MSF_Michael_Moodie_Todd_McDonald_Pole_Georges)
MSF_Michael_Moodie_Todd_McDonald_Pole_Georges <-  dplyr::select(MSF_Michael_Moodie_Todd_McDonald_Pole_Georges, -ID)

## this one will need some work
MSF_Michael_Moodie_Todd_McDonald_Pole_Norms <- data
str(MSF_Michael_Moodie_Todd_McDonald_Pole_Norms)
View(MSF_Michael_Moodie_Todd_McDonald_Pole_Norms)
#make an ID clm
MSF_Michael_Moodie_Todd_McDonald_Pole_Norms <- MSF_Michael_Moodie_Todd_McDonald_Pole_Norms %>% 
  mutate(ID = paste0(Paddock_tested, Zone, Rates))
#only keep unique id clms
MSF_Michael_Moodie_Todd_McDonald_Pole_Norms <- MSF_Michael_Moodie_Todd_McDonald_Pole_Norms %>% 
  distinct(ID, .keep_all = TRUE)
View(MSF_Michael_Moodie_Todd_McDonald_Pole_Norms)
MSF_Michael_Moodie_Todd_McDonald_Pole_Norms <-  dplyr::select(MSF_Michael_Moodie_Todd_McDonald_Pole_Norms, -ID)


### Bring in all that is run into one df

results_16_04_2020 <- rbind(Landmark_Claire_Gutsche_6_Lister_Trading_1, 
                            Landmark_Hamish_Verco_7_Ballinger_1,
                            Landmark_Steve_Richmond_5,
                            MSF_Michael_Moodie_Todd_McDonald_Anderson,
                            MSF_Michael_Moodie_Todd_McDonald_Hunt,
                            MSF_Michael_Moodie_Todd_McDonald_Pole_Norms)


write.csv(results_16_04_2020, "Sean_results_16_04_2020.csv")

