#Collating zone analsyis results

#1. install the lib that I will be using for this work
library(stringr)
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

my_list_landmark_files <- list.files(path = path_finished_wk,
                                     pattern = "^[Landmark]")
#pattern = ".csv") 
print(my_list_landmark_files)
#remove the summary landmark file Landmark_15_05_2020.csv - which appear first
landmark_files <- my_list_landmark_files[2:40]
landmark_files

data <- 
  do.call("rbind", 
          lapply(landmark_files, #this is a list of files
                 function(x) 
                   read.csv(paste(path_finished_wk, x, sep=''), 
                            stringsAsFactors = FALSE)))




# ####Some files are missing paddock info so I need to add these now - oops!
# modify <- read.csv("Landmark_Steve_Richmond_5_AC_Jacka_2_Home_6__zones.csv")
# modify <- modify %>% 
#   mutate(Paddock_tested = "Home_6")
# write_csv(modify,"Landmark_Steve_Richmond_5_AC_Jacka_2_Home_6__zones.csv")
# 
# ####Some files are missing paddock info and other info so I need to add these now - oops!
# modify <- read.csv("Landmark_James_Falvey_2_Tim_McClelland_4_Backsheehans__zones.csv")
# modify <- modify %>% 
#   mutate(Details = "not supplied yet",
#          Starter_Feriliser = "not supplied yet",
#          Topdress = "not supplied yet",
#          Zone = "no zones")
# write_csv(modify,"Landmark_James_Falvey_2_Tim_McClelland_4_Backsheehans__zones.csv")


##these are the files that have clm added to them
# dim(read.csv( "Landmark_Claire_Gutsche_6_Lister_Trading_1_Bevs__zones.csv" ))  #16     
# dim(read.csv( "Landmark_Claire_Gutsche_6_Lister_Trading_1_Willings_Willows__zones.csv"))#16
# dim(read.csv( "Landmark_Hamish_Verco_7_Ballinger_1_Reschkes_North__zones.csv"))  #16       
# dim(read.csv( "Landmark_Heath_Griffith_5_Campbell_2_Brennans__zones.csv"))   #16       
# dim(read.csv( "Landmark_James_Falvey_2_Tim_McClelland_4_Clover__zones.csv"))  #16          
# dim(read.csv("Landmark_James_Falvey_2_Tim_McClelland_4_Hennessy__zones.csv"  )) #16  
# dim(read.csv( "Landmark_James_Falvey_2_Tim_McClelland_4_Top__zones.csv"  ))  #16 
# dim(read.csv( "Landmark_James_Falvey_2_Tim_McClelland_4_Mervyns__strip.csv" )) #16 
# dim(read.csv( "Landmark_James_Falvey_2_Tim_McClelland_4_Backsheehans__zones.csv" )) #16
# 
# #these were good from the start
# dim(read.csv("Landmark_Alister_Tippert_1_Chris_Shark_4_Balls_Centre_2__zones.csv")) #16
# dim(read.csv("Landmark_Alister_Tippert_1_Chris_Shark_4_Balls_Centre_2__zones.csv" ))   
# dim(read.csv("Landmark_Alister_Tippert_1_Chris_Shark_4_Balls_Silo_1__zones.csv" ))     
# dim(read.csv("Landmark_Alister_Tippert_1_Leighview_2_Jash_5__zones.csv" ))             
# dim(read.csv( "Landmark_Alister_Tippert_1_PaulJenz_3_Harrys_Long_2__zones.csv"))        
# dim(read.csv( "Landmark_Andrew_McMahen_8_Barney_OCallaghan_2_Hickmont__zones.csv" ))    
# dim(read.csv( "Landmark_Andrew_McMahen_8_Barney_OCallaghan_2_Jardines__zones.csv" ))    
# dim(read.csv( "Landmark_Andrew_McMahen_8_Barney_OCallaghan_2_NOC_2__zones.csv" ))       
# dim(read.csv( "Landmark_Andrew_McMahen_8_Birkinshaw_1_Rail__zones.csv"   ))             
# dim(read.csv( "Landmark_Andrew_McMahen_8_Birkinshaw_1_Round_Home__zones.csv"))          
# dim(read.csv( "Landmark_Chris_Dunn_7_Jenharwil_2_Brooklands_2__zones.csv"  ))           
# dim(read.csv( "Landmark_Chris_Dunn_7_Jenharwil_2_James_2__zones.csv" ))                 
# dim(read.csv( "Landmark_Chris_Dunn_7_Jenharwil_2_Lindsey_Cali__zones.csv"  )) 
# dim(read.csv( "Landmark_Claire_Gutsche_6_Lister_Trading_1_East_Laundys__zones.csv" ))#16 
# dim(read.csv( "Landmark_James_Falvey_2_David_Ferrier_School__zones.csv"))               
# dim(read.csv( "Landmark_James_Falvey_2_Frankel_2_Front_1__zones.csv"  ))                
# dim(read.csv( "Landmark_James_Falvey_2_Frankel_2_Front_3__zones.csv"  ))                
# dim(read.csv( "Landmark_James_Falvey_2_Frankel_2_Stewarts_3__zones.csv"  ))             
# dim(read.csv( "Landmark_James_Falvey_2_Lehmann_3_Back__zones.csv"   ))                  
# dim(read.csv( "Landmark_James_Falvey_2_Lehmann_3_Swamp__zones.csv" ))                   
# dim(read.csv( "Landmark_James_Falvey_2_Tim_McClelland_4_Backsheehans__zones.csv"  ))  
# dim(read.csv( "Landmark_Kris_Dixon_6_Heightons_1_Bellas__zones.csv"  ))                 
# dim(read.csv( "Landmark_Kris_Dixon_6_Heightons_1_McKenzie__zones.csv"  ))               
# dim(read.csv( "Landmark_Matt_Nihill_7_GRG_Weeks_1_Reids_mid__zones.csv"   ))            
# dim(read.csv( "Landmark_Matt_Nihill_7_GRG_Weeks_1_Reids_mount__zones.csv"  ))           
# dim(read.csv( "Landmark_Steve_Richmond_5_AC_Jacka_2_Heads__zones.csv"   ))              
# dim(read.csv( "Landmark_Steve_Richmond_5_AC_Jacka_2_Home_6__zones.csv"  ))              
# dim(read.csv( "Landmark_Steve_Richmond_5_Clarke_Bros_1_07_Back_Grussing__zones.csv"  )) #16
# dim(read.csv( "Landmark_Steve_Richmond_5_Clarke_Bros_1_30_below_house__zones.csv"  ))   
# dim(read.csv( "Landmark_Tom_Cooper_9_A_P_Robertson_1_Affecks__zones.csv" ))             
# dim(read.csv( "Landmark_Tom_Cooper_9_A_P_Robertson_1_Bird__zones.csv"))                 
# dim(read.csv( "Landmark_Tom_Cooper_9_A_P_Robertson_1_McPhees__zones.csv"))   

          


write.csv(data, paste0(path_finished_wk,
                                      "complied",
                                      "/",
                                      "Landmark_",
                                      Sys.Date(),
                                      "_For_TM.csv"))
    
##################################################################################################################
### add in the paddock id and results from the database

### access file that has paddock code and chcek that these are correct.
Landmark_2020_06_04_For_TM_paddock_code_only <- read_csv("complied/Landmark_2020-06-04_For_TM_paddock_code_only.csv")
Landmark_2020_06_04_For_TM_paddock_code_only <- mutate(Landmark_2020_06_04_For_TM_paddock_code_only,
                           temp_ID = paste0(str_sub(Landmark_2020_06_04_For_TM_paddock_code_only$Contact,1,),
                                            "_",
                                            str_sub(Landmark_2020_06_04_For_TM_paddock_code_only$Farmer,1,),
                                            "_",
                                            str_sub(Landmark_2020_06_04_For_TM_paddock_code_only$Paddock_tested,1,),
                                            "_",
                                            str_sub(Landmark_2020_06_04_For_TM_paddock_code_only$Zone,1,)
                           ))

#Append to results and check
landmark_results <- read_csv("W:/value_soil_testing_prj/Yield_data/finished/complied/Landmark_2020-06-04_For_TM.csv")
landmark_results <- mutate(landmark_results,
                           temp_ID = paste0(str_sub(landmark_results$Contact,1,),
                                            "_",
                                            str_sub(landmark_results$Farmer,1,),
                                            "_",
                                            str_sub(landmark_results$Paddock_tested,1,),
                                            "_",
                                            str_sub(landmark_results$Zone,1,)
                           ))


str(Landmark_2020_06_04_For_TM_paddock_code_only)

str(landmark_results)

# landmark_results_temp <- dplyr::select(landmark_results,
#                                        "Organisation_T" =Organisation,
#                                        "Zone_T" = Zone,
#                                        "Contact_T" =Contact,
#                                        "Farmer_T" = Farmer, 
#                                        "Paddock_tested_T" = Paddock_tested  ,
#                                        temp_ID)
# 
# str(Landmark_2020_06_04_For_TM_paddock_code_only)
# str(landmark_results_temp)
# landmark_results <- left_join(landmark_results_temp, Landmark_2020_06_04_For_TM_paddock_code_only)

# write.csv(landmark_results, paste0(path_finished_wk,
#                        "complied",
#                        "/",
#                        "landmark_results_check",
#                        Sys.Date(),
#                        "_For_JO.csv"))

#all looks good now I can do this for landmark results
str(Landmark_2020_06_04_For_TM_paddock_code_only)
str(landmark_results)
landmark_results <- left_join(landmark_results, Landmark_2020_06_04_For_TM_paddock_code_only)


str(landmark_results)

landmark_results <- dplyr::select(landmark_results, -X1, -X, -temp_ID )

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

## join to landmark results

str(NP_database_31032020_SA)
str(landmark_results)
landmark_results_soil_and_pair <- left_join(landmark_results, NP_database_31032020_SA, by= c('Paddock code' = "Paddock_code"))

#order the clms

str(landmark_results_soil_and_pair)
landmark_results_soil_and_pair <- landmark_results_soil_and_pair %>%
  dplyr::select('Paddock code', Organisation, 
         Contact,
         Farmer,
         Paddock_tested,
         Zone,
         everything())


### Tim paddocks are missing data on what was applied this will need to be checked

Rates_applied_Tim <- read_excel("W:/value_soil_testing_prj/data_base/Rates_applied2019/Completed Project Information 2019 DB_rat.xlsx", 
                            sheet ="Added_by_Jaxs" )



str(Rates_applied_Tim)



write.csv(landmark_results_soil_and_pair, paste0(path_finished_wk,
                       "complied",
                       "/",
                       "Landmark_with_soil",
                       Sys.Date(),
                       "_For_TM.csv"))













