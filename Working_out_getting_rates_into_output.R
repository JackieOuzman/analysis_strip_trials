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


Rates_applied <- read_excel("W:/value_soil_testing_prj/data_base/Rates_applied2019/Completed Project Information 2019 DB_rat.xlsx")
str(Rates_applied)
unique(Rates_applied$Location)
Landmark\Steve_Richmond_5\AC_Jacka_2\Heads #in file
Landmark_Steve_Richmond_5_AC_Jacka_2_Heads #site details
site_details

#just pull out the site I am working on....need to change the \\ to _

library("stringr")
Rates_applied$Location <- str_replace_all(Rates_applied$Location, "\\\\", "_") #note the extra \\ to access the \\ because regular expresion use \\


rates_applied_by_site <- filter(Rates_applied, Location == site_details)

str(rates_applied_by_site)
#This says its 2 P so I have a 

Grower_rate = 50
rate1 = 0
rate2 = 100

Grower_rate_applied =  unique(rates_applied_by_site$strip_that_represents_GSP)
rate1_applied =        unique(rates_applied_by_site$`P Strip 1 rate`)
rate2_applied =        unique(rates_applied_by_site$`P Strip 2 rate`)
rate3_applied =        unique(rates_applied_by_site$`P Strip 3 rate`)
Starter_Feriliser = unique(rates_applied_by_site$`Starter Feriliser`)
Topdress = unique(rates_applied_by_site$`N Topdressed on P strips or N Topdressed on N strips`)


#### 2 rate
list_rates <- data.frame( rate_name = c("Grower_rate" , "rate1",  "rate2"), 
                          Fert_Rates = c(Grower_rate,rate1, rate2 ) )
print(list_rates)
long_name <-  data.frame( rate_name = c("Grower_rate" , "rate1",  "rate2"), 
                          details = c(Grower_rate_applied,
                                    rate1_applied, 
                                    rate3_applied ), 
                          Starter_Feriliser = Starter_Feriliser,
                          Topdress = Topdress)
 

Rates_labels <- left_join(list_rates,long_name )
 

