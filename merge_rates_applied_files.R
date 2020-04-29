###merge the CSV files ###

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



file1 = "Project Information WestBrooke.xlsx"
file2 ="Project Information AC Jacka.xlsx"
file3 ="Project Information Ag Schilling.xlsx"
file4 ="Project Information AP Robertson.xlsx"
file5 ="Project Information Barney O'Callahan .xlsx"
file6 ="Project Information Chris Shark.xlsx"
file7 ="Project Information Clarke Bros.xlsx"
file8 ="Project Information CRG Weeks.xlsx"
file9 ="Project Information Gary Virgin.xlsx"
file10 ="Project Information Heighton Partnership .xlsx"
file11 ="Project Information Jamie Frankel.xlsx"
file12 ="Project Information Jenharwil.xlsx"
file13 ="Project Information K&G Bird.xlsx"
file14 ="Project Information K&T Ryan.xlsx"
file15 ="Project Information Leighview.xlsx"
file16 ="Project Information Limevale.xlsx"
file17 ="Project Information Linc Lehmann.xlsx"
file18 ="Project Information Litster.xlsx"
file19 ="Project Information LS & EE harris.xlsx"
file20 ="Project Information MA & KL Thompson .xlsx"
file21 ="Project Information Melrose Ag.xlsx"
file22 ="Project Information Noel Birkinshaw.xlsx"
file23 ="Project Information P&D Nihil.xlsx"
file24 ="Project Information Paul Jenz.xlsx"
file25 ="Project Information Peninsula Trading .xlsx"
file26 ="Project Information Rob Comer.xlsx"
file27 ="Project Information Simon Ballinger.xlsx"
file28 ="Project Information Simon Martin .xlsx"
file29 ="Project Information Tyke Staude.xlsx"
file30 ="Project Information VH Hocking .xlsx"



name_of_path <-
  file.path(
    "W:",
    "value_soil_testing_prj",
    "data_base",
    "Rates_applied2019",
    "Completed Project Information 2019"
  )



name_of_path <- "W:/value_soil_testing_prj/data_base/Rates_applied2019/Completed Project Information 2019/"
file2
str(df1)

clm_needed_N <- c("...1",
                'Grower Standard Practise',
                'P Strip 1 rate' ,
                'P Strip 2 rate',
                #'P Strip 3 rate', 
                'N Strip 1 rate',
                'N Strip 2 rate',
                'N strip 3 rate',
                'N Topdressed on P strips',
                'source_file')
clm_needed_N_2 <- c("...1",
                  'Grower Standard Practise',
                  'P Strip 1 rate' ,
                  'P Strip 2 rate',
                  #'P Strip 3 rate', 
                  'N Strip 1 rate',
                  'N Strip 2 rate',
                  'N strip 3 rate',
                  #'N Topdressed on P strips',
                  'source_file')

clm_needed_P_2 <- c("...1",
                  'Grower Standard Practise',
                  'P Strip 1 rate' ,
                  'P Strip 2 rate',
                  #'P Strip 3 rate', 
                  'N Strip 1 rate',
                  'N Strip 2 rate',
                  'N strip 3 rate',
                  #'N Topdressed on P strips',
                  'source_file')
clm_needed_P_3 <- c("...1",
                    'Grower Standard Practise',
                    'P Strip 1 rate' ,
                    'P Strip 2 rate',
                    'P Strip 3 rate', 
                    'N Strip 1 rate',
                    'N Strip 2 rate',
                    'N strip 3 rate',
                    #'N Topdressed on P strips',
                    'source_file')
clm_needed_P_3 <- c("...1",
                    'Grower Standard Practise',
                    'P Strip 1 rate' ,
                    'P Strip 2 rate',
                    'P Strip 3 rate', 
                    #'N Strip 1 rate',
                    #'N Strip 2 rate',
                    #'N strip 3 rate',
                    #'N Topdressed on P strips',
                    'source_file')

df1 <-
  read_excel(paste0(name_of_path,  "/", file1), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file1)) %>%
  dplyr::select(
    c(
      "...1" ,
      'Grower Standard Practise',
      'P Strip 1 rate' ,
      'P Strip 2 rate',
      #'P Strip 3 rate',
      'N Strip 1 rate',
      'N Strip 2 rate',
      'N strip 3 rate',
      'N Topdressed on P strips',
      'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided")  

df2 <-
  read_excel(paste0(name_of_path,  "/", file2), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file2)) %>%
  dplyr::select(
    c(
      "...1" ,
      'Grower Standard Practise',
      'P Strip 1 rate' ,
      'P Strip 2 rate',
      #'P Strip 3 rate',
      'N Strip 1 rate',
      'N Strip 2 rate',
      'N strip 3 rate',
      'N Topdressed on P strips',
      'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided") 

    
df3 <-
  read_excel(paste0(name_of_path,  "/", file3), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file3)) %>%
  dplyr::select(
    c(
      "...1" ,
      'Grower Standard Practise',
      'P Strip 1 rate' ,
      'P Strip 2 rate',
      #'P Strip 3 rate',
      'N Strip 1 rate',
      'N Strip 2 rate',
      'N strip 3 rate',
      #'N Topdressed on P strips',
      'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided",
         'N Topdressed on P strips'= "not_provided") 

df4 <-
  read_excel(paste0(name_of_path,  "/", file4), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file4)) %>%
  dplyr::select(
    c(
      "...1" ,
      'Grower Standard Practise',
      'P Strip 1 rate' ,
      'P Strip 2 rate',
      #'P Strip 3 rate',
      'N Strip 1 rate',
      'N Strip 2 rate',
      'N strip 3 rate',
      'N Topdressed on P strips',
      'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided")#,
         #'N Topdressed on P strips'= "not_provided") 

df5 <-  read_excel(paste0(name_of_path,  "/", file5), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file5)) %>% 
  dplyr::select(c("...2", 'Grower Standard Practise',
                  'P Strip 1 rate' ,
                  'P Strip 2 rate',
                  'P Strip 3 rate',
                  'N Strip 1 rate',
                  'N Strip 2 rate',
                  'N strip 3 rate',
                  'N Topdressed on P strips',
                  'source_file')) %>% 
  dplyr::rename("ID" = "...2")  


df6 <-
  read_excel(paste0(name_of_path,  "/", file6), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file6)) %>%
  dplyr::select(
    c(
      "...1" ,
      'Grower Standard Practise',
      'P Strip 1 rate' ,
      'P Strip 2 rate',
      'P Strip 3 rate',
      #'N Strip 1 rate',
      #'N Strip 2 rate',
      #'N strip 3 rate',
      #'N Topdressed on P strips',
      'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('N Strip 1 rate' = "not_provided",
         'N Strip 2 rate' = "not_provided",
         'N strip 3 rate' = "not_provided",
         'N Topdressed on P strips'= "not_provided") 


df7 <-
  read_excel(paste0(name_of_path,  "/", file7), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file7)) %>%
  dplyr::select(
    c(
      "...1" ,
      'Grower Standard Practise',
      'P Strip 1 rate' ,
      'P Strip 2 rate',
      #'P Strip 3 rate',
      'N Strip 1 rate',
      'N Strip 2 rate',
      'N strip 3 rate',
      'N Topdressed on P strips',
      'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided") 

df8 <-
  read_excel(paste0(name_of_path,  "/", file8), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file8)) %>%
  dplyr::select(
    c(
      "...1" ,
      'Grower Standard Practise',
      'P Strip 1 rate' ,
      'P Strip 2 rate',
      #'P Strip 3 rate',
      'N Strip 1 rate',
      'N Strip 2 rate',
      'N strip 3 rate',
      'N Topdressed on P strips',
      'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided") 
   

df9 <-
  read_excel(paste0(name_of_path,  "/", file9), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file9)) %>%
  dplyr::select(
    c(      "...1" ,
      'Grower Standard Practise',
      'P Strip 1 rate' ,
      'P Strip 2 rate',
      'P Strip 3 rate',
      #'N Strip 1 rate',
      #'N Strip 2 rate',
      #'N strip 3 rate',
      'N Topdressed on P strips',
      'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('N Strip 1 rate' = "not_provided",
         'N Strip 2 rate' = "not_provided",
         'N strip 3 rate' = "not_provided"
         )

df10 <-
  read_excel(paste0(name_of_path,  "/", file10), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file10)) %>%
  dplyr::select(
    c(      "...1" ,
            'Grower Standard Practise',
            'P Strip 1 rate' ,
            'P Strip 2 rate',
            #'P Strip 3 rate',
            #'N Strip 1 rate',
            'N Strip 2 rate',
            'N strip 3 rate',
            'N Topdressed on P strips',
            'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided",
         'N Strip 1 rate' = "not_provided"
         
  )


df11 <-
  read_excel(paste0(name_of_path,  "/", file11), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file11)) %>%
  dplyr::select(
    c(      "...1" ,
            'Grower Standard Practise',
            'P Strip 1 rate' ,
            'P Strip 2 rate',
            #'P Strip 3 rate',
            'N Strip 1 rate',
            'N Strip 2 rate',
            'N strip 3 rate',
            'N Topdressed on P strips',
            'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided")

df12 <-
  read_excel(paste0(name_of_path,  "/", file12), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file12)) %>%
  dplyr::select(
    c(      "...1" ,
            'Grower Standard Practise',
            'P Strip 1 rate' ,
            'P Strip 2 rate',
            'P Strip 3 rate',
            'N Strip 1 rate',
            'N Strip 2 rate',
            'N strip 3 rate',
            'N Topdressed on P strips',
            'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  

df13 <-
  read_excel(paste0(name_of_path,  "/", file13), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file13)) %>%
  dplyr::select(
    c(      "...1" ,
            'Grower Standard Practise',
            'P Strip 1 rate' ,
            'P Strip 2 rate',
            #'P Strip 3 rate',
            'N Strip 1 rate',
            'N Strip 2 rate',
            'N strip 3 rate',
            'N Topdressed on P strips',
            'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided")

df14 <-
  read_excel(paste0(name_of_path,  "/", file14), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file14)) %>%
  dplyr::select(
    c(      "...1" ,
            'Grower Standard Practise',
            'P Strip 1 rate' ,
            'P Strip 2 rate',
            'P Strip 3 rate',
            'N Strip 1 rate',
            'N Strip 2 rate',
            'N strip 3 rate',
            'N Topdressed on P strips',
            'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1") 

df15 <-
  read_excel(paste0(name_of_path,  "/", file15), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file15)) %>%
  dplyr::select(
    c(      "...1" ,
            'Grower Standard Practise',
            'P Strip 1 rate' ,
            'P Strip 2 rate',
            'P Strip 3 rate',
            'N Strip 1 rate',
            'N Strip 2 rate',
            'N strip 3 rate',
            'N Topdressed on P strips',
            'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")


df16 <-
  read_excel(paste0(name_of_path,  "/", file16), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file16)) %>%
  dplyr::select(
    c(      "...1" ,
            'Grower Standard Practise',
            'P Strip 1 rate' ,
            'P Strip 2 rate',
            #'P Strip 3 rate',
            'N Strip 1 rate',
            'N Strip 2 rate',
            'N strip 3 rate',
            #'N Topdressed on P strips',
            'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided",
         'N Topdressed on P strips'= "not_provided") 

df17 <-
  read_excel(paste0(name_of_path,  "/", file17), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file17)) %>%
  dplyr::select(
    c(      "...1" ,
            'Grower Standard Practise',
            'P Strip 1 rate' ,
            'P Strip 2 rate',
            #'P Strip 3 rate',
            'N Strip 1 rate',
            'N Strip 2 rate',
            'N strip 3 rate',
            #'N Topdressed on P strips',
            'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided",
         'N Topdressed on P strips'= "not_provided") 

df18 <-
  read_excel(paste0(name_of_path,  "/", file18), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file18)) %>%
  dplyr::select(
    c(    "...1" ,
            'Grower Standard Practise',
            'P Strip 1 rate' ,
            'P Strip 2 rate',
            #'P Strip 3 rate',
            'N Strip 1 rate',
            'N Strip 2 rate',
            'N strip 3 rate',
            'N Topdressed on P strips',
            'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided") 

df19 <-
  read_excel(paste0(name_of_path,  "/", file19), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file19)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          #'P Strip 3 rate',
          'N Strip 1 rate',
          'N Strip 2 rate',
          'N strip 3 rate',
          'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided") 

df20 <-
  read_excel(paste0(name_of_path,  "/", file20), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file20)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          #'P Strip 3 rate',
          'N Strip 1 rate',
          'N Strip 2 rate',
          'N strip 3 rate',
          #'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided", 
         'N Topdressed on P strips'= "not_provided") 


df21 <-
  read_excel(paste0(name_of_path,  "/", file21), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file21)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          #'P Strip 3 rate',
          'N Strip 1 rate',
          'N Strip 2 rate',
          'N strip 3 rate',
          'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate' = "not_provided") 

df22 <-
  read_excel(paste0(name_of_path,  "/", file22), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file22)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          'P Strip 3 rate',
          'N Strip 1 rate',
          'N Strip 2 rate',
          'N strip 3 rate',
          'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  

df23 <-
  read_excel(paste0(name_of_path,  "/", file23), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file23)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          #'P Strip 3 rate',
          'N Strip 1 rate',
          'N Strip 2 rate',
          'N strip 3 rate',
          #'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('N Topdressed on P strips' = "not_provided",
         'P Strip 3 rate'= "not_provided")

df24 <-
  read_excel(paste0(name_of_path,  "/", file24), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file24)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          #'P Strip 3 rate',
          'N Strip 1 rate',
          'N Strip 2 rate',
          'N strip 3 rate',
          #'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('N Topdressed on P strips' = "not_provided",
         'P Strip 3 rate'= "not_provided")

df25 <-
  read_excel(paste0(name_of_path,  "/", file25), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file25)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          #'P Strip 3 rate',
          'N Strip 1 rate',
          'N Strip 2 rate',
          'N strip 3 rate',
          #'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('N Topdressed on P strips' = "not_provided",
         'P Strip 3 rate'= "not_provided")

df26 <-
  read_excel(paste0(name_of_path,  "/", file26), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file26)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          #'P Strip 3 rate',
          'N Strip 1 rate',
          'N Strip 2 rate',
          'N strip 3 rate',
          'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate'= "not_provided")

df27 <-
  read_excel(paste0(name_of_path,  "/", file27), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file27)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          #'P Strip 3 rate',
          'N Strip 1 rate',
          'N Strip 2 rate',
          'N strip 3 rate',
          'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate'= "not_provided")

df28 <-
  read_excel(paste0(name_of_path,  "/", file28), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file28)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          #'P Strip 3 rate',
          'N Strip 1 rate',
          'N Strip 2 rate',
          'N strip 3 rate',
          #'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('N Topdressed on P strips' = "not_provided",
         'P Strip 3 rate'= "not_provided")

df29 <-
  read_excel(paste0(name_of_path,  "/", file29), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file29)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          'P Strip 3 rate',
          #'N Strip 1 rate',
          #'N Strip 2 rate',
          #'N strip 3 rate',
          'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('N Strip 1 rate'= "not_provided",
         'N Strip 2 rate'= "not_provided",
         'N strip 3 rate'= "not_provided")

df30 <-
  read_excel(paste0(name_of_path,  "/", file30), sheet = "P and N strips") %>%
  mutate(source_file = paste0(file30)) %>%
  dplyr::select(
    c(    "...1" ,
          'Grower Standard Practise',
          'P Strip 1 rate' ,
          'P Strip 2 rate',
          #'P Strip 3 rate',
          'N Strip 1 rate',
          'N Strip 2 rate',
          'N strip 3 rate',
          'N Topdressed on P strips',
          'source_file'
    )
  ) %>%
  dplyr::rename("ID" = "...1")  %>%
  mutate('P Strip 3 rate'= "not_provided")

merged <- rbind(df1, df2, df3, df4, df5, df7, df8, df10, df6, df9, 
                df11, df12, df13, df14, df15, df16, df17, df18, df19, df20,
                df21, df22, df23, df24, df25, df26, df27, df28, df29, df30)
merged <- merged %>% 
  dplyr::select("source_file",
"ID",
"Grower Standard Practise" ,
"P Strip 1 rate" ,
"P Strip 2 rate" ,
"P Strip 3 rate",
"N Strip 1 rate" ,
"N Strip 2 rate" ,
"N strip 3 rate",
"N Topdressed on P strips") %>% 
dplyr::mutate(strip_that_represents_GSP = "Pls fill in")
 
# print(merged)
 name_of_path
# paste0(name_of_path, "/raw_data/",Paddock_tested_db,"zone_rates.csv")
 write.csv(merged, paste0(name_of_path,"rates_applied2019.csv"))
                                     
 
