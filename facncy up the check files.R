paddock_done2020_t<- read_excel("C:/Users/ouz001/CSIRO/Ratcliff, Christina (A&F, Waite Campus) - Soil_Testing_Project/paddock_done2020.xlsx", 
                              sheet = "T-test_done")

paddock_done2020_t <- paddock_done2020_t %>% 
  dplyr::mutate(paddock5_digits = substr(`Paddock code`, start = 1, stop = 5))

#select orgaiastion


paddock_done2020 <- 
  filter(paddock_done2020_t, Organisation %in% "Agrivision") %>% 
    distinct(paddock5_digits, .keep_all= TRUE) %>% 
    dplyr::select(paddock = paddock5_digits,
                  Organisation,
                  Contact,
                  Farmer,
                  `Paddock tested`,
                  'issues/ comments')  



paddock_done2020

png_folder <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/png"

#Growers_folder <- "W:/value_soil_testing_prj/Yield_data/2020/Agrivision/Craig_Muir/"

#Growers_folder <- "W:/value_soil_testing_prj/Yield_data/2020/Agrivision/Craig_Muir/"
Growers_folder <- paste0("W:/value_soil_testing_prj/Yield_data/2020/", 
unique(paddock_done2020$Organisation), "/",unique(paddock_done2020$Contact), "/")


Growers_folder <- gsub(" ", "_", Growers_folder)
Growers_folder <- gsub("-", "_", Growers_folder)
Growers_folder


#each folder for the sites (4 listed here)
site1 <- paste0((unique(paddock_done2020$Farmer)[1]),"/",
                (unique(paddock_done2020$`Paddock tested`)[1]),
                "/output/")
                
site2 <- paste0((unique(paddock_done2020$Farmer)[2]),"/",
                (unique(paddock_done2020$`Paddock tested`)[2]),
                "/output/")

site3 <- paste0((unique(paddock_done2020$Farmer)[3]),"/",
                (unique(paddock_done2020$`Paddock tested`)[3]),
                "/output/")

site4 <- paste0((unique(paddock_done2020$Farmer)[4]),"/",
                (unique(paddock_done2020$`Paddock tested`)[4]),
                "/output/")

site1 <- gsub(" ", "_", site1)
site1 <- gsub("-", "_", site1)
site2 <- gsub(" ", "_", site2)
site2 <- gsub("-", "_", site2)
site3 <- gsub(" ", "_", site3)
site3 <- gsub("-", "_", site3)
site4 <- gsub(" ", "_", site4)
site4 <- gsub("-", "_", site4)

# site1
# site1_old <- "Rick_Plant/Tynans_Flat_1/output/"
site1
site2
"W:\value_soil_testing_prj\Yield_data\2020\Agrivision\Craig_Muir\Andrew_Parsons\Jampot_East\output"
#Lets look for a yld trace maps
list.files (path = paste0(Growers_folder, site1))

#list files that conatin the word "yld_trace" followed by anything and ending with the word ".png":
yld_trace_site1 <- list.files(path = paste0(Growers_folder, site1),pattern = "trace(.*).png$", ignore.case = TRUE)
yld_trace_site2 <- list.files(path = paste0(Growers_folder, site2),pattern = "trace(.*).png$", ignore.case = TRUE)
yld_trace_site3 <- list.files(path = paste0(Growers_folder, site3),pattern = "trace(.*).png$", ignore.case = TRUE)
yld_trace_site4 <- list.files(path = paste0(Growers_folder, site4),pattern = "trace(.*).png$", ignore.case = TRUE)

############## up to here
#list files that conatin the word "harvest" followed by anything and ending with the word ".png":
harvest_site1 <- list.files(path = paste0(Growers_folder, site1), pattern ="(?!.*pre)harvest")

Do a better job at defining pattern#grep "word1" | grep -v "word2"


# yld_trace_site2 <- list.files(path = paste0(Growers_folder, site2),pattern = "trace(.*).png$")
# yld_trace_site3 <- list.files(path = paste0(Growers_folder, site3),pattern = "trace(.*).png$")
# yld_trace_site4 <- list.files(path = paste0(Growers_folder, site4),pattern = "trace(.*).png$")


yld_trace_site1 <- "TyanansFlat1_yld_trace.png"
harvest_map_site1 <- "TyanansFlat1_harvest_map.png"
Paddock_selected_site1 <- "33111"
site_name_1 <- paste0("^",Paddock_selected_site1)


# paste0(unique(paddock_done2020$Farmer)),
# paste0(unique(paddock_done2020$`Paddock tested`)))





print(file.path("foo", "bar", "..", "name")) # "foo/bar/../name"
