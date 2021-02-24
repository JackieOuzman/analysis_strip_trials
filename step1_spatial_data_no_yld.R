library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)


library(rgdal)
library(sf)

#install.packages("plotKML")
library(raster)
library(knitr)
library(png)

library(readxl)
library(raster)

#update.packages(ask = FALSE, checkBuilt = TRUE)

########################################################################################################
############# Spatial data for rainfall class #########################################################
########################################################################################################

#1.bring in shapefile strip data

all_strips <- st_read("W:/value_soil_testing_prj/Yield_data/2020/All_Strips_2020_wgs84.shp")

# select a few clm
str(all_strips)
all_strips <- all_strips %>%
  dplyr::select(
    Paddock_ID,
    Rate,
    GSP,
    Strip_Rate,
    Start_Fert,
    Top_Dress,
    Strip_Type,
    StripCount,
    GSP_list,
    join_field
  )
str(all_strips)
all_strips <- all_strips %>% 
  dplyr::mutate(ID_Rate_GSP_type = paste0(Paddock_ID, "-", Rate, "-", GSP,"-",Strip_Type ),
         ID_trial_type = paste0(Paddock_ID, "-",Strip_Type))



#2.turn polygons into points - centriod

all_strips_centroid = st_centroid(all_strips)
#str(all_strips_centroid)

###################################################################################################
###################  rainfall data from BOM ########################################################
####################################################################################################

# downloaded gridded rainfall data from BOM

#http://www.bom.gov.au/jsp/ncc/climate_averages/rainfall/index.jsp
#choosing the average rainfall - convert the text file into a grid and extract the gridded values to points
                             
av_rain <- raster::raster("W:/value_soil_testing_prj/Yield_data/2020/processing/rain_grid")
av_rain

##2. extract strips coordinates points from the raster (eg shapefile points and average rainfall grid)
all_strips_centroid$av_rain <- raster::extract(av_rain, all_strips_centroid)



#####################################################################################################
##############  recode GSP_list clm to work out if I have alt GSP strip ############################
####################################################################################################
str(all_strips_centroid)
all_strips_centroid <-all_strips_centroid %>% 
  dplyr::mutate(Alt_GSP = ifelse(stringr::str_detect(GSP_list, "Alt GSP"), "Alt_GPS", "No_Alt_GSP"))



st_write(all_strips_centroid, 
         paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step1_spatial_data_no_yld", "_",Sys.Date(), ".csv"), 
         layer_options = "GEOMETRY=AS_XY")

