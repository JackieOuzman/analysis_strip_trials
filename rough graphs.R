spatial_data_no_yld <- st_read("W:/value_soil_testing_prj/Yield_data/2020/All_Strips_2020_wgs84.shp")
spatial_data_no_yld_df <- data.frame(spatial_data_no_yld)
spatial_data_no_yld_df <- spatial_data_no_yld_df %>% dplyr::select(-geometry)


spatial_data_no_yld_df <- spatial_data_no_yld_df %>% 
  dplyr::mutate(ID_Rate_GSP_type = paste0(Paddock_ID, "-", Rate, "-", GSP,"-",Strip_Type ),
                ID_trial_type = paste0(Paddock_ID, "-",Strip_Type))

spatial_data_no_yld_df <-spatial_data_no_yld_df %>% 
  dplyr::mutate(Alt_GSP = ifelse(stringr::str_detect(GSP_list, "Alt GSP"), "Alt_GPS", "No_Alt_GSP"))

test <- spatial_data_no_yld_df 

test <-test %>% drop_na(Strip_Type)
test <-test %>% drop_na(State)
names(test)
unique(test$State)
test$State <- as.character(test$State)

test <- test %>% 
  mutate(State = case_when(
    State == "Vic" ~ "VIC",
    TRUE ~ State))

   dplyr::distinct(test, Paddock_ID , .keep_all = TRUE) %>%
  # dplyr::select(Paddock_ID, Strip_Type, Alt_GSP) %>%
  ggplot( aes(Strip_Type)) +
  geom_bar() +
  facet_wrap(.~ State)+
  theme_bw() +
  geom_text(aes(y = ((..count..)), label = ((..count..))), stat = "count", vjust = -0.25) +
  labs(
    title = "Number of P and N trials. 2020 to 2021 season",
    y = "Count",
    x = "Type of trial",
    subtitle = paste0("Total number of paddocks = ", count(
      distinct(spatial_data_no_yld_df, ID_trial_type)), ". Note some paddocks are waiting more details from growers"
    )
  )

   
   
   spatial <-  st_as_sf(spatial_data_no_yld, coords = c("X", "Y"), crs = 4326)
   #spatial
   worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                            returnclass = 'sf')
   
   
   ggplot() + geom_sf(data = worldmap) +
     geom_sf(data = spatial, color = "black", fill = NA)+
     coord_sf(xlim = c(120, 160), ylim = c(-44, -25), expand = FALSE) +
     theme_bw()+
     theme(axis.text.x=element_text(angle=90,hjust=1)) +
     facet_wrap(.~ Strip_Type)+
     labs(title = "Location of paddocks",
          caption = "")
   
   
   
   