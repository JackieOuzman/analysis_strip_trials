## Create a dummy file for when we dont have alt GSP strip



#remove all files expect...
rm(list = ls()[!ls() %in% c("strips", 
                            "paddock_ID_1",
                            "paddock_ID_2",
                            "Zone_labels",
                            "input_file",
                            "assigned_names2",
                            "all_results_1"
                            #"function_grand_mean_std_error"
)])

str(all_results_1)
str(Zone_labels)
str(strips)


dummy_alt_file_1 <- all_results_1 %>% 
  dplyr::select(Zone_ID, zone,Zone,input_file)
dummy_alt_file_2 <- strips %>% 
  dplyr::select(Zone_ID, Strip_Type, paddock_ID = Paddock_ID) %>% 
  dplyr::distinct(Zone_ID, .keep_all = TRUE)
dummy_alt_file <- left_join(dummy_alt_file_1, dummy_alt_file_2)


#now all the empty clm
dummy_alt_file <- dummy_alt_file %>%
  mutate(
    yld_response	= "NA",
    `Alt GSP` = "NA",
    GSP	= "NA",
    GSP_vs_Alt_GSP	= "NA",
    se_comp_GSP_AltGSP	= "NA",
    P_value	= "NA",
    Mean_diff	= "NA",
    rounded	= "NA",
    Significant	= "NA",
    comparison	= "NA"
  )

dummy_alt_file

name_gsp <- paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/GSP/GSP_AltGSP_comp_", 
                   dplyr::distinct(all_results_1,paddock_ID_Type), ".csv")
name_gsp
write.csv(dummy_alt_file, name_gsp)
