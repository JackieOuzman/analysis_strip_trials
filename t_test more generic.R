## modify the function 


### Extra t test #######################################################################################################################

#Prep the data making a sub selection of df for each zone and run the paired t test

str(recom_rate1)
unique(recom_rate1$rec_rate_high_low_p )
recom_rate1 <- recom_rate1
zone_x <- 1
comp <- "lower"



# this is a check what comaprison I have
recom_rate1 %>%  group_by(rec_rate_high_low_p, Rate, Zone_ID, zone_name) %>% 
  summarise(count= n()) %>% 
  arrange(Zone_ID, Rate) %>% 
  group_by(zone_name) %>% 
  distinct(rec_rate_high_low_p)%>% 
  filter(rec_rate_high_low_p != "rec_rate_p") %>% 
  arrange(rec_rate_high_low_p)


 # "higher_than_rec_rate_p" # or lower_than_rec_rate_p

#function_paired_ttest_rec_rate_low_high <- function(recom_rate1, zone_x, comp){
  
  #select the zone data and the high vs low rates
  zone_x_rec_r_p_vs_x <- recom_rate1 %>% 
    filter(zone_name == paste0("zone", zone_x)) %>%
    filter(rec_rate_high_low_p == "rec_rate_p" | rec_rate_high_low_p == paste0(comp,"_than_rec_rate_p"))
  
  # I am happy with the filter
  
  
  
  #average the yld per segment and rate
  zone_x_rec_r_p_vs_x_av <- group_by(zone_x_rec_r_p_vs_high, 
                                     SegmentID, Rate, Zone, 
                                     rate_name, zone_name , rec_rate_high_low_p) %>% 
    summarise_all(mean, na.rm= TRUE)
  str(zone_x_rec_r_p_vs_x_av)
  #ensure that the dataset is duplictaed
  list_SegmentID_values_rec_rate_l <- zone_x_rec_r_p_vs_x_av$SegmentID[duplicated(zone_x_rec_r_p_vs_x$SegmentID)] #this returns a list of values I want to keep
  list_SegmentID_values_rec_rate_l
  zone_x_rec_r_p_vs_x_av <- zone_x_rec_r_p_vs_x_av %>% filter(SegmentID %in% list_SegmentID_values_rec_rate_l)
  str(zone_x_rec_r_p_vs_x_av)
 
  
  #### Stuck here
  
  
   # test <- zone_x_rec_r_p_vs_x %>% group_by(SegmentID, rec_rate_high_low_p) %>% 
  #   summarise(count= n())
  # test
  # run paired ttest
  zone_x_rec_rate_p_vs_x_res <- t.test(YldMassDry ~ rec_rate_high_low_p, 
                                       data = zone_x_rec_r_p_vs_x_av, paired = TRUE)
  
  #####test results
  # Report values from the t.test
  zone_x_rec_rate_p_vs_x_res_sig <-
    data.frame(P_value = as.double(zone_x_rec_rate_p_vs_x_res$p.value),
               Mean_diff = (zone_x_rec_rate_p_vs_x_res$estimate)) %>%
    mutate(
      comparison = paste0("rec_p_v_" comp),
      zone = paste0("zone", zone_x),
      rounded = abs(round(Mean_diff, 2)),
      Significant = case_when(P_value < 0.05 ~ "significant",
                              TRUE ~ "not significant"))
  zone_x_rec_rate_p_vs_x_res_sig 
  
  ##########################################################################################################################
  #select the zone data and the GSP vs high rates
  # zone_x_rec_r_p_vs_low <- recom_rate1 %>% 
  #   filter(zone_name == paste0("zone", zone_x)) %>%
  #   filter(rec_rate_high_low_p == "rec_rate_p" | rec_rate_high_low_p == "higher_than_rec_rate_p")
  # 
  # #average the yld per segment and rate
  # zone_x_rec_p_vs_low_av <- group_by(zone_x_rec_r_p_vs_high, SegmentID, Rate, Zone, rate_name, zone_name , rec_rate_high_low_p) %>% 
  #   summarise_all(mean, na.rm= TRUE)
  # #ensure that the dataset is duplictaed
  # list_SegmentID_values_rec_rate_2 <- zone_x_rec_p_vs_low_av$SegmentID[duplicated(zone_x_rec_r_p_vs_low$SegmentID)] #this returns a list of values I want to keep
  # zone_x_rec_p_vs_low_av <- zone_x_rec_p_vs_low_av %>% filter(SegmentID %in% list_SegmentID_values_rec_rate_2)
  # # run paired ttest
  # zone_x_rec_rate_p_vs_low_res <- t.test(YldMassDry ~ rec_rate_high_low_p, data = zone_x_rec_p_vs_low_av, paired = TRUE)
  # 
  # #####test results
  # # Report values from the t.test
  # zone_x_rec_r_p_vs_low_res_sig <-
  #   data.frame(P_value = as.double(zone_x_rec_rate_p_vs_low_res$p.value),
  #              Mean_diff = (zone_x_rec_rate_p_vs_low_res$estimate)) %>%
  #   mutate(
  #     comparison = "GSP_v_high",
  #     zone = paste0("zone", zone_x),
  #     rounded = abs(round(Mean_diff, 2)),
  #     Significant = case_when(P_value < 0.05 ~ "significant",
  #                             TRUE ~ "not significant"))
  # zone_x_rec_rate_p_vs_low_sig 
  
  
  
  #zone_x_rec_r_p_vs_low_res_sig
  zone_x_rec_r_p_vs_high_res_sig
  
  
  zone_x_rec_r_p_vs_low_vs_high_res_sig <- rbind(#zone_x_rec_r_p_vs_low_res_sig,
    zone_x_rec_r_p_vs_high_res_sig)
  
  return(data.frame(zone_x_rec_r_p_vs_low_vs_high_res_sig))
}
assign(paste0("rec_rate_p_vs_low_high", "zone_", "1"), function_paired_ttest_GR_low_high(recom_rate1, 1))
assign(paste0("rec_rate_p_vs_low_high","zone_", "2"), function_paired_ttest_GR_low_high(recom_rate1, 2))















