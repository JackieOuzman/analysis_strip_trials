## modify the function 


### Extra t test #######################################################################################################################

#Prep the data making a sub selection of df for each zone and run the paired t test



# this is a check what comaprison I have
# recom_rate1 %>%  group_by(rec_rate_high_low_p, Rate, Zone_ID, zone_name) %>% 
#   summarise(count= n()) %>% 
#   arrange(Zone_ID, Rate) %>% 
#   group_by(zone_name) %>% 
#   distinct(rec_rate_high_low_p)%>% 
#   filter(rec_rate_high_low_p != "rec_rate_p") %>% 
#   arrange(rec_rate_high_low_p)

#####################################################################################

function_paired_ttest_rec_rate_low_high <- function(recom_rate1, zone_x, comp){
  
  #select the zone data and the high vs low rates
  zone_x_rec_r_p_vs_x <- recom_rate1 %>% 
    filter(zone_name == paste0("zone", zone_x)) %>%
    filter(rec_rate_high_low_p == "rec_rate_p" | rec_rate_high_low_p == paste0(comp,"_than_rec_rate_p"))
  
  
  #average the yld per segment and rate
  zone_x_rec_r_p_vs_x_av <- group_by(zone_x_rec_r_p_vs_x, 
                                     SegmentID, Rate, Zone,Zone_ID, 
                                     rate_name, zone_name , rec_rate_high_low_p) %>% 
    summarise_all(mean, na.rm= TRUE)
  str(zone_x_rec_r_p_vs_x_av)
  #ensure that the dataset is duplictaed
  list_SegmentID_values_rec_rate_l <- zone_x_rec_r_p_vs_x_av$SegmentID[duplicated(zone_x_rec_r_p_vs_x$SegmentID)] #this returns a list of values I want to keep
  list_SegmentID_values_rec_rate_l
  zone_x_rec_r_p_vs_x_av <- zone_x_rec_r_p_vs_x_av %>% filter(SegmentID %in% list_SegmentID_values_rec_rate_l)
  str(zone_x_rec_r_p_vs_x_av)
  zone_x_rec_rate_p_vs_x_res <- t.test(YldMassDry ~ rec_rate_high_low_p, 
                                       data = zone_x_rec_r_p_vs_x_av, paired = TRUE)
  
  #####test results
  # Report values from the t.test
  zone_x_rec_rate_p_vs_x_res_sig <-
    data.frame(P_value = as.double(zone_x_rec_rate_p_vs_x_res$p.value),
               Mean_diff = (zone_x_rec_rate_p_vs_x_res$estimate)) %>%
    mutate(
      comparison = paste0("rec_p_v_", comp),
      zone = paste0("zone", zone_x),
      rounded = abs(round(Mean_diff, 2)),
      Significant = case_when(P_value < 0.05 ~ "significant",
                              TRUE ~ "not significant"))
  zone_x_rec_rate_p_vs_x_res_sig 
  
  return(data.frame(zone_x_rec_rate_p_vs_x_res_sig))
}

function(recom_rate1, zone_x, comp)

assign(paste0("rec_rate_p_vs_lower_", "zone_", "1"),function_paired_ttest_rec_rate_low_high(recom_rate1, 1, "lower"))
assign(paste0("rec_rate_p_vs_lower_","zone_", "2"),function_paired_ttest_rec_rate_low_high(recom_rate1, 2, "lower"))

assign(paste0("rec_rate_p_vs_higher_", "zone_", "1"),function_paired_ttest_rec_rate_low_high(recom_rate1, 1, "higher"))
assign(paste0("rec_rate_p_vs_higher_","zone_", "2"),function_paired_ttest_rec_rate_low_high(recom_rate1, 2, "higher"))


#what ran?
rec_rate_p_vs_lower_zone_1
rec_rate_p_vs_lower_zone_2

rec_rate_p_vs_higher_zone_1
rec_rate_p_vs_higher_zone_2

# this is a check what comaprison I have what was I expecting to run?
recom_rate1 %>%  group_by(rec_rate_high_low_p, Rate, Zone_ID, zone_name) %>% 
  summarise(count= n()) %>% 
  arrange(Zone_ID, Rate) %>% 
  group_by(zone_name) %>% 
  distinct(rec_rate_high_low_p)%>% 
  filter(rec_rate_high_low_p != "rec_rate_p") %>% 
  arrange(rec_rate_high_low_p)

rec_rate_p_low_vs_high_all <- rbind(rec_rate_p_vs_lower_zone_1,
                                    rec_rate_p_vs_lower_zone_2,
                                    
                                    #rec_rate_p_vs_higher_zone_1,
                                    rec_rate_p_vs_higher_zone_2
                                    )




## turn GR_vs_low_High_rate_summary to narrow format

## need t0 add in the zone name 
zoneID_zone_names <- recom_rate1 %>%  distinct(Zone_ID, .keep_all = TRUE) %>% 
  dplyr::select(Zone_ID,zone_name )
zoneID_zone_names

rec_rate_p_low_vs_high_all <- left_join(rec_rate_p_low_vs_high_all,zoneID_zone_names,
                                        by = c("zone" = "zone_name"))


### stuck here The rec_rate_p_vs_low_High_summary needs a clm with zone names

rec_rate_p_vs_low_High_summary
rec_rate_p_low_vs_high_all

rec_rate_p_vs_low_High_summary <- full_join(rec_rate_p_vs_low_High_summary, 
                  rec_rate_p_low_vs_high_all, by = c("Zone_ID", "comparison"))

names(rec_rate_p_vs_low_High_summary)

rec_rate_p_vs_low_High_summary <- rec_rate_p_vs_low_High_summary %>%
  dplyr::select(
    Zone_ID,
    zone,
    comparison,
    yld_response,
    higher_than_rec_rate_p,
    #lower_than_rec_rate_p,
    rec_rate_p,
    rec_rate_p_vs_higher,
    #rec_rate_p_vs_lower  ,
    se_comp_rec_rate_high_p,
    #se_comp_rec_rate_low_p,
    Significant,
    P_value
  )

## add in a few clms that help later
rec_rate_p_vs_low_High_summary <- rec_rate_p_vs_low_High_summary %>% 
  mutate(paddock_ID = unique(strips$Paddock_ID),
         Strip_Type = unique(strips$Strip_Type),
         input_file = input_file)


rec_rate_p_vs_low_High_summary
assigned_names2
rec_rate_p_vs_low_High_summary <- cbind(rec_rate_p_vs_low_High_summary,assigned_names2)
rec_rate_p_vs_low_High_summary

#what is the recommed rate?

label_rec_rates <- recom_rate1 %>%  group_by(rec_rate_high_low_p, Rate, Strip_Rate) %>% 
  summarise(count= n())
label_rec_rates <- ungroup(label_rec_rates) %>% 
  dplyr::select( rec_rate_high_low_p, Strip_Rate)
label_rec_rates <- tidyr::pivot_wider(
  label_rec_rates,
  names_from = rec_rate_high_low_p,
  values_from = Strip_Rate
)

rec_rate_p_vs_low_High_summary <- cbind(rec_rate_p_vs_low_High_summary, label_rec_rates)
rec_rate_p_vs_low_High_summary
View(rec_rate_p_vs_low_High_summary)

#save the output
name_rec_rate_low_high <- paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_", 
                                 dplyr::distinct(all_results_1,paddock_ID_Type), ".csv")
name_rec_rate_low_high
write.csv(rec_rate_p_vs_low_High_summary, name_rec_rate_low_high)  
  














