
### around line 1079

###############################################################################################
### !!!! User input !!!! #####################################################################
###############################################################################################
#how many rates are lower_than_GSP - this is checking how may are lower and how many higher
GR_vs_low_High_rate %>%  group_by(GSP_high_low, Rate) %>% 
  summarise(count= n())


## filter out one rate so we only have gsp rate, lower than and higher than
# try and aim for sensible rates not zero if it can be avoided
# 1/2 the GSP rate and *2 GSP rate
GR_vs_low_High_rate %>%  group_by(GSP_high_low, Rate, Zone_ID, zone_name) %>% 
  summarise(count= n()) %>% 
  filter(GSP_high_low  == "the_GSP" ) %>% 
  mutate(double_GSP_rate = Rate*2,
         half_GPS_rate = Rate*.5)

## filter out one rate so we only have 3
GR_vs_low_High_rate <- GR_vs_low_High_rate %>% 
  filter(Rate != 96)





#######################################################
### add this code in line 1310 ###
#######################################################

GR_vs_low_High_rate <- data.frame(GR_vs_low_High_rate)

label_GR_v_rates <- GR_vs_low_High_rate %>%  group_by(GSP_high_low,
                                                      Rate,
                                                      #Strip_Rate,
                                                      Zone_ID) %>%
  summarise(count = n())
label_GR_v_rates <- ungroup(label_GR_v_rates) %>% 
  dplyr::select( GSP_high_low, Rate, Zone_ID)

label_GR_v_rates <- tidyr::pivot_wider(
  label_GR_v_rates,
  names_from = GSP_high_low ,
  values_from = Rate
)
label_GR_v_rates <- data.frame(label_GR_v_rates)


label_GR_v_rates <-label_GR_v_rates %>% rename(
  higher_than_GSP_label = higher_than_GSP,
  lower_than_GSP_label = lower_than_GSP,
  the_GSP_label = the_GSP)

GR_vs_low_High_rate_summary <- full_join(GR_vs_low_High_rate_summary, label_GR_v_rates)





#save the output
name_CSP_low_high <- paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/GSP_low_high_comparision/GSP_low_high_comp_", 
                            dplyr::distinct(all_results_1,paddock_ID_Type), ".csv")

write.csv(GR_vs_low_High_rate_summary, name_CSP_low_high)






