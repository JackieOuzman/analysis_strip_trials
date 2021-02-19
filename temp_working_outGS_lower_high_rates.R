


str(strips)
GR_vs_low_High_rate <- strips %>% 
  filter(!is.na(zone_name)) %>% 
  dplyr::select(Zone_ID, SegmentID, YldMassDry, Rate, rate_name_order, rate_name, zone_name, Zone)
GR_vs_low_High_rate

#what is the growers rate?
GRrate <- dplyr::distinct(GR_vs_low_High_rate,rate_name, .keep_all = TRUE) %>% 
  filter(rate_name == "Grower_rate") %>% 
  dplyr::select(Rate)

GRrate
## add this to df
GR_vs_low_High_rate <- GR_vs_low_High_rate %>% 
  mutate(Rate_GSP = GRrate[1])
names(GR_vs_low_High_rate)

## is the GSP higher or lower than the GSP
GR_vs_low_High_rate <- GR_vs_low_High_rate %>% 
  mutate(
    GSP_high_low = case_when(
      Rate_GSP -  Rate > 0 ~  "lower_than_GSP",
      Rate_GSP -  Rate < 0 ~  "higher_than_GSP",
      Rate_GSP -  Rate == 0 ~  "the_GSP",
      TRUE ~ "other"))

 #how many rates are lower_than_GSP - this is checking how may are lower and how many higher
GR_vs_low_High_rate %>%  group_by(GSP_high_low, Rate) %>% 
  summarise(count= n())
      
 ## all good - if it wasnt I would need to adjust something??

unique(GR_vs_low_High_rate$GSP_high_low)   
# first I need to make anew clm for the comparsions
GR_vs_low_High_rate <- GR_vs_low_High_rate %>% 
  mutate(
    comparison_GSP_high = case_when(
      GSP_high_low == "the_GSP"         ~ "GSP_high",
      GSP_high_low == "higher_than_GSP" ~ "GSP_high",
      TRUE                      ~ "other"
    ),
    comparison_GSP_low = case_when(
      GSP_high_low == "the_GSP"         ~   "GSP_low",
      GSP_high_low == "lower_than_GSP"  ~   "GSP_low",
      TRUE                      ~ "other"
    )
  )

assign(paste0("grand_mean_std_error_", "GSP_high"), 
       function_grand_mean_std_error(GR_vs_low_High_rate,"GSP_high"))
assign(paste0("grand_mean_std_error_", "GSP_low"), 
       function_grand_mean_std_error(GR_vs_low_High_rate,"GSP_low"))

grand_mean_GSP_H_L_se <- left_join(grand_mean_std_error_GSP_high,grand_mean_std_error_GSP_low )


## I need to generate mean yield value for the zone and Rate
str(GR_vs_low_High_rate)

GR_vs_low_High_rate_2 <- GR_vs_low_High_rate %>% 
  group_by( Zone_ID, GSP_high_low) %>% 
  summarise(zone_yld = mean(YldMassDry, na.rm = TRUE))

str(GR_vs_low_High_rate_2)

GR_vs_low_High_rate_wide <- tidyr::pivot_wider(GR_vs_low_High_rate_2, 
                                               id_cols = c( Zone_ID),
                                               names_from =GSP_high_low,
                                               values_from = zone_yld
)

str(GR_vs_low_High_rate_wide)
## differences in yld clms
GR_vs_low_High_rate_wide <- GR_vs_low_High_rate_wide %>% 
  mutate(GSP_vs_lower = the_GSP   - lower_than_GSP,
         GSP_vs_higher = the_GSP  - higher_than_GSP)
str(GR_vs_low_High_rate_wide)

GR_vs_low_High_rate_wide <- ungroup(GR_vs_low_High_rate_wide)

GR_vs_low_High_rate_wide
grand_mean_GSP_H_L_se


GR_vs_low_High_rate_wide <- left_join(GR_vs_low_High_rate_wide, grand_mean_GSP_H_L_se)

#####
str(GR_vs_low_High_rate_wide)
GR_vs_low_High_rate_summary <- GR_vs_low_High_rate_wide %>%
  mutate(
    yld_resposne_GSP_v_low =  case_when(
      GSP_vs_lower > 0 + se_comp_GSP_low ~ "positive",
      GSP_vs_lower < 0 - se_comp_GSP_low ~ "negative",
      TRUE ~ "no_response"
    ),
    yld_resposne_GSP_v_high =  case_when(
      GSP_vs_higher  > 0 +  se_comp_GSP_high ~ "negative",
      GSP_vs_higher  < 0 -  se_comp_GSP_high ~ "positive",
      TRUE ~ "no_response"
    )
  )

str(GR_vs_low_High_rate_summary)

GR_vs_low_High_rate_summary <- GR_vs_low_High_rate_summary %>%
  tidyr::pivot_longer(
    cols = c("yld_resposne_GSP_v_low", "yld_resposne_GSP_v_high"),
    names_to = "comparison",
    values_to = "yld_response"
  ) %>%
  dplyr::select(
    Zone_ID,
    comparison,
    yld_response,
    higher_than_GSP,
    lower_than_GSP,
    the_GSP,
    GSP_vs_lower,
    GSP_vs_higher,
    se_comp_GSP_low,
    se_comp_GSP_high
  ) %>% 
   mutate(
     comparison = case_when(
       comparison == "yld_resposne_GSP_v_low"  ~ "GSP_v_low",
       comparison == "yld_resposne_GSP_v_high" ~ "GSP_v_high"
     ))

GR_vs_low_High_rate_summary

### Extra t test #######################################################################################################################

#Prep the data making a sub selection of df for each zone and run the paired t test

str(GR_vs_low_High_rate)
unique(GR_vs_low_High_rate$GSP_high_low)

function_paired_ttest_GR_low_high <- function(GR_vs_low_High_rate, zone_x){
  
  #select the zone data and the high vs low rates
  zone_x_GSP_vs_low <- GR_vs_low_High_rate %>% 
    filter(zone_name == paste0("zone", zone_x)) %>%
    filter(GSP_high_low == "the_GSP" | GSP_high_low == "lower_than_GSP")
  
  #average the yld per segment and rate
  zone_x_GSP_vs_low_av <- group_by(zone_x_GSP_vs_low, SegmentID, Rate, Zone, rate_name, zone_name , GSP_high_low) %>% 
    summarise_all(mean, na.rm= TRUE)
  #ensure that the dataset is duplictaed
  list_SegmentID_values_GSP_l <- zone_x_GSP_vs_low_av$SegmentID[duplicated(zone_x_GSP_vs_low$SegmentID)] #this returns a list of values I want to keep
  zone_x_GSP_vs_low_av <- zone_x_GSP_vs_low_av %>% filter(SegmentID %in% list_SegmentID_values_GSP_l)
  # run paired ttest
  zone_x_GSP_vs_low_res <- t.test(YldMassDry ~ GSP_high_low, data = zone_x_GSP_vs_low_av, paired = TRUE)
  
  #####test results
  # Report values from the t.test
  zone_x_GSP_vs_low_res_sig <-
    data.frame(P_value = as.double(zone_x_GSP_vs_low_res$p.value),
               Mean_diff = (zone_x_GSP_vs_low_res$estimate)) %>%
    mutate(
      comparison = "GSP_v_low",
      zone = paste0("zone", zone_x),
      rounded = abs(round(Mean_diff, 2)),
      Significant = case_when(P_value < 0.05 ~ "significant",
                              TRUE ~ "not significant"))
  zone_x_GSP_vs_low_res_sig 
  
  ##########################################################################################################################
  #select the zone data and the GSP vs high rates
  zone_x_GSP_vs_high <- GR_vs_low_High_rate %>% 
    filter(zone_name == paste0("zone", zone_x)) %>%
    filter(GSP_high_low == "the_GSP" | GSP_high_low == "higher_than_GSP")
  
  #average the yld per segment and rate
  zone_x_GSP_vs_high_av <- group_by(zone_x_GSP_vs_high, SegmentID, Rate, Zone, rate_name, zone_name , GSP_high_low) %>% 
    summarise_all(mean, na.rm= TRUE)
  #ensure that the dataset is duplictaed
  list_SegmentID_values_GSP_h <- zone_x_GSP_vs_high_av$SegmentID[duplicated(zone_x_GSP_vs_high$SegmentID)] #this returns a list of values I want to keep
  zone_x_GSP_vs_high_av <- zone_x_GSP_vs_high_av %>% filter(SegmentID %in% list_SegmentID_values_GSP_h)
  # run paired ttest
  zone_x_GSP_vs_high_res <- t.test(YldMassDry ~ GSP_high_low, data = zone_x_GSP_vs_high_av, paired = TRUE)
  
  #####test results
  # Report values from the t.test
  zone_x_GSP_vs_high_res_sig <-
    data.frame(P_value = as.double(zone_x_GSP_vs_high_res$p.value),
               Mean_diff = (zone_x_GSP_vs_high_res$estimate)) %>%
    mutate(
      comparison = "GSP_v_high",
      zone = paste0("zone", zone_x),
      rounded = abs(round(Mean_diff, 2)),
      Significant = case_when(P_value < 0.05 ~ "significant",
                              TRUE ~ "not significant"))
  zone_x_GSP_vs_high_res_sig 
  
  
  
  zone_x_GSP_vs_low_res_sig
  zone_x_GSP_vs_high_res_sig
   
  
  zone_x_GSP_vs_low_vs_high_res_sig <- rbind(zone_x_GSP_vs_low_res_sig, 
                                                zone_x_GSP_vs_high_res_sig)
  
  return(data.frame(zone_x_GSP_vs_low_vs_high_res_sig))
}
assign(paste0("GSP_low_vs_high", "zone_", "1"), function_paired_ttest_GR_low_high(GR_vs_low_High_rate, 1))
assign(paste0("GSP_low_vs_high","zone_", "2"), function_paired_ttest_GR_low_high(GR_vs_low_High_rate, 2))



GSP_low_vs_high_all <- rbind(GSP_low_vs_highzone_1, GSP_low_vs_highzone_2) 
GSP_low_vs_high_all <- left_join(GSP_low_vs_high_all, Zone_labels, by = c("zone"=  "zone_name"))


## turn GR_vs_low_High_rate_summary to narrow format
str(GR_vs_low_High_rate_summary)
str(GSP_low_vs_high_all)

GR_vs_low_High_rate_summary <- left_join(GR_vs_low_High_rate_summary, GSP_low_vs_high_all, by = c("Zone_ID", "comparison"))

names(GR_vs_low_High_rate_summary)

GR_vs_low_High_rate_summary <- GR_vs_low_High_rate_summary %>%
  dplyr::select(
    Zone_ID,
    Zone,
    comparison,
    yld_response,
    lower_than_GSP,
    the_GSP,
    GSP_vs_lower,
    GSP_vs_lower  ,
    GSP_vs_higher ,
    se_comp_GSP_low,
    se_comp_GSP_high,
    Significant,
    P_value
  )

## add in a few clms that help later
GR_vs_low_High_rate_summary <- GR_vs_low_High_rate_summary %>% 
  mutate(paddock_ID = unique(strips$Paddock_ID),
         Strip_Type = unique(strips$Strip_Type),
         input_file = input_file)


#save the output
name_CSP_low_high <- paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/GSP_low_high_comparision/GSP_low_high_comp_", 
               dplyr::distinct(all_results_1,paddock_ID_Type), ".csv")

write.csv(GR_vs_low_High_rate_summary, name_CSP_low_high)









