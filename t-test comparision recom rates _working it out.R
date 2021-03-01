

## this goes with Jampot 

## perhaps this should be a stand alone script that I run at the end???
# I am accessing df called
# strips
# paddock_ID_1
#Zone_labels
#input_file
#assigned_names2
#all_results_1

#remove all files expect...
rm(list = ls()[!ls() %in% c("strips", 
                            "paddock_ID_1",
                            "paddock_ID_2",
                            "Zone_labels",
                            "input_file",
                            "assigned_names2",
                            "all_results_1",
                            "function_grand_mean_std_error")])


recom_rateDB <- read_excel( "W:/value_soil_testing_prj/Yield_data/2020/processing/GRDC 2020 Paddock Database_SA_VIC_Feb24.xlsx")
##########################################################################################################################################
### Extra analysis for ricks tables GSP vs low high comparision 
recom_rateDB <- recom_rateDB %>% 
  dplyr::select(Zone_ID =    `Paddock code` ,
                p_rec =           `P rec`,
                n_rec_yld_low =   `N Rec (< 3 t/ha)` ,       
                n_rec_yld_med =   `N Rec (3-5 t/ha)` ,             
                n_rec_yld_high =  `N Rec (> 5 t/ha)`
                ) 

recom_rateDB <-  dplyr::mutate(recom_rateDB,  maxN = apply(recom_rateDB[3:5], 1, max, na.rm = TRUE))

str(recom_rateDB)
# remove redunant clm and replace inf
recom_rateDB <- recom_rateDB %>% 
  mutate(
    maxN = case_when(
      maxN > 0 ~ maxN,
      TRUE ~ NA_real_
    )
  )
recom_rateDB <- recom_rateDB %>% 
  dplyr::select(Zone_ID ,
                p_rec,
                N_rec = maxN
  ) 


str(strips)
rec_rates <- strips %>% 
  filter(!is.na(zone_name)) %>% 
  dplyr::select(Zone_ID, SegmentID, YldMassDry, Rate, rate_name_order, rate_name, zone_name, Zone, Strip_Type)
rec_rates


#put the tow files togther
str(rec_rates)
str(recom_rateDB)

recom_rate1 <- left_join( rec_rates, recom_rateDB)
## bring in the fert rates applied cal
fert_app_all_steps <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step2_fert_app_all_steps.csv")

fert_app_all_steps <- fert_app_all_steps %>% 
  dplyr::filter(Paddock_ID == substr(paddock_ID_1, start = 1, stop = 5)|
                Paddock_ID == substr(paddock_ID_2, start = 1, stop = 5)) %>% 
  dplyr::select( Paddock_ID, Rate, Strip_Rate, Total_sum_P_content, Total_sum_N_content)


recom_rate1 <- left_join(recom_rate1, fert_app_all_steps)
str(recom_rate1)

###############################################################################################################
## what are the comparision I want to make


recom_rate1_summary <- recom_rate1 %>%  group_by(Zone_ID,
                                                 Rate) %>%
  summarise(
     p_rec = max(p_rec, na.rm = TRUE),
     P_content = max(Total_sum_P_content, na.rm = TRUE),
     n_rec = max(N_rec, na.rm = TRUE),
     N_content = max(Total_sum_N_content, na.rm = TRUE)

  )
recom_rate1_summary <- ungroup(recom_rate1_summary)
recom_rate1_summary[] <- Map(function(x) replace(x, is.infinite(x), NA), recom_rate1_summary)




## do the difference for P 
recom_rate1_summary <- recom_rate1_summary %>% 
  dplyr::mutate(difference_p = abs(p_rec - P_content)) %>% 
  arrange(difference_p)  
 
recom_rate1_summary <- ungroup(recom_rate1_summary)
recom_rate1_summary

recom_rate1_summary <- recom_rate1_summary %>% 
  dplyr::mutate(
    approx_p_rec = 
      dplyr::case_when(
     difference_p == min(recom_rate1_summary$difference_p) ~ "best_match",
     difference_p == min(recom_rate1_summary$difference_p[recom_rate1_summary$difference_p!=min(recom_rate1_summary$difference_p)] ) ~ "rate1",
     #difference_p == max(recom_rate1_summary$difference_p[recom_rate1_summary$difference_p!=max(recom_rate1_summary$difference_p)] ) ~ "rate2", #use this if you have 4 rates best match rate 1 -3
     difference_p == max(recom_rate1_summary$difference_p) ~ "rate2",
    TRUE ~ as.character(Rate)))
    
## do the difference for n 
recom_rate1_summary <- recom_rate1_summary %>% 
  dplyr::mutate(difference_n = abs(n_rec - N_content)) %>% 
  arrange(difference_n)  

recom_rate1_summary <- ungroup(recom_rate1_summary)
recom_rate1_summary

recom_rate1_summary <- recom_rate1_summary %>% 
  dplyr::mutate(
    approx_n_rec = 
      dplyr::case_when(
        difference_n == min(recom_rate1_summary$difference_n) ~ "best_match",
        difference_n == min(recom_rate1_summary$difference_n[recom_rate1_summary$difference_n!=min(recom_rate1_summary$difference_n)] ) ~ "rate1",
        #difference_n == max(recom_rate1_summary$difference_n[recom_rate1_summary$difference_n!=max(recom_rate1_summary$difference_n)] ) ~ "rate2", #use this if you have 4 rates best match rate 1 -3
        difference_n == max(recom_rate1_summary$difference_n) ~ "rate2",
        TRUE ~ as.character(Rate)))

recom_rate1_summary
#what is the samllest value in the difference clm


#what is the recommed rate for p?
Rec_rate_p <- dplyr::distinct(recom_rate1_summary,Rate, .keep_all = TRUE) %>% 
  filter(approx_p_rec == "best_match") %>% 
  dplyr::select(Rate)

Rec_rate_p
## add this to df with all the yield data 
recom_rate1 <- recom_rate1 %>% 
  mutate(rec_rate_p = Rec_rate_p[1])
names(recom_rate1)

## is the rec higher or lower than the rec
recom_rate1 <- recom_rate1 %>% 
  mutate(
    rec_rate_high_low_p = case_when(
      rec_rate_p -  Total_sum_P_content > 0 ~  "lower_than_rec_rate_p",
      rec_rate_p -  Total_sum_P_content < 0 ~  "higher_than_rec_rate_p",
      rec_rate_p -  Total_sum_P_content == 0 ~  "rec_rate_p",
      TRUE ~ "other"))

#what is the recommed rate for n?
Rec_rate_n <- dplyr::distinct(recom_rate1_summary,Rate, .keep_all = TRUE) %>% 
  filter(approx_n_rec == "best_match") %>% 
  dplyr::select(Rate)

Rec_rate_n
recom_rate1
## add this to df
recom_rate1 <- recom_rate1 %>% 
  mutate(rec_rate_n = Rec_rate_n[1])

names(recom_rate1)

## is the GSP higher or lower than the rec rate
recom_rate1 <- recom_rate1 %>% 
  mutate(
    rec_rate_high_low_n = case_when(
      rec_rate_n -  Total_sum_N_content > 0 ~  "lower_than_rec_rate_n",
      rec_rate_n -  Total_sum_N_content < 0 ~  "higher_than_rec_rate_n",
      rec_rate_n -  Total_sum_N_content == 0 ~  "rec_rate_n",
      TRUE ~ "other"))

str(recom_rate1$rec_rate_high_low_n)
str(recom_rate1$rec_rate_high_low_p)

#how many rates are lower_than_rec rate - this is checking how may are lower and how many higher
# what trial is it first?
unique(recom_rate1$Strip_Type)
# for P
recom_rate1 %>%  group_by(rec_rate_high_low_p, Rate) %>% 
  summarise(count= n())
#for n
# recom_rate1 %>%  group_by(rec_rate_high_low_n, Rate) %>% 
#   summarise(count= n())


## all good - if it wasnt I would need to adjust something??

## filter out one rate so we only have 3
recom_rate1 <- recom_rate1 %>% 
   filter(Rate != 50) # make a note of this in DB

unique(recom_rate1$rec_rate_high_low_p)

names(recom_rate1)
# first I need to make a new clm for the comparsions
recom_rate1 <- recom_rate1 %>% 
  mutate(
    comparison_rec_rate_high_p = case_when(
      rec_rate_high_low_p == "rec_rate_p"           ~   "rec_rate_high_p",
      rec_rate_high_low_p == "higher_than_rec_rate_p" ~ "rec_rate_high_p",
      TRUE                      ~ "other"
    ),
    comparison_rec_rate_low_p = case_when(
      rec_rate_high_low_p == "rec_rate_p"         ~       "rec_rate_low_p",
      rec_rate_high_low_p == "lower_than_rec_rate_p"  ~   "rec_rate_low_p",
      TRUE                      ~ "other"
    # ),
    # 
    # comparison_rec_rate_high_n = case_when(
    #   rec_rate_high_low_n == "rec_rate_n"           ~   "rec_rate_high_n",
    #   rec_rate_high_low_n == "higher_than_rec_rate_n" ~ "rec_rate_high_n",
    #   TRUE                      ~ "other"
    # ),
    # comparison_rec_rate_low_n = case_when(
    #   rec_rate_high_low_n == "rec_rate_p"         ~       "rec_rate_low_n",
    #   rec_rate_high_low_n == "lower_than_rec_rate_n"  ~   "rec_rate_low_n",
    #   TRUE                      ~ "other"
    )
  )

########################################################################################################################################



#some won't run if there arent enough higher or lower...
assign(paste0("grand_mean_std_error_", "rec_rate_high"), 
       function_grand_mean_std_error(recom_rate1,"rec_rate_high_p"))


# assign(paste0("grand_mean_std_error_", "rec_rate_low_p"), 
#        function_grand_mean_std_error(GR_vs_low_High_rate,"rec_rate_low_p"))

#if I have both high and low then I can join them togther
# grand_mean_recom_rate_H_L_se <- left_join(grand_mean_std_error_rec_rate_high,
#                                           grand_mean_std_error_rec_rate_low_p 
#                                           )

grand_mean_recom_rate_H_L_se <- grand_mean_std_error_rec_rate_high
                                          

## I need to generate mean yield value for the zone and Rate
str(recom_rate1)

## for P If the trial is N this needs to be changed
rec_rate_p_vs_low_High <- recom_rate1 %>% 
  group_by( Zone_ID, rec_rate_high_low_p) %>% 
  summarise(zone_yld = mean(YldMassDry, na.rm = TRUE))

str(rec_rate_p_vs_low_High)

rec_rate_p_vs_low_High_wide <- tidyr::pivot_wider(rec_rate_p_vs_low_High, 
                                               id_cols = c( Zone_ID),
                                               names_from =rec_rate_high_low_p,
                                               values_from = zone_yld
)

rec_rate_p_vs_low_High_wide
names(rec_rate_p_vs_low_High_wide)

## differences in yld clms
rec_rate_p_vs_low_High_wide <- rec_rate_p_vs_low_High_wide %>% 
  mutate(
         #rec_rate_p_vs_lower = rec_rate_p   - lower_than_rec_rate_p,
         rec_rate_p_vs_higher = rec_rate_p  - higher_than_rec_rate_p
         )
rec_rate_p_vs_low_High_wide

rec_rate_p_vs_low_High_wide <- ungroup(rec_rate_p_vs_low_High_wide)

rec_rate_p_vs_low_High_wide
grand_mean_recom_rate_H_L_se


rec_rate_p_vs_low_High_wide <- left_join(rec_rate_p_vs_low_High_wide, grand_mean_recom_rate_H_L_se)

#####
(rec_rate_p_vs_low_High_wide)
rec_rate_p_vs_low_High_summary <- rec_rate_p_vs_low_High_wide %>%
  mutate(
    # yld_resposne_rec_v_low =  case_when(
    #   rec_rate_p_vs_lower > 0 + se_comp_rec_rate_low_p ~ "positive",
    #   rec_rate_p_vs_lower < 0 - se_comp_rec_rate_low_p ~ "negative",
    #   TRUE ~ "no_response"
    # ),
    yld_resposne_rec_v_high =  case_when(
      rec_rate_p_vs_higher  > 0 +  se_comp_rec_rate_high_p ~ "negative",
      rec_rate_p_vs_higher  < 0 -  se_comp_rec_rate_high_p ~ "positive",
      TRUE ~ "no_response"
    )
  )

str(rec_rate_p_vs_low_High_summary)


### stuck here
rec_rate_p_vs_low_High_summary <- rec_rate_p_vs_low_High_summary %>%
  tidyr::pivot_longer(
    cols = c("yld_resposne_rec_v_high"), 
             #"yld_resposne_rec_v_low"),
    names_to = "comparison",
    values_to = "yld_response"
  ) %>%
  dplyr::select(
    Zone_ID,
    comparison,
    yld_response,
    higher_than_rec_rate_p,
    #lower_than_rec_rate_p,
    rec_rate_p,
    #rec_rate_p_vs_lower,
    rec_rate_p_vs_higher,
    #se_comp_rec_rate_low_p ,
    se_comp_rec_rate_high_p 
  ) %>% 
  mutate(
    comparison = case_when(
      comparison == "yld_resposne_rec_v_low"  ~ "rec_p_v_low",
      comparison == "yld_resposne_rec_v_high" ~ "rec_p_v_high"
    ))

rec_rate_p_vs_low_High_summary

### Extra t test #######################################################################################################################

#Prep the data making a sub selection of df for each zone and run the paired t test

str(recom_rate1)
unique(recom_rate1$rec_rate_high_low_p )

function_paired_ttest_GR_low_high <- function(recom_rate1, zone_x){
  
  #select the zone data and the high vs low rates
  zone_x_rec_r_p_vs_high <- recom_rate1 %>% 
    filter(zone_name == paste0("zone", zone_x)) %>%
    filter(rec_rate_high_low_p == "rec_rate_p" | rec_rate_high_low_p == "higher_than_rec_rate_p")
  
  #average the yld per segment and rate
  zone_x_rec_p_vs_high_av <- group_by(zone_x_rec_r_p_vs_high, SegmentID, Rate, Zone, rate_name, zone_name , rec_rate_high_low_p) %>% 
    summarise_all(mean, na.rm= TRUE)
  #ensure that the dataset is duplictaed
  list_SegmentID_values_rec_rate_l <- zone_x_rec_p_vs_high_av$SegmentID[duplicated(zone_x_rec_r_p_vs_high$SegmentID)] #this returns a list of values I want to keep
  zone_x_rec_p_vs_high_av <- zone_x_rec_p_vs_high_av %>% filter(SegmentID %in% list_SegmentID_values_rec_rate_l)
  # run paired ttest
  zone_x_rec_rate_p_vs_high_res <- t.test(YldMassDry ~ rec_rate_high_low_p, data = zone_x_rec_p_vs_high_av, paired = TRUE)
  
  #####test results
  # Report values from the t.test
  zone_x_rec_r_p_vs_high_res_sig <-
    data.frame(P_value = as.double(zone_x_rec_rate_p_vs_high_res$p.value),
               Mean_diff = (zone_x_rec_rate_p_vs_high_res$estimate)) %>%
    mutate(
      comparison = "rec_p_v_high",
      zone = paste0("zone", zone_x),
      rounded = abs(round(Mean_diff, 2)),
      Significant = case_when(P_value < 0.05 ~ "significant",
                              TRUE ~ "not significant"))
  zone_x_rec_r_p_vs_high_res_sig 
  
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



rec_rate_p_low_vs_high_all <- rbind(rec_rate_p_vs_low_highzone_1, rec_rate_p_vs_low_highzone_2) 
rec_rate_p_low_vs_high_all <- left_join(rec_rate_p_low_vs_high_all, Zone_labels, by = c("zone"=  "zone_name"))


## turn GR_vs_low_High_rate_summary to narrow format
str(rec_rate_p_vs_low_High_summary)
str(rec_rate_p_low_vs_high_all)

rec_rate_p_vs_low_High_summary <- left_join(rec_rate_p_vs_low_High_summary, rec_rate_p_low_vs_high_all, by = c("Zone_ID", "comparison"))

names(rec_rate_p_vs_low_High_summary)

rec_rate_p_vs_low_High_summary <- rec_rate_p_vs_low_High_summary %>%
  dplyr::select(
    Zone_ID,
    Zone,
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


#save the output
name_rec_rate_low_high <- paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_", 
                            dplyr::distinct(all_results_1,paddock_ID_Type), ".csv")
name_rec_rate_low_high
write.csv(rec_rate_p_vs_low_High_summary, name_rec_rate_low_high)




