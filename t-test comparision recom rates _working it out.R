

## this goes with Jampot 

## perhaps this should be a stand alone script that I run at the end???
# I am accessing df called
# strips
# paddock_ID_1
#ooh an a heap of function

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
## add this to df
recom_rate1 <- recom_rate1 %>% 
  mutate(rec_rate_p = Rec_rate_p[1])
names(recom_rate1)

## is the GSP higher or lower than the GSP
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
    ),
    
    comparison_rec_rate_high_n = case_when(
      rec_rate_high_low_n == "rec_rate_n"           ~   "rec_rate_high_n",
      rec_rate_high_low_n == "higher_than_rec_rate_n" ~ "rec_rate_high_n",
      TRUE                      ~ "other"
    ),
    comparison_rec_rate_low_n = case_when(
      rec_rate_high_low_n == "rec_rate_p"         ~       "rec_rate_low_n",
      rec_rate_high_low_n == "lower_than_rec_rate_n"  ~   "rec_rate_low_n",
      TRUE                      ~ "other"
    )
  )

########################################################################################################################################
####### up to here #####

unique(GR_vs_low_High_rate$comparison_GSP_high)
names(recom_rate1)
unique(recom_rate1$comparison_rec_rate_high_p)
unique(recom_rate1$comparison_rec_rate_low_p)

#some wont run if there arent enough higher or lower...
assign(paste0("grand_mean_std_error_", "rec_rate_high"), 
       function_grand_mean_std_error(recom_rate1,"rec_rate_high_p"))


assign(paste0("grand_mean_std_error_", "rec_rate_low_p"), 
       function_grand_mean_std_error(GR_vs_low_High_rate,"rec_rate_low_p"))

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

str(rec_rate_p_vs_low_High_wide)
names(rec_rate_p_vs_low_High_wide)

## differences in yld clms
rec_rate_p_vs_low_High_wide <- rec_rate_p_vs_low_High_wide %>% 
  mutate(
         #rec_rate_p_vs_lower = rec_rate_p   - lower_than_rec_rate_p,
         rec_rate_p_vs_higher = rec_rate_p  - higher_than_rec_rate_p
         )
str(rec_rate_p_vs_low_High_wide)

rec_rate_p_vs_low_High_wide <- ungroup(rec_rate_p_vs_low_High_wide)

rec_rate_p_vs_low_High_wide





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


GR_vs_low_High_rate_summary
assigned_names2
GR_vs_low_High_rate_summary <- cbind(GR_vs_low_High_rate_summary,assigned_names2)
GR_vs_low_High_rate_summary
#save the output
name_CSP_low_high <- paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/GSP_low_high_comparision/GSP_low_high_comp_", 
                            dplyr::distinct(all_results_1,paddock_ID_Type), ".csv")

write.csv(GR_vs_low_High_rate_summary, name_CSP_low_high)

