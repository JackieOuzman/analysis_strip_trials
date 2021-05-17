


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


recom_rateDB <- read_excel( "W:/value_soil_testing_prj/Yield_data/2020/processing/GRDC 2020 Paddock Database_SA_VIC_May05 2021.xlsx")
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

recom_rateDB$Zone_ID <- as.double(recom_rateDB$Zone_ID)

#put the tow files togther
str(rec_rates)
str(recom_rateDB)

recom_rate1 <- left_join( rec_rates, recom_rateDB)
recom_rate1 <- data.frame(recom_rate1)
str(recom_rate1)


## bring in the fert rates applied cal
fert_app_all_steps <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step2_fert_app_all_steps.csv")

fert_app_all_steps <- fert_app_all_steps %>% 
  dplyr::filter(Paddock_ID == substr(paddock_ID_1, start = 1, stop = 5)|
                Paddock_ID == substr(paddock_ID_2, start = 1, stop = 5)) %>% 
  dplyr::select( Paddock_ID, Rate, Strip_Rate, Total_sum_P_content, Total_sum_N_content)

str(fert_app_all_steps)
str(recom_rate1)

recom_rate1 <- left_join(recom_rate1, fert_app_all_steps)
str(recom_rate1)
View(recom_rate1)
###############################################################################################################
## what are the comparision I want to make

# names(recom_rate1)
# View(recom_rate1)
recom_rate1_summary <- recom_rate1 %>%  group_by(Zone_ID,
                                                 Rate, zone_name) %>%
  summarise(
     p_rec = max(p_rec, na.rm = TRUE),
     P_content = max(Total_sum_P_content, na.rm = TRUE),
     n_rec = max(N_rec, na.rm = TRUE),
     N_content = max(Total_sum_N_content, na.rm = TRUE)

  )
recom_rate1_summary <- ungroup(recom_rate1_summary)
recom_rate1_summary[] <- Map(function(x) replace(x, is.infinite(x), NA), recom_rate1_summary)
recom_rate1_summary <- data.frame(recom_rate1_summary)

str(recom_rate1_summary)


## do the difference for P 
recom_rate1_summary <- recom_rate1_summary %>% 
  dplyr::mutate(difference_p = abs(p_rec - P_content)) %>% 
  arrange(difference_p)  
str(recom_rate1_summary)

recom_rate1_summary <- ungroup(recom_rate1_summary)

str(recom_rate1_summary)

## Two steps need to filter data first (zone 1 and zone 2 need to have this as clm in 
recom_rate1_summary_zone1 <- recom_rate1_summary %>%
  filter(zone_name == "zone1")
recom_rate1_summary_zone2 <- recom_rate1_summary %>%
  filter(zone_name == "zone2")  

recom_rate1_summary_zone1
recom_rate1_summary_zone2

recom_rate1_summary_zone1 <-recom_rate1_summary_zone1 %>%
    dplyr::mutate(
    approx_p_rec =
      dplyr::case_when(
        difference_p == min(recom_rate1_summary_zone1$difference_p) ~ "best_match",
        difference_p == min(recom_rate1_summary_zone1$difference_p[recom_rate1_summary_zone1$difference_p !=
                                                                     min(recom_rate1_summary_zone1$difference_p)]) ~ "rate1",
        # difference_p == max(recom_rate1_summary_zone1$difference_p[recom_rate1_summary_zone1$difference_p !=
        #                                                              max(recom_rate1_summary_zone1$difference_p)]) ~ "rate2",
        #use this if you have 4 rates best match rate 1 -3
        #difference_p == max(recom_rate1_summary_zone1$difference_p) ~ "rate3",
        difference_p == max(recom_rate1_summary_zone1$difference_p) ~ "rate2",
        TRUE ~ as.character(Rate)
      )
  )

recom_rate1_summary_zone2 <-recom_rate1_summary_zone2 %>%
  dplyr::mutate(
    approx_p_rec =
      dplyr::case_when(
        difference_p == min(recom_rate1_summary_zone2$difference_p) ~ "best_match",
        difference_p == min(recom_rate1_summary_zone2$difference_p[recom_rate1_summary_zone2$difference_p !=
                                                                     min(recom_rate1_summary_zone2$difference_p)]) ~ "rate1",
        # difference_p == max(recom_rate1_summary_zone2$difference_p[recom_rate1_summary_zone2$difference_p !=
        #                                                              max(recom_rate1_summary_zone2$difference_p)]) ~ "rate2",
        #use this if you have 4 rates best match rate 1 -3
        difference_p == max(recom_rate1_summary_zone2$difference_p) ~ "rate2",
        #difference_p == max(recom_rate1_summary_zone2$difference_p) ~ "rate3",
        TRUE ~ as.character(Rate)
      )
  )

recom_rate1_summary_zone1
recom_rate1_summary_zone2
# put them back togther
recom_rate1_summary <- rbind(recom_rate1_summary_zone1, recom_rate1_summary_zone2)
rm(recom_rate1_summary_zone1, recom_rate1_summary_zone2)
str(recom_rate1_summary)



##########################################################################################################
    
## do the difference for n This needs more work
recom_rate1_summary <- recom_rate1_summary %>% 
  dplyr::mutate(difference_n = abs(n_rec - N_content)) %>% 
  arrange(difference_n)  

recom_rate1_summary <- ungroup(recom_rate1_summary)
str(recom_rate1_summary)

recom_rate1_summary_zone1 <- recom_rate1_summary %>%
  filter(zone_name == "zone1")
recom_rate1_summary_zone2 <- recom_rate1_summary %>%
  filter(zone_name == "zone2")  

recom_rate1_summary_zone1
recom_rate1_summary_zone2

recom_rate1_summary_zone1 <-recom_rate1_summary_zone1 %>%
  dplyr::mutate(
    approx_n_rec =
      dplyr::case_when(
        difference_n == min(recom_rate1_summary_zone1$difference_n) ~ "best_match",
        difference_n == min(recom_rate1_summary_zone1$difference_n[recom_rate1_summary_zone1$difference_n !=
                                                                     min(recom_rate1_summary_zone1$difference_n)]) ~ "rate1",
        difference_n == max(recom_rate1_summary_zone1$difference_n[recom_rate1_summary_zone1$difference_n !=
                                                                     max(recom_rate1_summary_zone1$difference_n)]) ~ "rate2",
        #use this if you have 4 rates best match rate 1 -3
        difference_n == max(recom_rate1_summary_zone1$difference_n) ~ "rate3",
        #difference_n == max(recom_rate1_summary_zone1$difference_n) ~ "rate2",
        TRUE ~ as.character(Rate)
      )
  )

recom_rate1_summary_zone2 <-recom_rate1_summary_zone2 %>%
  dplyr::mutate(
    approx_n_rec =
      dplyr::case_when(
        difference_p == min(recom_rate1_summary_zone2$difference_n) ~ "best_match",
        difference_p == min(recom_rate1_summary_zone2$difference_n[recom_rate1_summary_zone2$difference_n !=
                                                                     min(recom_rate1_summary_zone2$difference_n)]) ~ "rate1",
        difference_p == max(recom_rate1_summary_zone2$difference_n[recom_rate1_summary_zone2$difference_n !=
                                                                     max(recom_rate1_summary_zone2$difference_n)]) ~ "rate2",
        #use this if you have 4 rates best match rate 1 -3
        difference_p == max(recom_rate1_summary_zone2$difference_n) ~ "rate3",
        #difference_n == max(recom_rate1_summary_zone2$difference_n) ~ "rate2",
        TRUE ~ as.character(Rate)
      )
  )

recom_rate1_summary_zone1
recom_rate1_summary_zone2
# put them back togther
recom_rate1_summary <- rbind(recom_rate1_summary_zone1, recom_rate1_summary_zone2)
rm(recom_rate1_summary_zone1, recom_rate1_summary_zone2)
recom_rate1_summary

###################################################################
str(recom_rate1_summary)


#what is the recommed rate for p?
Rec_rate_p <- recom_rate1_summary %>%  filter(approx_p_rec == "best_match") %>% 
  dplyr::select( Zone_ID, P_content) %>% 
  rename( rec_rate_p = P_content)

Rec_rate_p
## add this to df with all the yield data 
names(recom_rate1)
names(Rec_rate_p)


recom_rate1 <- left_join(recom_rate1, Rec_rate_p, by ="Zone_ID")
head(recom_rate1)
  
  ## is the rec higher or lower than the rec
recom_rate1 <- recom_rate1 %>% 
  mutate(
    rec_rate_high_low_p = case_when(
      rec_rate_p -  Total_sum_P_content > 0 ~  "lower_than_rec_rate_p",
      rec_rate_p -  Total_sum_P_content < 0 ~  "higher_than_rec_rate_p",
      rec_rate_p -  Total_sum_P_content == 0 ~  "rec_rate_p",
      TRUE ~ "other"))


#check this is correct
# test <- recom_rate1 %>% 
#   mutate(
#     rec_rate_high_low_p = rec_rate_p -  Total_sum_P_content)

#what is the recommed rate for p?
Rec_rate_n <- recom_rate1_summary %>%  filter(approx_n_rec == "best_match") %>% 
  dplyr::select( Zone_ID, N_content) %>% 
  rename( rec_rate_n = N_content)

Rec_rate_n
## add this to df with all the yield data 
names(recom_rate1)
names(Rec_rate_n)



#what is the recommed rate for n?
Rec_rate_n <- dplyr::distinct(recom_rate1_summary,Rate, .keep_all = TRUE) %>% 
  filter(approx_n_rec == "best_match") %>% 
  dplyr::select(Rate)

Rec_rate_n_value <- as.character(Rec_rate_n[1])
Rec_rate_n_value
recom_rate1
## add this to df
recom_rate1 <- recom_rate1 %>% 
  mutate(rec_rate_n = as.double(Rec_rate_n_value))
  #mutate(rec_rate_n = Rec_rate_n[1])
names(recom_rate1)
str(recom_rate1)

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

str(recom_rate1)

#################################################################################################
######### !!!!! User input needed here !!!!#####################################################
#################################################################################################

#how many rates are lower_than_rec rate - this is checking how may are lower and how many higher
# what trial is it first?
unique(recom_rate1$Strip_Type)
# for P
recom_rate1 %>%  group_by(rec_rate_high_low_p, Rate, Zone_ID, zone_name) %>% 
  summarise(count= n()) %>% 
  arrange(Zone_ID, Rate )

str(recom_rate1)

#for n
# recom_rate1 %>%  group_by(rec_rate_high_low_n, Rate, Zone_ID) %>% 
#   summarise(count= n())%>% 
#arrange(Zone_ID, Rate )

## all good - if it wasnt I would need to adjust something??


#################################################################################
### list what rates per zone I want to keep

## filter out one rate so we only have rec rate, lower than and higher than
# try and aim for sensible rates not zero if it can be avoided
# 1/2 the rec rate and *2 rec rate
recom_rate1 %>%  group_by(rec_rate_high_low_p, Rate, Zone_ID, zone_name) %>% 
  summarise(count= n()) %>% 
  filter(rec_rate_high_low_p  == "rec_rate_p" ) %>% 
  mutate(double_rec_rate = Rate*2,
         half_rec_rate = Rate*.5)


zone_1_filter <- recom_rate1 %>% 
  filter(Rate %in% c(0, 50) & zone_name == "zone1") #what is in the bracket we will keep
zone_2_filter <- recom_rate1 %>% 
  filter(Rate %in% c(0, 50) & zone_name == "zone2")


recom_rate1 <- rbind(zone_1_filter, zone_2_filter)
#rm(zone_1_filter, zone_2_filter)
unique(recom_rate1$Rate)


# this is a check
recom_rate1 %>%  group_by(rec_rate_high_low_p, Rate, Zone_ID, zone_name) %>% 
  summarise(count= n()) %>% 
  arrange(Zone_ID, Rate )




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

#View(recom_rate1)
########################################################################################################################################


### for each zone and comparsion what is the mean and st error
function_grand_mean_std_error_zone <- function(df, comparison, zone){
  
  clm <- paste0("comparison_", comparison)
  comparison_grand_mean <- paste0("grand_mean_", comparison)
  comparison_se <- paste0("se_comp_", comparison)
  
  
  grand_mean_std_error <- df %>%
    dplyr::filter(zone_name == paste0("zone",zone )) %>% 
    dplyr::filter(.data[[clm[[1]]]] == comparison) %>% 
    group_by(Zone_ID ) %>% 
    summarise(!!comparison_grand_mean := mean(YldMassDry,na.rm = TRUE ),
              sd = sd(YldMassDry),
              n = n(),
              !!comparison_se := sd / sqrt(n)) %>% 
    dplyr::select(-sd, -n)
  
  grand_mean_std_error
  
}

#higher than rec rate comaprison
assign(paste0("grand_mean_std_error_zone1_", "rec_rate_high"), 
       function_grand_mean_std_error_zone(recom_rate1,"rec_rate_high_p",1))
assign(paste0("grand_mean_std_error_zone2_", "rec_rate_high"), 
       function_grand_mean_std_error_zone(recom_rate1,"rec_rate_high_p",2))

#low than rec rate comaprison
assign(paste0("grand_mean_std_error_zone1_", "rec_rate_low"), 
       function_grand_mean_std_error_zone(recom_rate1,"rec_rate_low_p",1))
assign(paste0("grand_mean_std_error_zone2_", "rec_rate_low"), 
       function_grand_mean_std_error_zone(recom_rate1,"rec_rate_low_p",2))

#if I have both high and low then I can join them togther
#But first check what I want to add

# this is a check what comaprison I have
recom_rate1 %>%  group_by(rec_rate_high_low_p, Rate, Zone_ID, zone_name) %>% 
  summarise(count= n()) %>% 
  arrange(Zone_ID, Rate) %>% 
  group_by(zone_name) %>% 
  distinct(rec_rate_high_low_p)%>% 
  filter(rec_rate_high_low_p != "rec_rate_p") %>% 
  arrange(rec_rate_high_low_p)

# ## higher than P rate comparision
  grand_mean_recom_rate_H_se <- rbind(
    grand_mean_std_error_zone1_rec_rate_high,
    grand_mean_std_error_zone2_rec_rate_high
  )
#grand_mean_recom_rate_H_se <- grand_mean_std_error_zone2_rec_rate_high

## lower than P rate comparision                                         
 grand_mean_recom_rate_L_se <- rbind(
   grand_mean_std_error_zone1_rec_rate_low,
   grand_mean_std_error_zone2_rec_rate_low)

grand_mean_recom_rate_H_se #
grand_mean_recom_rate_L_se 

 grand_mean_recom_rate_H_L_se <- full_join(grand_mean_recom_rate_H_se,
                                            grand_mean_recom_rate_L_se
                                            )

#grand_mean_recom_rate_H_L_se <- grand_mean_recom_rate_H_se
#grand_mean_recom_rate_H_L_se <- grand_mean_recom_rate_L_se

grand_mean_recom_rate_H_L_se

#### !!! select what comaprision are needed
## we have no lower comparision so I need to empty these clm
# this occurs beacsue we always have the rec rate flagged as a yield value
 

grand_mean_recom_rate_H_L_se <- grand_mean_recom_rate_H_L_se %>% 
  mutate(
    #higher than recom rate comp
     grand_mean_rec_rate_high_p = grand_mean_rec_rate_high_p,
     se_comp_rec_rate_high_p = se_comp_rec_rate_high_p,
    #grand_mean_rec_rate_high_p = NA,
    #se_comp_rec_rate_high_p = NA,
    
    #lower than recom rate comp
    #grand_mean_rec_rate_low_p = NA,
    #se_comp_rec_rate_low_p = NA)
         grand_mean_rec_rate_low_p = grand_mean_rec_rate_low_p,
         se_comp_rec_rate_low_p = se_comp_rec_rate_low_p)

grand_mean_recom_rate_H_L_se
#View(grand_mean_recom_rate_H_L_se)
             

## I need to generate mean yield value for the zone and Rate

## for P If the trial is N this needs to be changed
rec_rate_p_vs_low_High <- recom_rate1 %>% 
  group_by( Zone_ID, zone_name, rec_rate_high_low_p) %>% 
  summarise(zone_yld = mean(YldMassDry, na.rm = TRUE))

rec_rate_p_vs_low_High <- ungroup(rec_rate_p_vs_low_High)
rec_rate_p_vs_low_High
rec_rate_p_vs_low_High_wide <- tidyr::pivot_wider(rec_rate_p_vs_low_High, 
                                               id_cols = c( Zone_ID),
                                               names_from =rec_rate_high_low_p,
                                               values_from = zone_yld
)

rec_rate_p_vs_low_High_wide
names(rec_rate_p_vs_low_High_wide)

#### !!! select what comaprision are needed
## differences in yld clms
rec_rate_p_vs_low_High_wide <- rec_rate_p_vs_low_High_wide %>% 
  mutate(
         #rec_rate_p_vs_lower = rec_rate_p - lower_than_rec_rate_p,
         rec_rate_p_vs_lower = NA,
         rec_rate_p_vs_higher = rec_rate_p  - higher_than_rec_rate_p
         #rec_rate_p_vs_higher = NA
         )
rec_rate_p_vs_low_High_wide

rec_rate_p_vs_low_High_wide
grand_mean_recom_rate_H_L_se


rec_rate_p_vs_low_High_wide <- left_join(rec_rate_p_vs_low_High_wide, 
                                         grand_mean_recom_rate_H_L_se)


str(rec_rate_p_vs_low_High_wide)
#View(rec_rate_p_vs_low_High_wide)
#####

rec_rate_p_vs_low_High_summary <- rec_rate_p_vs_low_High_wide %>%
  mutate(
     yld_resposne_rec_v_low =  case_when(
       rec_rate_p_vs_lower > 0 + se_comp_rec_rate_low_p ~ "positive",
       rec_rate_p_vs_lower < 0 - se_comp_rec_rate_low_p ~ "negative",
       TRUE ~ "no_response"
     ),
    yld_resposne_rec_v_high =  case_when(
      rec_rate_p_vs_higher  > 0 +  se_comp_rec_rate_high_p ~ "negative",
      rec_rate_p_vs_higher  < 0 -  se_comp_rec_rate_high_p ~ "positive",
      TRUE ~ "no_response"
    )
  )

str(rec_rate_p_vs_low_High_summary)
names(rec_rate_p_vs_low_High_summary)
#### !!! select what comaprision are needed if we are missing clm add at the end
### 

rec_rate_p_vs_low_High_summary <- rec_rate_p_vs_low_High_summary %>%
  tidyr::pivot_longer(
    cols = c("yld_resposne_rec_v_high", 
             "yld_resposne_rec_v_low"),
    names_to = "comparison",
    values_to = "yld_response"
  ) %>%
  dplyr::select(
    Zone_ID,
    comparison,
    yld_response,
    higher_than_rec_rate_p ,
    #lower_than_rec_rate_p,
    rec_rate_p,
    #rec_rate_p_vs_lower,
    rec_rate_p_vs_higher,
    se_comp_rec_rate_low_p ,
    se_comp_rec_rate_high_p 
  ) %>% 
  mutate(
    comparison = case_when(
      comparison == "yld_resposne_rec_v_low"  ~ "rec_p_v_lower",
      comparison == "yld_resposne_rec_v_high" ~ "rec_p_v_higher"
    )) 
 rec_rate_p_vs_low_High_summary <- rec_rate_p_vs_low_High_summary %>% 
    mutate(lower_than_rec_rate_p = NA,
           rec_rate_p_vs_lower = NA)

rec_rate_p_vs_low_High_summary
#View(rec_rate_p_vs_low_High_summary)


### Extra t test #######################################################################################################################

#Prep the data making a sub selection of df for each zone and run the paired t test

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

#function(recom_rate1, zone_x, comp)
  
assign(paste0("rec_rate_p_vs_lower_", "zone_", "1"),function_paired_ttest_rec_rate_low_high(recom_rate1, 1, "lower"))
assign(paste0("rec_rate_p_vs_lower_","zone_", "2"),function_paired_ttest_rec_rate_low_high(recom_rate1, 2, "lower"))

assign(paste0("rec_rate_p_vs_higher_", "zone_", "1"),function_paired_ttest_rec_rate_low_high(recom_rate1, 1, "higher"))
assign(paste0("rec_rate_p_vs_higher_","zone_", "2"),function_paired_ttest_rec_rate_low_high(recom_rate1, 2, "higher"))


#what ran?
rec_rate_p_vs_lower_zone_1 #
rec_rate_p_vs_lower_zone_2 #

rec_rate_p_vs_higher_zone_1 # 
rec_rate_p_vs_higher_zone_2 

# this is a check what comaprison I have what was I expecting to run?
recom_rate1 %>%  group_by(rec_rate_high_low_p, Rate, Zone_ID, zone_name) %>% 
  summarise(count= n()) %>% 
  arrange(Zone_ID, Rate) %>% 
  group_by(zone_name) %>% 
  distinct(rec_rate_high_low_p)%>% 
  filter(rec_rate_high_low_p != "rec_rate_p") %>% 
  arrange(rec_rate_high_low_p)

### !!! user input required
rec_rate_p_low_vs_high_all <- rbind(#rec_rate_p_vs_lower_zone_1,
                                    #rec_rate_p_vs_lower_zone_2,

                                    rec_rate_p_vs_higher_zone_1,
                                    rec_rate_p_vs_higher_zone_2)

#rec_rate_p_low_vs_high_all <- rec_rate_p_vs_lower_zone_1



rec_rate_p_low_vs_high_all
## turn rec rate_vs_low_High_rate_summary to narrow format

## need t0 add in the zone name 
zoneID_zone_names <- recom_rate1 %>%  distinct(Zone_ID, .keep_all = TRUE) %>% 
  dplyr::select(Zone_ID,zone_name )
zoneID_zone_names

rec_rate_p_low_vs_high_all <- left_join(rec_rate_p_low_vs_high_all,zoneID_zone_names,
                                        by = c("zone" = "zone_name"))




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
    lower_than_rec_rate_p,
    rec_rate_p,
    rec_rate_p_vs_higher,
    rec_rate_p_vs_lower  ,
    se_comp_rec_rate_high_p,
    se_comp_rec_rate_low_p,
    Significant,
    P_value,
    rounded
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
str(rec_rate_p_vs_low_High_summary)


## not all comparison are valid - I need to drop some
rec_rate_p_vs_low_High_summary <- rec_rate_p_vs_low_High_summary %>% 
  filter(!is.na(zone))

#what is the recommed rate?
names(recom_rate1)


label_rec_rates <- recom_rate1 %>%  group_by(rec_rate_high_low_p, 
                                             
                                             Rate, Strip_Rate, Zone_ID) %>% 
  summarise(count= n())


label_rec_rates
label_rec_rates <- ungroup(label_rec_rates) %>% 
  dplyr::select( rec_rate_high_low_p, Strip_Rate, Zone_ID)
label_rec_rates

label_rec_rates <- tidyr::pivot_wider(
  label_rec_rates,
  names_from = rec_rate_high_low_p,
  values_from = Strip_Rate
)
label_rec_rates <- data.frame(label_rec_rates)
names(label_rec_rates)

## !! make sure this runs
label_rec_rates <-label_rec_rates %>% rename(
                           higher_than_rec_rate_p_label = higher_than_rec_rate_p,
                           #lower_than_rec_rate_p_label = lower_than_rec_rate_p,
                           rec_rate_p_label = rec_rate_p)


str(label_rec_rates)
str(rec_rate_p_vs_low_High_summary)


rec_rate_p_vs_low_High_summary <- full_join(rec_rate_p_vs_low_High_summary, label_rec_rates, by = "Zone_ID")

names(rec_rate_p_vs_low_High_summary)
#remove duplication 
rec_rate_p_vs_low_High_summary <- dplyr::distinct(rec_rate_p_vs_low_High_summary, 
                                                  Zone_ID, comparison, .keep_all = TRUE)


### check that what I am outputting is sensible Yld repsonse should reflect the comparsion made.


rec_rate_p_vs_low_High_summary <- rec_rate_p_vs_low_High_summary %>% 
  mutate(
    yld_response = case_when(
      comparison == "rec_p_v_higher" &
        higher_than_rec_rate_p != "NA" ~ yld_response,
      comparison == "rec_p_v_lower" &
        lower_than_rec_rate_p != "NA" ~ yld_response,
      TRUE ~ "NA"
    ))

View(rec_rate_p_vs_low_High_summary)

#save the output
name_rec_rate_low_high <- paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/rec_rate_comparision/rec_rate_comp_", 
                                 dplyr::distinct(all_results_1,paddock_ID_Type), ".csv")
name_rec_rate_low_high
write.csv(rec_rate_p_vs_low_High_summary, name_rec_rate_low_high)  

