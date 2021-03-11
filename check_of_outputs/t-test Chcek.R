### check on t test for jampot east zones 2021

zone_x <-  1
rate_x <- 1
#function_paired_ttest <- function(strips, zone_x, rate_x){
  
  #select the zone data and the rates
  zone_x_rateXvsGR <- filter(strips,
                             zone_name == paste0("zone", zone_x)) %>%
    filter(rate_name == paste0("rate", rate_x) | rate_name == "Grower_rate")
  #View(zone_x_rateXvsGR)
  
  unique(zone_x_rateXvsGR$zone_name)
  unique(zone_x_rateXvsGR$rate_name)
  
  zone_x_rateXvsGR %>%  group_by(rate_name) %>% 
    summarise(count= n(),
              yld_check = mean(YldMassDry, na.rm = TRUE))
  
 
  
  #average the yld per segment and rate
  zone_x_rateXvsGR_av <- group_by(zone_x_rateXvsGR, SegmentID, Rate, Zone, rate_name, zone_name ) %>% 
    summarise_all(mean, na.rm= TRUE)
  zone_x_rateXvsGR_av
  
  #ensure that the dataset is duplictaed
  list_SegmentID_values <- zone_x_rateXvsGR_av$SegmentID[duplicated(zone_x_rateXvsGR_av$SegmentID)] #this returns a list of values I want to keep
  zone_x_rateXvsGR_av <- zone_x_rateXvsGR_av %>% filter(SegmentID %in% list_SegmentID_values)
  
  #write.csv(zone_x_rateXvsGR_av, "W:/value_soil_testing_prj/Yield_data/2020/processing/r_scripts/check_of_outputs/jampot_east_input_t_test.csv")
  
  
  zone_x_rateXvsGR_av
  # run paired ttest
  zone_x_rateXvsGR_res <- t.test(YldMassDry ~ Rate, data = zone_x_rateXvsGR_av, 
                                 paired = TRUE)
  zone_x_rateXvsGR_res
  
  
  ###############################################
  DF_1_rate50_zone2 <- zone_x_rateXvsGR_av %>% 
    filter(Rate == 50)
  DF_2_rate100_zone2 <- zone_x_rateXvsGR_av %>% 
    dplyr::filter(Rate == 50)
  #DF_1_rate50_zone2 <- ungroup(DF_1_rate50_zone2)
  #DF_2_rate100_zone2 <- ungroup(DF_2_rate100_zone2)
  dim(zone_x_rateXvsGR_av) #22 points
  dim(DF_1_rate50_zone2)   #10 points
  dim(DF_2_rate100_zone2)  #10 points
  
  
  
  ### Different options ...
 t_test_paired <- t.test(YldMassDry ~ Rate, 
                                        data = zone_x_rateXvsGR_av,
                                        paired = TRUE,
                                        var.equal = FALSE)
 t_test_paired # p-value = 0.02802 var.equal can be set to TRUE or FALSE and produces same results
  
 t_test_Not_paired_var_equal_T <- t.test(YldMassDry ~ Rate, 
                                            data = zone_x_rateXvsGR_av,
                                        paired = FALSE,
                                        var.equal = TRUE)
 t_test_Not_paired_var_equal_T # p-value = 0.4002
  
 t_test_Not_paired_var_equal_F <- t.test(YldMassDry ~ Rate, 
                                           data = zone_x_rateXvsGR_av,
                                           paired = FALSE,
                                           var.equal = FALSE)
 t_test_Not_paired_var_equal_F #p-value = 0.4009
  
  
  comapre_t_test <-
    data.frame(t_test_paired = 
                 as.double(t_test_paired$p.value),
               
               t_test_Not_paired_var_equal_T = 
                 as.double(t_test_Not_paired_var_equal_T$p.value),
               
               t_test_Not_paired_var_equal_F = 
                 as.double(t_test_Not_paired_var_equal_F$p.value))
  
  comapre_t_test <- pivot_longer(comapre_t_test, 
                                 cols = starts_with("t_test"),
                                 names_to = "P_test",
                                 values_to = "P_value",)
  
  comapre_t_test <- mutate(comapre_t_test,
                           Significant = case_when(
                             P_value < 0.05 ~ "significant",
                            TRUE ~ "not significant"))
  comapre_t_test                     
               
     
  
  
