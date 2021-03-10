### check on t test for jampot east zones

zone_x <-  2
rate_x <- 2
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
  
  write.csv(zone_x_rateXvsGR_av, "W:/value_soil_testing_prj/Yield_data/2020/processing/r_scripts/check_of_outputs/jampot_east_input_t_test.csv")
  
  
  zone_x_rateXvsGR_av
  # run paired ttest
  zone_x_rateXvsGR_res <- t.test(YldMassDry ~ Rate, data = zone_x_rateXvsGR_av, paired = TRUE)
 
  DF_1_rate50_zone2 <- zone_x_rateXvsGR_av %>% 
    filter(Rate == 50)
  DF_2_rate100_zone2 <- zone_x_rateXvsGR_av %>% 
    dplyr::filter(Rate == 50)
  DF_1_rate50_zone2 <- ungroup(DF_1_rate50_zone2)
  DF_2_rate100_zone2 <- ungroup(DF_2_rate100_zone2)
  dim(DF_1_rate50_zone2)
  dim(DF_2_rate100_zone2)
  
  
  zone_x_rateXvsGR_res_alt <- t.test(DF_1_rate50_zone2, DF_2_rate100_zone2, var.equal = TRUE, paired = TRUE)
  t.test(DF_1_rate50_zone2, DF_2_rate100_zone2 )
  
  
  zone_x_rateXvsGR_res
  
  
  #####test results
  # Report values from the t.test
  zone_x_rateXvsGR_res_sig <-
    data.frame(P_value = as.double(zone_x_rateXvsGR_res$p.value),
               Mean_diff = (zone_x_rateXvsGR_res$estimate)) %>%
    mutate(
      rate_name = paste0("rate", rate_x),
      zone = paste0("zone", zone_x),
      rounded = abs(round(Mean_diff, 2)),
      Significant = case_when(P_value < 0.05 ~ "significant",
                              TRUE ~ "not significant"))
  zone_x_rateXvsGR_res_sig 
  
  
  
  return(data.frame(zone_x_rateXvsGR_res_sig))
}
assign(paste0("zone_", "1", "rate_", "1"), function_paired_ttest(strips, 1, 1))
assign(paste0("zone_", "1", "rate_", "2"), function_paired_ttest(strips, 1, 2))
assign(paste0("zone_", "2", "rate_", "1"), function_paired_ttest(strips, 2, 1))
assign(paste0("zone_", "2", "rate_", "2"), function_paired_ttest(strips, 2, 2))
