


str(strips)
GR_vs_low_High_rate <- strips %>% 
  filter(!is.na(zone_name)) %>% 
  dplyr::select(Zone_ID, SegmentID, YldMassDry, Rate, rate_name_order, rate_name)
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
