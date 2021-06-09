str(for_plotting)

# rm(list = ls()[!ls() %in% c("for_plotting")])


ws_plotting <- ungroup(for_plotting)
ws_plotting$rate_as_factor <- as.factor(ws_plotting$Rate)  
str(ws_plotting)

ws_plotting <- ws_plotting %>% 
  dplyr::select(zone_name,
         YldMassDry,
         rate_as_factor,
         SegmentID)


min <- min(ws_plotting$SegmentID)
max <- max(ws_plotting$SegmentID)

number_rates <- 4

SegmentID <- seq(min,max, by = 10) #steps of 10 (all should be like this)
SegmentID <- rep(SegmentID, times = number_rates) #4 rates
number_seg <- 492 #just look this up


Rates <-rep(c(0 ,25,50,75), time = (number_seg/number_rates)) # number of segments / number of rates
Rates_df <- data.frame(Rates)

SegmentID_df <- data.frame(SegmentID)
SegmentID_df <- SegmentID_df %>%  arrange(SegmentID)

dummy_segments_rates <- data.frame(SegmentID_df, Rates_df)

# create a field to join for both df
dummy_segments_rates <- dummy_segments_rates %>% 
  mutate(segment_ID_Rate = paste0(SegmentID, "_", Rates))
ws_plotting <- ws_plotting %>% 
  mutate(segment_ID_Rate = paste0(SegmentID, "_", rate_as_factor))

ws_plotting_with_na <- full_join(dummy_segments_rates, ws_plotting) %>% 
  dplyr::select(-rate_as_factor)




### almost the same as the function but using both df 1.ws_plotting_with_na 2.for_plotting (for names)

ws_plotting_with_na$rate_as_factor <- as.factor(ws_plotting_with_na$Rate) 

## change the names of Rate

ws_plotting_with_na <- ws_plotting_with_na %>% 
  mutate(rate_as_factor = case_when(
    rate_as_factor == "0" ~ "0 / strip av 2.41",
    rate_as_factor == "25" ~ "25 / strip av 2.40",
    rate_as_factor == "50" ~ "50 / strip av 2.54",
    rate_as_factor == "75" ~ "75 / strip av 2.62"
  ))

str(ws_plotting_with_na)

zone1_min <- filter(ws_plotting_with_na, zone_name == "zone1") %>% summarise(min_zone = min(SegmentID))
zone1_min <- zone1_min[[1]]
zone1_min <- zone1_min[[1]]
zone1_max <- filter(ws_plotting_with_na, zone_name == "zone1") %>% summarise(max_zone = max(SegmentID))
zone1_max <- zone1_max[[1]]  
zone1_max <- zone1_max[[1]]
zone2_min <- filter(ws_plotting_with_na, zone_name == "zone2") %>% summarise(min_zone = min(SegmentID))
zone2_min <- zone2_min[[1]]
zone2_min <- zone2_min[[1]]
zone2_max <- filter(ws_plotting_with_na, zone_name == "zone2") %>% summarise(max_zone = max(SegmentID))
zone2_max <- zone2_max[[1]] 
zone2_max <- zone2_max[[1]] 
zone3_min <- filter(ws_plotting_with_na, zone_name == "zone3") %>% summarise(min_zone = min(SegmentID))
zone3_min <- zone3_min[[1]]
zone3_min <- zone3_min[[1]]
zone3_max <- filter(ws_plotting_with_na, zone_name == "zone3") %>% summarise(max_zone = max(SegmentID))
zone3_max <- zone3_max[[1]]  
zone3_max <- zone3_max[[1]]


zone1_range <- ((zone1_max - zone1_min)/2)+zone1_min
zone2_range <- ((zone2_max - zone2_min)/2)+zone2_min
zone3_range <- ((zone3_max - zone3_min)/2)+zone3_min

zone1_range
zone2_range
zone3_range

max_yld <- max(ws_plotting_with_na$YldMassDry, na.rm = TRUE)
min_yld <- min(ws_plotting_with_na$YldMassDry, na.rm = TRUE)

label_paddock<-   unique(for_plotting$name_Paddock)
label_paddock <- stringr::str_split(label_paddock, "_", simplify = TRUE)
label_paddock <- label_paddock[1,1]

label_zone1 <- ungroup(for_plotting) %>% 
  dplyr::select(Zone, zone_name) %>%
  filter( zone_name == "zone1") %>% 
  distinct(Zone)

label_zone2 <- ungroup(for_plotting) %>% 
  dplyr::select(Zone, zone_name) %>%
  filter( zone_name == "zone2") %>% 
  distinct(Zone)

label_zone3 <- ungroup(for_plotting) %>% 
  dplyr::select(Zone, zone_name) %>%
  filter( zone_name == "zone3") %>% 
  distinct(Zone)

whole_strip <- ggplot(ws_plotting_with_na, aes(SegmentID , YldMassDry, group = rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue', 'red'), name  = "Fertiliser Rates")+
  theme_bw()+
  ylim(0,max_yld)+ #this needs to be a max and min of all the 
  labs(x= "Distance along the strip (meters)",
       y = "Yield t/ha",
       title = "",
       subtitle = paste0(label_paddock),
       caption = paste(Grower_rate_label, 
                       Starter_label, 
                       Topdress_label, sep="\n"))+
  theme(plot.caption = element_text(hjust = 0))+
  
  annotate("rect", xmin = zone1_min, xmax = zone1_max, ymin = 0, ymax = max_yld, #Zone 1
           alpha = .2)  +
  annotate("text", x = zone1_range, y= 0,label = label_zone1)+
  
  
  annotate("rect", xmin =zone2_min , xmax = zone2_max, ymin = 0, ymax = max_yld, #zone 2
           alpha = .2)+
  annotate("text", x = zone2_range, y= 0,label =label_zone2)+
  
  annotate("rect", xmin =zone3_min , xmax = zone3_max, ymin = 0, ymax = max_yld, #zone 3
           alpha = .2)+
  annotate("text", x = zone3_range, y= 0,label =label_zone3)#+

whole_strip



#### Therese also wants an average for the whole strip for each rate? double check this?
names(ws_plotting_with_na)

av_fert_rate_strip <- ws_plotting_with_na %>% 
group_by(rate_as_factor) %>% 
  summarise(YldMassDry_average = mean(YldMassDry, na.rm = TRUE))

av_fert_rate_strip

# YldMassDry_average_0  <-  av_fert_rate_strip[1,2] 
# YldMassDry_average_25 <-  av_fert_rate_strip[2,2] 
# YldMassDry_average_50 <-  av_fert_rate_strip[3,2] 
# YldMassDry_average_75 <-  av_fert_rate_strip[4,2] 
# 
#  YldMassDry_average_0
# # YldMassDry_average_25
# # YldMassDry_average_50
# # YldMassDry_average_75

av_temp <- ws_plotting_with_na %>% 
  mutate(ave_yld_strip = case_when(
    rate_as_factor == "0" ~ 2.41,
    rate_as_factor == "25" ~ 2.40,
    rate_as_factor == "50" ~ 2.54,
    rate_as_factor == "75" ~ 2.62
  ))
names(av_temp)
av_temp <- av_temp %>% 
  dplyr::select(SegmentID, rate_as_factor, ave_yld_strip, zone_name)

#change clm names and contents

av_temp <- av_temp %>% 
  mutate(rate_as_factor1 = paste0(rate_as_factor,"_", "av"))

av_temp <- av_temp %>%
  dplyr::select(rate_as_factor1,
               SegmentID,
               ave_yld_strip,
               zone_name)

av_temp <- av_temp %>%
  rename(rate_as_factor = rate_as_factor1,
         YldMassDry = ave_yld_strip)

## append this to the ws_plotting_with_na

names(av_temp)
ws_plotting_with_na <- ws_plotting_with_na %>% 
  dplyr::select( rate_as_factor,
          SegmentID ,
          YldMassDry ,
          zone_name)

names(ws_plotting_with_na)


test <- rbind(ws_plotting_with_na, av_temp)


whole_strip_av <- ggplot(test, aes(SegmentID , YldMassDry, group = rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue', 'red', 'grey', 'springgreen','skyblue', 'rosybrown' ), name  = "Fertiliser Rates")+
  theme_bw()+
  ylim(0,max_yld)+ #this needs to be a max and min of all the 
  labs(x= "Distance along the strip (meters)",
       y = "Yield t/ha",
       title = "",
       subtitle = paste0(label_paddock),
       caption = paste(Grower_rate_label, 
                       Starter_label, 
                       Topdress_label, sep="\n"))+
  theme(plot.caption = element_text(hjust = 0))+
  
  annotate("rect", xmin = zone1_min, xmax = zone1_max, ymin = 0, ymax = max_yld, #Zone 1
           alpha = .2)  +
  annotate("text", x = zone1_range, y= 0,label = label_zone1)+
  
  
  annotate("rect", xmin =zone2_min , xmax = zone2_max, ymin = 0, ymax = max_yld, #zone 2
           alpha = .2)+
  annotate("text", x = zone2_range, y= 0,label =label_zone2)+
  
  annotate("rect", xmin =zone3_min , xmax = zone3_max, ymin = 0, ymax = max_yld, #zone 3
           alpha = .2)+
  annotate("text", x = zone3_range, y= 0,label =label_zone3)#+

whole_strip_av
  