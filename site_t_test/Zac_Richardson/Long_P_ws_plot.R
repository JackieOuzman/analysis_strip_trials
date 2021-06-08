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





#### Therese also wants an average for the whole strip for each rate? double check this?
names(ws_plotting)

av_fert_rate_strip <- ws_plotting %>% 
group_by(rate_as_factor) %>% 
  summarise(YldMassDry_average = mean(YldMassDry, na.rm = TRUE))

av_fert_rate_strip
unique(ws_plotting$rate_as_factor)

av_temp <- ws_plotting %>% 
  mutate(ave_yld_strip = case_when(
    rate_as_factor == "0" ~ 2.47,
    rate_as_factor == "60" ~ 2.31,
    rate_as_factor == "120" ~ 2.31
    
  ))
names(av_temp)
av_temp <- av_temp %>% 
  dplyr::select(SegmentID, rate_as_factor, ave_yld_strip, zone_name)

#change clm names and contents

av_temp <- av_temp %>% 
  mutate(rate_as_factor1 = paste0( "average ",rate_as_factor))

av_temp <- av_temp %>%
  dplyr::select(rate_as_factor1,
               SegmentID,
               ave_yld_strip,
               zone_name)

av_temp <- av_temp %>%
  rename(rate_as_factor = rate_as_factor1,
         YldMassDry = ave_yld_strip)

## append this to the ws_plotting

names(av_temp)
ws_plotting <- ws_plotting %>% 
  dplyr::select( rate_as_factor,
          SegmentID ,
          YldMassDry ,
          zone_name)

names(ws_plotting)


ws_plotting_av <- rbind(ws_plotting, av_temp)


#ws_plotting_av$rate_as_factor <- as.factor(ws_plotting_av$Rate)  

zone1_min <- filter(ws_plotting_av, zone_name == "zone1") %>% summarise(min_zone = min(SegmentID))
zone1_min <- zone1_min[[1]]
zone1_min <- zone1_min[[1]]
zone1_max <- filter(ws_plotting_av, zone_name == "zone1") %>% summarise(max_zone = max(SegmentID))
zone1_max <- zone1_max[[1]]  
zone1_max <- zone1_max[[1]]
zone2_min <- filter(ws_plotting_av, zone_name == "zone2") %>% summarise(min_zone = min(SegmentID))
zone2_min <- zone2_min[[1]]
zone2_min <- zone2_min[[1]]
zone2_max <- filter(ws_plotting_av, zone_name == "zone2") %>% summarise(max_zone = max(SegmentID))
zone2_max <- zone2_max[[1]] 
zone2_max <- zone2_max[[1]] 

zone1_range <- ((zone1_max - zone1_min)/2)+zone1_min
zone2_range <- ((zone2_max - zone2_min)/2)+zone2_min
zone1_range
zone2_range

max_yld <- max(ws_plotting_av$YldMassDry, na.rm = TRUE)
min_yld <- min(ws_plotting_av$YldMassDry, na.rm = TRUE)

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

str(ws_plotting_av)

ggplot(ws_plotting_av, aes(SegmentID , YldMassDry, group = rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue',  'grey','darkgreen', 'navy' ), name  = "Fertiliser Rates")+
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
  annotate("text", x = zone2_range, y= 0,label =label_zone2)#+


ggsave("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/ws_plots_etc/prime_faily_trust_longP.png", 
       width = 60, height = 20, units = "cm")
