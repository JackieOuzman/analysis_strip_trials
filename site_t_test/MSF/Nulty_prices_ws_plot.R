str(for_plotting)

ws_plotting <- ungroup(for_plotting)
ws_plotting$rate_as_factor <- as.factor(ws_plotting$Rate)  
str(ws_plotting)

ws_plotting <- ws_plotting %>% 
  dplyr::select(zone_name,
         YldMassDry,
         rate_as_factor,
         SegmentID)

#Perhaps add a cotniuous Segement ID add NA when there is no yld data
#geom_path(na.rm = FALSE)
min <- min(ws_plotting$SegmentID)
max <- max(ws_plotting$SegmentID)

min
max
range <- max -min
SegmentID <- seq(min,max, by = 10) #steps of 10 (all should be like this)
dim(SegmentID)
SegmentID <- rep(SegmentID, times = 4) #4 rates


unique(ws_plotting$rate_as_factor)
rates <- rep(c(0 ,25,50,75), times = range/4) #4 rates




SegmentID_rates <- data.frame(SegmentID, rates)

dim(SegmentID_rates)/4

ws_plotting <- full_join(SegmentID, ws_plotting)
str(ws_plotting)


ggplot(ws_plotting, aes(SegmentID , YldMassDry, group = rate_as_factor))+ 
  geom_line()+
        theme_bw()
 
       

ggplot(ws_plotting, aes(SegmentID , YldMassDry, group = rate_as_factor))+
  #geom_line(size=1, alpha=0.4, na.rm = FALSE, aes( color = rate_as_factor ))+
  geom_line(size=1, alpha=0.4, na.rm = FALSE)+
  #scale_color_manual(values=c('darkgrey','green', 'blue', 'red'), name  = "Fertiliser Rates")+
  theme_bw()


ggplot(for_plotting, aes(SegmentID , YldMassDry, group = rate_as_factor))+
  geom_line(size=1, alpha=0.4, , na.rm = FALSE, aes( color = rate_as_factor ))+