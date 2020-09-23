library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
###########################################################################################
#std

#group the reuslts into P and N and then into number of strips
#tidy this code up so I can have functions for analysis

baseDir <- file.path("C:","Users", "ouz001", "working_from_home","soil_testing",  "dig_deeper", "whole_strip", "P", "3Rates")
outputDir <- file.path("C:","Users", "ouz001", "working_from_home","soil_testing",  "dig_deeper", "whole_strip", "P", "3Rates", "output")
baseDir
list.files(baseDir, full.names = FALSE)


#input_file <- "Backsheehans_Yld_SegID_Zones.csv" # perhaps use this as a loop from the file.path
#input_file <- "BallsCentre2_Yld_SegID_Zone.csv" 
#input_file <- "BallsSilo1_Yld_SegID_Zones.csv"
#input_file <-"Bella_Yld_SegID_Zone.csv"
#input_file <-"Bevs_Yld_SegID_zone.csv"
#input_file <-"Brennans_Yld_SegID_Zones.csv"
#input_file <-"Brookland2_Yld_SegID_zone.csv"

#input_file <-"Cail_Yld_SegID_zone.csv"         
#input_file <-"Clover_Yld_SegID_Zone.csv" 
#input_file <-"Front1_Yld_SegID_Zone.csv"  
#input_file <-"Front3_Yld_SegID_Zone.csv" 
#input_file <-"Heads_Yld_SegID_Zone.csv"
#input_file <-"Hennessy_Yld_SegID_Zone.csv"  
#input_file <-"Hickmonts_Yld_SegID_zone.csv"    
#input_file <-"Home06_Yld_SegID_Zone.csv" 
#input_file <-"Jardines_Yld_SedID_zones.csv" 
#input_file <-"Jash5_Yld_SegID_Zone.csv"        
#input_file <-"McKenzie_Yld_SegID_Zone.csv" 
#input_file <-"Mervyns_Yld_SegID.csv"       # No zone can't run the function

input_file <-"Rail_Yld_SegID_Zone.csv"
#input_file <-"RoundHome_Yld_SegID_Zone.csv"
#input_file <-"School_Yld_SegID_Zone.csv"       
#input_file <-"Stewarts3_Yld_SegID_Zone.csv"
#input_file <-"Top_Yld_SegID_Zone.csv"          

################################################################################################################
function_1_import_data <- function(input_file){
strips <- read_csv(paste0(baseDir, "/",input_file))
return(strips)}

#call function
function_1_import_data(input_file)
################################################################################################################

#tidy up data frame
#make clms standard and remove the NA vlues in rate clm - having trouble with this as function because of the errors if the clm name doesnt exist
strips <- rename(strips,Rate = Rates )
strips <- rename(strips,YldMassDry = Yld_Mass_D )
strips <- rename(strips,Zone = zone )
strips <- rename(strips,Zone = zones )
strips <- filter(strips, !is.na(Rate))
strips$Rate <- as.double(strips$Rate )
### clean the data removing zero values
strips <- filter(strips,
                 DistOnLine != 0)
strips <- filter(strips,
                 YldMassDry != 0)
### Add correction to segmentID so we can read it as meters (this will depend on how it was defined in spatial)
strips <-  mutate(strips,
                  SegmentID = SegmentID *10)


##################################################################################################################
## details of what was applied and the GR
##################################################################################################################


#need to assign a extra clms with the GR and details of product to this df rate name, details, starter fertiliser and topdress
#also can't make this a function because I want to return multiple R objcets

Rates_labels <- data.frame(Rate = unique(strips$Rate)) %>% 
  arrange(Rate) %>% 
  mutate(rate_name = c("rate1","Grower_rate" , "rate2"))

strips <- left_join(strips, Rates_labels, by= "Rate")

x <- Rates_labels[1,1]
y <- Rates_labels[2,1]
z <- Rates_labels[3,1]


####################################################################################################################
##################################################################################################################
## details of zones
##################################################################################################################
Zone_labels <- 
  data.frame(Zone = unique(strips$Zone)) %>% 
  filter(!is.na(Zone)) %>% 
  arrange(Zone) %>% #this should work with text
  mutate(zone_name = c("zone1","zone2" ))

# join this to the strips data
strips <- left_join(strips, Zone_labels, by= "Zone")
unique(strips$rate_name)
names(strips)

###############################################################################################################
#Prep the data making a sub selection of df for each zone and run the paired t test

function_paired_ttest <- function(strips, zone_x, rate_x){

#select the zone data and the rates
zone_x_rateXvsGR <- filter(strips,
                          zone_name == paste0("zone", zone_x)) %>%
                    filter(rate_name == paste0("rate", rate_x) | rate_name == "Grower_rate")

#average the yld per segment and rate
zone_x_rateXvsGR_av <- group_by(zone_x_rateXvsGR, SegmentID, Rate, Zone, rate_name, zone_name ) %>% 
  summarise_all(mean)
#ensure that the dataset is duplictaed
list_SegmentID_values <- zone_x_rateXvsGR_av$SegmentID[duplicated(zone_x_rateXvsGR_av$SegmentID)] #this returns a list of values I want to keep
zone_x_rateXvsGR_av <- zone_x_rateXvsGR_av %>% filter(SegmentID %in% list_SegmentID_values)
# run paired ttest
zone_x_rateXvsGR_res <- t.test(YldMassDry ~ Rate, data = zone_x_rateXvsGR_av, paired = TRUE)

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

################################################################################################################
## function to join everything  togther
#join these togther
function_all_results <- function(zone_1rate_1,
                                 zone_1rate_2,
                                 zone_2rate_1,
                                 zone_2rate_2){
results_ttest <- bind_rows(zone_1rate_1,
                           zone_1rate_2,
                           zone_2rate_1,
                           zone_2rate_2)

#what is the mean yield value for the zone by strip
mean_zone1 <-  filter(strips,
                          zone_name == "zone1") %>%
                          group_by(Rate) %>%
                          summarise(yield = mean(YldMassDry)) %>% 
                          mutate(zone = "zone1" )
mean_zone2 <-  filter(strips,
                      zone_name == "zone2") %>%
                      group_by(Rate) %>%
                      summarise(yield = mean(YldMassDry)) %>% 
                      mutate(zone = "zone2" )

mean_zone <- bind_rows(mean_zone1, mean_zone2)
mean_zone <- left_join(mean_zone,Rates_labels)

results_ttest <- left_join(mean_zone, results_ttest)
return(results_ttest)}

assign(("all_results"), function_all_results(zone_1rate_1,
                                            zone_1rate_2,
                                            zone_2rate_1,
                                            zone_2rate_2))

##################################################################################################################
### Plotting the results
## step 1 complie the results avearge of segment per zone
for_plotting <- filter(strips, !is.na(zone_name)) %>% 
        group_by(Rate, Zone, rate_name, zone_name, SegmentID, ) %>% 
        summarise_all(mean)

function_zone_plots <- function(for_plotting, zone_x){

for_plotting$rate_as_factor <- as.factor(for_plotting$Rate)
zone_plot <- filter(for_plotting, zone_name == paste0("zone", zone_x)) %>% 
  ggplot( aes(rate_as_factor, YldMassDry))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  ylim(0,6)+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10))+
  labs(x = "Fertiliser Rates",
       y= "Yield t/ha",
       title = paste0("zone ", zone_x),
       caption = "Below table reports mean values and significant differences compared to GSR")+
  theme(plot.caption = element_text(size=8, face="italic", color="black"))+
  annotate("text", x = 2, y= 0, size = 3,label = "box plot = 25%, 50%, 75%, dashed line = mean")

return(zone_plot)
}

assign(("plot_zone1"), function_zone_plots(for_plotting, 1))
assign(("plot_zone2"), function_zone_plots(for_plotting, 2))
plot_zone1
plot_zone2


function_strip_plot <- function(for_plotting){

for_plotting$rate_as_factor <- as.factor(for_plotting$Rate)  
zone1_min <- filter(for_plotting, zone_name == "zone1") %>% summarise(min_zone = min(SegmentID))
zone1_min <- zone1_min[[5]]
zone1_min <- zone1_min[[1]]
zone1_max <- filter(for_plotting, zone_name == "zone1") %>% summarise(max_zone = max(SegmentID))
zone1_max <- zone1_max[[5]]  
zone1_max <- zone1_max[[1]]
zone2_min <- filter(for_plotting, zone_name == "zone2") %>% summarise(min_zone = min(SegmentID))
zone2_min <- zone2_min[[5]]
zone2_min <- zone2_min[[1]]
zone2_max <- filter(for_plotting, zone_name == "zone2") %>% summarise(max_zone = max(SegmentID))
zone2_max <- zone2_max[[5]] 
zone2_max <- zone2_max[[1]] 
zone1_range <- ((zone1_max - zone1_min)/2)+zone1_min
zone2_range <- ((zone2_max - zone2_min)/2)+zone2_min
zone1_range
zone2_range

whole_strip <- ggplot(for_plotting, aes(SegmentID , YldMassDry, group = rate_as_factor))+
  geom_line(size=1, alpha=0.4, aes( color = rate_as_factor ))+
  scale_color_manual(values=c('darkgrey','green', 'blue', 'red'), name  = "Fertiliser Rates")+
  theme_bw()+
  ylim(0.0,6)+
  labs(x= "Distance along the strip (meters)",
       y = "Yield t/ha",
       title = "",
       subtitle = "Paddock_to_be_nomiated",
       caption = paste("Grower_rate_label", "Additional_fert_label",  sep="\n"))+
  theme(plot.caption = element_text(hjust = 0))+
  annotate("rect", xmin = zone1_min, xmax = zone1_max, ymin = 0, ymax = 6, #Zone 1
           alpha = .2)  +
  annotate("text", x = zone1_range, y= 1,label = "zone1")+
  
  annotate("rect", xmin =zone2_min , xmax = zone2_max, ymin = 0, ymax = 6, #zone 2
           alpha = .2)+
  annotate("text", x = zone2_range, y= 1,label = "zone2")#+

return(whole_strip)}
assign(("plot_whole_strip"), function_strip_plot(for_plotting))

plot_whole_strip





############################################################################
#this is calling the function (make sure the function in an object first)
value_whole_strip(input_file, x, y, z)
############################################################################


#this could be a function...
value_whole_strip <- function(input_file, x,y,z, strips){
site <- stringr:: str_split(input_file, "_", simplify = TRUE)
site <- site[1,1]

strips <- strips %>% 
  mutate(
    Rate_name = case_when(
      Rate == x ~ "low_rate",
      Rate == y ~ "middle_rate",
      Rate == z ~ "high_rate"
    ))

## std with averaging of each seg
step1_strips <- group_by(strips, SegmentID, Rate_name) %>% 
  summarise(Yld_Mass_Av = mean(YldMassDry ))

unique(step1_strip_std$Rate_name)
#turn this into a wide data - because I am at a loss how to do this with tidy data
str(step1_strip)
strips_wide <- step1_strips %>% 
  pivot_wider(names_from = Rate_name , values_from = Yld_Mass_Av)
strips_wide <- as.data.frame(strips_wide)

strips_wide <- strips_wide %>% 
                          mutate(
                          low_vs_middle = case_when(
                            low_rate <   middle_rate ~ "yld_increase",
                            TRUE           ~ "yld_decrease"
                          ))


strips_std_wide <- strips_std_wide %>% 
  mutate(
    middle_vs_high = case_when(
      middle_rate  <  high_rate ~ "yld_increase",
      TRUE           ~ "yld_decrease"
    ))

strips_std_wide <- strips_std_wide %>% 
  mutate(
    low_vs_high = case_when(
      low_rate <  high_rate ~ "yld_increase",
      TRUE           ~ "yld_decrease"
    ))

strips_std_wide <- as.data.frame(strips_std_wide)
str(strips_std_wide)


low_vs_middle_tally <-  strips_std_wide %>% group_by(low_vs_middle) %>%   summarise(n = n()) %>%  mutate(percent = (freq = n / sum(n)))    %>%  mutate(comaprsion = "low_vs_middle") %>% rename(yld_response = low_vs_middle )
middle_vs_high_tally <- strips_std_wide %>% group_by(middle_vs_high) %>%  summarise(n = n()) %>%  mutate(percent = (freq = n / sum(n)))    %>%  mutate(comaprsion = "middle_vs_high") %>% rename(yld_response = middle_vs_high )
low_vs_high_tally <-    strips_std_wide %>% group_by( low_vs_high)  %>%   summarise(n = n()) %>%  mutate(percent = (freq = n / sum(n)))    %>%  mutate(comaprsion = "low_vs_high") %>% rename(yld_response = low_vs_high )


names(low_vs_middle_tally)
names(middle_vs_high_tally)
names(low_vs_high_tally)

strips_count_segments <- bind_rows(low_vs_middle_tally,
                                     middle_vs_high_tally,
                                     low_vs_high_tally)  
########################################################################
### how does it behave with zone data
str(strips_std)
unique(strips_std$Zone)
## std with averaging of each seg

step1_strips_zone_std <- group_by(strips_std, SegmentID, Rate, Zone) %>% 
  summarise(Yld_Mass_Av = mean(YldMassDry))

strips_zone_std_ave <- group_by(step1_strips_zone_std, Rate, Zone ) %>% 
  summarise(Yld_Mass_Av = mean(Yld_Mass_Av)) %>% 
  mutate(site= paste0(site, "_yld_std"),
         raw_or_av = 'average_all'
         )

strips_zone_std_ave <- filter(strips_zone_std_ave, !is.na(Zone))
strips_zone_std_ave <- ungroup(strips_zone_std_ave)

#########################################################################
### how does it behave with all the strip data
str(strips_std)
## std with averaging of each seg

step1_strips_all_std <- group_by(strips_std, SegmentID, Rate) %>% 
  summarise(Yld_Mass_Av = mean(YldMassDry, na.rm = TRUE))
print(step1_strips_all_std)
strips_all_std_ave <- group_by(step1_strips_all_std, Rate ) %>% 
  summarise(Yld_Mass_Av = mean(Yld_Mass_Av,na.rm = TRUE)) %>% 
  mutate(site= paste0(site, "_yld_std"),
         raw_or_av = 'average_all', 
         Zone = "entire_strip")
print(strips_all_std_ave)

#then order the df by rate and add a new clm for comparsion
strips_all_std_ave <- arrange(strips_all_std_ave, Rate) %>% 
  mutate(comaprsion = c("low_vs_high", "low_vs_middle", "middle_vs_high"))

#######################################################################
#join the entrie strip and zone togther
str(strips_zone_std_ave)
str(strips_all_std_ave)
print(strips_all_std_ave)
strips_all_std_ave <- bind_rows(strips_all_std_ave, strips_zone_std_ave)



## add in the strip count info
strips_count_segments <- filter(strips_count_segments,
                                yld_response == "yld_increase")
strip_whole <- full_join(strips_all_std_ave, strips_count_segments, by = "comaprsion")

str(strips_all_std_ave)
str(strips_count_segments)

#rm(list=setdiff(ls(), c("strips_all_std_ave", "strips_count_segments", "strip_whole", "outputDir", "site")))


strip_whole <- strip_whole %>% 
  mutate(
    Rate_name = case_when(
      Rate == x ~ "low_rate",
      Rate == y ~ "middle_rate",
      Rate == z ~ "high_rate"
    ))

write.csv(strip_whole,
          paste0(outputDir, "/",site, "_strip_whole.csv"))

return(strip_whole)
}


########################################################################################################################
## can I bring in all the processed files and make one big one?



baseDir2 <- file.path("C:","Users", "ouz001", "working_from_home","soil_testing",  "dig_deeper", "whole_strip", "P", "3Rates", "output")
baseDir2
list.files(baseDir2, full.names = FALSE)

# Get file list
file_list <- list.files(baseDir2, full.names = TRUE)

# Read all csv files in the folder and create a list of dataframes
ldf <- lapply(file_list , read.csv)

# Combine each dataframe in the list into a single dataframe
df.final <- do.call("rbind", ldf)

baseDir3 <- file.path("C:","Users", "ouz001", "working_from_home","soil_testing",  "dig_deeper", "whole_strip", "P", "3Rates", "output_merge")
write.csv(df.final,
          paste0(baseDir3, "/", "landmark_3P_rates_strip_whole.csv"))


### let chcek out what I have done...

str(df.final)
unique(df.final$Rate_name)


df.final$Rate_name <- factor(df.final$Rate_name, levels = c("low_rate", "middle_rate", "high_rate"))



  ggplot(df.final, aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
  #geom_line(aes(linetype = Zone)) +
  geom_line(aes(colour = Zone)) +
  #scale_linetype_manual(values = c("solid", "longdash", "solid")) +
  #scale_color_manual(values = c("red", "black", "black")) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(title = "3 Rates P strips",
       
       x = "Rate" , y = "Yield")+
  facet_wrap(.~site)
  
#######################  
  filter(df.final , Rate_name != "middle_rate") %>%
   ggplot( aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
    #geom_line(aes(linetype = Zone)) +
    geom_line(aes(colour = Zone)) +
    #scale_linetype_manual(values = c("solid", "longdash", "solid")) +
    #scale_color_manual(values = c("red", "black", "black")) +
    theme_bw() +
    theme(legend.position = "top") +
    labs(title = "3 Rates P strips",
         
         x = "Rate" , y = "Yield")+
    facet_wrap(.~site)
  
################################################################################  
list_Of_sites <- as.data.frame(unique(df.final$site))
  
  Tim_paddocks <- filter(df.final , site == "Backsheehans_yld_std"|
                         site == "Clover_yld_std"|
                           site == "Top_yld_std"|
                           site == "Hennessy_yld_std")
  
  #filter(Tim_paddocks , Rate_name != "middle_rate") %>%
    #ggplot( aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
    ggplot(Tim_paddocks, aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
    #geom_line(aes(linetype = Zone)) +
    geom_line(aes(colour = Zone)) +
    #scale_linetype_manual(values = c("solid", "longdash", "solid")) +
    #scale_color_manual(values = c("red", "black", "black")) +
    theme_bw() +
    theme(legend.position = "top") +
    labs(title = "3 Rates P strips",
         
         x = "Rate" , y = "Yield")+
    facet_wrap(.~site)

    
    Alister_Tippert <- filter(df.final , site == "BallsCentre2_yld_std"|
                            site == "BallsSilo1_yld_std"|
                            site == "Jash5_yld_std")
    #filter(Alister_Tippert , Rate_name != "middle_rate") %>%
    #ggplot( aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
    ggplot(Alister_Tippert, aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
      #geom_line(aes(linetype = Zone)) +
      geom_line(aes(colour = Zone)) +
      #scale_linetype_manual(values = c("solid", "longdash", "solid")) +
      #scale_color_manual(values = c("red", "black", "black")) +
      theme_bw() +
      theme(legend.position = "top") +
      labs(title = "3 Rates P strips",
           
           x = "Rate" , y = "Yield")+
      facet_wrap(.~site)
    
    Andrew_McMahen <- filter(df.final , site == "Hickmonts_yld_std"|
                                site == "Jardines_yld_std"|
                                #site == "NOC2_yld_std"|
                                site == "RoundHome_yld_std"|
                                site == "Rail_yld_std")
    #filter(Andrew_McMahen , Rate_name != "middle_rate") %>%
    #ggplot( aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
    ggplot(Andrew_McMahen, aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
      #geom_line(aes(linetype = Zone)) +
      geom_line(aes(colour = Zone)) +
      #scale_linetype_manual(values = c("solid", "longdash", "solid")) +
      #scale_color_manual(values = c("red", "black", "black")) +
      theme_bw() +
      theme(legend.position = "top") +
      labs(title = "3 Rates P strips",
           
           x = "Rate" , y = "Yield")+
      facet_wrap(.~site)
    
##########################################################################################  
    Chris_Dunn <- filter(df.final , site == "Brookland2_yld_std"|
                               site == "Cail_yld_std") 
    ggplot(Chris_Dunn, aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
      #geom_line(aes(linetype = Zone)) +
      geom_line(aes(colour = Zone)) +
      #scale_linetype_manual(values = c("solid", "longdash", "solid")) +
      #scale_color_manual(values = c("red", "black", "black")) +
      theme_bw() +
      theme(legend.position = "top") +
      labs(title = "3 Rates P strips",
           
           x = "Rate" , y = "Yield")+
      facet_wrap(.~site)
########################################################################################    
    Claire_Gutsche <- filter(df.final , site == "Bevs_yld_std"
                             ) 
    ggplot(Claire_Gutsche, aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
      #geom_line(aes(linetype = Zone)) +
      geom_line(aes(colour = Zone)) +
      #scale_linetype_manual(values = c("solid", "longdash", "solid")) +
      #scale_color_manual(values = c("red", "black", "black")) +
      theme_bw() +
      theme(legend.position = "top") +
      labs(title = "3 Rates P strips",
           
           x = "Rate" , y = "Yield")+
      facet_wrap(.~site)
        
########################################################################################    
    Heath_Verco <- filter(df.final , site == "Brennans_yld_std"
    ) 
    ggplot(Heath_Verco, aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
      #geom_line(aes(linetype = Zone)) +
      geom_line(aes(colour = Zone)) +
      #scale_linetype_manual(values = c("solid", "longdash", "solid")) +
      #scale_color_manual(values = c("red", "black", "black")) +
      theme_bw() +
      theme(legend.position = "top") +
      labs(title = "3 Rates P strips",
           
           x = "Rate" , y = "Yield")+
      facet_wrap(.~site)
########################################################################################    
    James_Falvey <- filter(df.final , site == "School_yld_std"|
                             site == "Front1_yld_std"|
                             site == "Front3_yld_std"|
                             site == "Stewarts3_yld_std"
                             
    ) 
    ggplot(James_Falvey, aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
      #geom_line(aes(linetype = Zone)) +
      geom_line(aes(colour = Zone)) +
      #scale_linetype_manual(values = c("solid", "longdash", "solid")) +
      #scale_color_manual(values = c("red", "black", "black")) +
      theme_bw() +
      theme(legend.position = "top") +
      labs(title = "3 Rates P strips",
           
           x = "Rate" , y = "Yield")+
      facet_wrap(.~site)
    
########################################################################################    
    kris_dixon <- filter(df.final , site == "Bella_yld_std"|
                         site == "McKenzie_yld_std"
    ) 
    ggplot(kris_dixon, aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
      #geom_line(aes(linetype = Zone)) +
      geom_line(aes(colour = Zone)) +
      #scale_linetype_manual(values = c("solid", "longdash", "solid")) +
      #scale_color_manual(values = c("red", "black", "black")) +
      theme_bw() +
      theme(legend.position = "top") +
      labs(title = "3 Rates P strips",
           
           x = "Rate" , y = "Yield")+
      facet_wrap(.~site)
########################################################################################    
    steve_richmond <- filter(df.final , site == "Heads_yld_std"|
                               site == "Home06_yld_std"
                             
    ) 
    ggplot(steve_richmond, aes(x = Rate_name, y = Yld_Mass_Av, group = Zone, colour = Zone ))+
      #geom_line(aes(linetype = Zone)) +
      geom_line(aes(colour = Zone)) +
      #scale_linetype_manual(values = c("solid", "longdash", "solid")) +
      #scale_color_manual(values = c("red", "black", "black")) +
      theme_bw() +
      theme(legend.position = "top") +
      labs(title = "3 Rates P strips",
           
           x = "Rate" , y = "Yield")+
      facet_wrap(.~site)  
    