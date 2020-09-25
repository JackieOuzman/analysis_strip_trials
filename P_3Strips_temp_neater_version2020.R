### load in the libraries

#install.packages("PairedData")
#install.packages("RGraphics")
#install.packages("gridExtra")
#install.packages("rdrop2")
libs <- c("dplyr", "tidyverse", 
          "ggplot2", "readxl",
          "PairedData", "cowplot", "grid", 
          "RGraphics", 
          "gridExtra", "rdrop2", "readr")

install.libraries <- function(lib=NULL){
  new <- lib[!(lib %in% installed.packages()[, "Package"])]
  if (length(new)){   
    install.packages(new, dependencies = TRUE)
  }
} 

load.libraries <- function(lib=NULL){
  sapply(libs, require, character.only = TRUE)
}

install.libraries(libs)
load.libraries(libs)
#########################################################
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
assign("strips", function_1_import_data(input_file))

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

##############################################################################################################
# make a table for the report
##############################################################################################################
# table 1 soil testing results


DB_file_name <- "N&P 2019 data for analysis Vic 16 April2020.csv"
# paddock ID 318130 and 318131
paddock_ID_1 <- "318130"
paddock_ID_2 <- "318131"

function_tabel_soil_testing <- function(DB_file_name, paddock_ID_1, paddock_ID_2){
paddock_ID <- c(paddock_ID_1, paddock_ID_2)
harm_database  <- read_csv(paste0("W:/value_soil_testing_prj/data_base/",DB_file_name))

#fix up some names
harm_database<-
  dplyr::select(harm_database,
                "Paddock_code" =  `Paddock code`,     
                Contact, Farmer,
                "Paddock_tested" = `Paddock tested`,
                Zone ,
                Colwell,
                DGT,
                PBI ,
                `Total N`,
                `Colwell rec rate`,
                `DGT rec rate`)
#remove the row that is missing..
harm_database <-filter(harm_database, Paddock_code != "NA")


#extract the paddock I want 
site <- filter(harm_database,
               Paddock_code == paddock_ID) %>% 
   dplyr::select(5, 6: 11)

#remove the text
site <-site %>% 
  mutate(`Colwell rec rate` = replace(`Colwell rec rate`,
                                      `Colwell rec rate` == 'Replacement', 
                                      "9999"),
         `DGT rec rate` = replace(`DGT rec rate`,
                                  `DGT rec rate` == 'Replacement', 
                                  "9999"))

site$`Colwell rec rate` <- as.numeric(site$`Colwell rec rate`)
site$`DGT rec rate` <- as.numeric(site$`DGT rec rate`)
site <- site %>% mutate_if(is.numeric, ~round(., 0))
site$`Colwell rec rate` <- as.character(site$`Colwell rec rate`)
site$`DGT rec rate` <- as.character(site$`DGT rec rate`)

#put the replacement back in
site <- site %>% mutate_if(is.character,~replace(.,.== "9999", 'Replacement'))
site
return(site)
}
assign(("site"), function_tabel_soil_testing(DB_file_name, paddock_ID_1, paddock_ID_2))


##############################################################################################################
# table 2 yield  results
#this is adding aclm that should be in the import file but is empty now
all_results <- all_results %>% mutate(Details = NA)

function_tabel_yield <- function(all_results){
mean_zone_av_output_display <-all_results %>% 
  mutate(Significant = case_when(Significant == "significant" ~ "*",
                                 TRUE ~ "" ))
mean_zone_av_output_display <- mean_zone_av_output_display %>% mutate_if(is.numeric, ~round(., 1))
mean_zone_av_output_display <- mutate(mean_zone_av_output_display,
                                      Yld = paste0(yield, Significant))
mean_zone_av_output_display <- dplyr::select(mean_zone_av_output_display, Rate, zone, Details, Yld)
mean_zone_av_output_display <- spread(mean_zone_av_output_display, zone, Yld)
mean_zone_av_output_display <- mean_zone_av_output_display[c(1,3,4,2)] #record the clms

return(mean_zone_av_output_display)
}

assign(("tabel_yield"), function_tabel_yield(all_results))

TSpecial <- ttheme_minimal(base_size = 8)
table1 <- tableGrob(site , rows = NULL, theme=TSpecial )
table2 <- tableGrob(tabel_yield, rows = NULL, theme=TSpecial)


plot_whole_strip
plot_zone1
plot_zone2

table1
paddock

collection <- grid.arrange(plot_zone1, plot_zone2, table2, table1, plot_whole_strip, nrow = 5,  ncol=2, 
                           layout_matrix = cbind(c(1,1,3,5,5), c(2,2,4,5,5)),
                           bottom = textGrob(
                             Sys.Date(),
                             gp = gpar(fontface = 3, fontsize = 9),
                             hjust = 2,
                             x = 1
                           ))

collection


#collection
# ggsave(path= graph_path, filename = paste0(paddock, "_collection.png"), device = "png", 
#        width = 35, height = 20, units = "cm", collection)
