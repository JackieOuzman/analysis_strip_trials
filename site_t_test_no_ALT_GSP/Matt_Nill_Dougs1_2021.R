### load in the libraries


libs <- c("dplyr", "tidyverse", 
          "ggplot2", "readxl",
          "PairedData", "cowplot", "grid", 
          "RGraphics", 
          "gridExtra", "rdrop2", "readr", "readxl")

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


#group the reuslts into P and N and then into number of strips
#tidy this code up so I can have functions for analysis
# perhaps have a file that list the site and location of data along with paddock ID as well as a date when analysis was run

#############################################################################################################
#######################         set up file directory                               ########################
Trial_type <- "N"  # "P"
Number_of_strips <-   "strip3" # "strip2"
  
baseDir <- file.path("C:","Users", "ouz001", "working_from_home","soil_testing",  "Streamline" , Trial_type, Number_of_strips)
outputDir <- file.path("C:","Users", "ouz001", "working_from_home","soil_testing",  "Streamline")
baseDir
list.files(baseDir, full.names = FALSE)
###########################################################################################################
## I would be good to work on this step to run all the files in the directory at once.

input_file <-"Dougs1_SegID_Zone.csv"
name_Paddock <- unlist(strsplit(input_file,"_"))[1]
## add this into the strips df


################################################################################################################
#######################         Read in the yield data                               ########################### 

function_1_import_data <- function(input_file){
  strips <- read_csv(paste0(baseDir, "/",input_file))
  strips <- strips %>% 
    mutate(name_Paddock= name_Paddock,
           input_file = name_Paddock)
  
  return(strips)}

assign("strips", function_1_import_data(input_file))

###############################################################################################################
#############    This analysis doesnt include the Alt GSP strip   so i will remove it now  ####################

strips <-   strips %>% 
  filter((GSP !="Alt GSP") %>% 
             replace_na(TRUE))

################################################################################################################

#tidy up data frame
#make clms standard and remove the NA vlues in rate clm - having trouble with this as function because of the errors if the clm name doesnt exist
names(strips)

function_2_tidy_clm <- function(strips) {
  
  strips <- if (c("Yld_Mass_D") %in% names(strips) == TRUE) {
    rename(strips, YldMassDry = Yld_Mass_D)
  } else {
    rename(strips, YldMassDry = YldMassDry)
  }
  
  strips <- filter(strips,!is.na(Rate))
  strips$Rate <- as.double(strips$Rate) #need to double check that I always have a number
  strips$YldMassDry <- as.double(strips$YldMassDry)
  strips$DistOnLine <- as.double(strips$DistOnLine)
   
   ### clean the data removing zero values
  strips <- filter(strips,
                  DistOnLine != 0)
  strips <- filter(strips,
                  YldMassDry != 0)
   ### Add correction to segmentID so we can read it as meters (this will depend on how it was defined in spatial)
  strips <-  mutate(strips,
                   SegmentID = SegmentID * 10)
  
  return(strips)
  }


assign("strips", function_2_tidy_clm(strips))

 

##################################################################################################################
## details of what was applied and the GR
##################################################################################################################


#need to assign a extra clms with the GR and details of product to this df rate name, details, starter fertiliser and topdress
#also can't make this a function because I want to return multiple R objcets

Rates_labels <- data.frame(Rate = unique(strips$Rate)) %>% 
  arrange(Rate) %>% 
  mutate(rate_name_order = c("low","medium" , "high")) 

#what the rate of the GR?
GSP_rate <- strips %>% 
  filter(GSP == "GSP") %>% 
  distinct(Rate)


Rates_labels <- Rates_labels %>% 
  mutate(
    rate_name = case_when(
      Rate == GSP_rate[[1]] ~ "Grower_rate",
      TRUE ~ rate_name_order))  
#and again to get the other names 
Rates_labels <- Rates_labels %>%
  mutate(rate_name = case_when(
    rate_name == "low" ~ "rate1",
    rate_name == "high" ~ "rate2",
    TRUE ~ rate_name
  ))

### need to add in the zone ID here

strips <- left_join(strips, Rates_labels, by= "Rate")

x <- Rates_labels[1,1]
y <- Rates_labels[2,1]
z <- Rates_labels[3,1]


str(strips)
unique(strips$Zone_ID)

labels_graph <- dplyr::select(strips, Rate, Strip_Rate, Start_Fert, Top_Dress) %>% 
  distinct(Strip_Rate, .keep_all = TRUE)
labels_graph <- left_join(labels_graph, Rates_labels, by = "Rate") 

####################################################################################################################
##################################################################################################################
## details of zones
##################################################################################################################
Zone_labels <- strips %>%
  distinct(Zone, .keep_all = TRUE) %>%
  filter(!is.na(Zone)) %>%
  arrange(Zone) %>% #this should work with text
  mutate(zone_name = c("zone1","zone2" )) %>%
  dplyr::select(Zone, zone_name, Zone_ID)



# join this to the strips data
strips <- left_join(strips, Zone_labels, by= "Zone")
names(strips)
strips <- strips %>% dplyr::select(-Zone_ID.x) %>% 
  rename(Zone_ID = Zone_ID.y)
  
  
strips <- strips %>% 
  mutate(zone_name2 = ifelse(is.na(zone_name), NA , paste0(strips$zone_name, "_", strips$Zone)  ))


###############################################################################################################
#Prep the data making a sub selection of df for each zone and run the paired t test

function_paired_ttest <- function(strips, zone_x, rate_x){

#select the zone data and the rates
zone_x_rateXvsGR <- filter(strips,
                          zone_name == paste0("zone", zone_x)) %>%
                    filter(rate_name == paste0("rate", rate_x) | rate_name == "Grower_rate")

#average the yld per segment and rate
zone_x_rateXvsGR_av <- group_by(zone_x_rateXvsGR, SegmentID, Rate, Zone, rate_name, zone_name ) %>% 
  summarise_all(mean, na.rm= TRUE)
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
  summarise(
    yield = mean(YldMassDry, na.rm = TRUE),
    n = n(),
    sd = sd(YldMassDry),
    se = sd / sqrt(n),
    PtCount_tally = sum(PtCount)
  )  %>%
  mutate(zone = "zone1")


mean_zone2 <-  filter(strips,
                      zone_name == "zone2") %>%
  group_by(Rate) %>%
  summarise(
    yield = mean(YldMassDry, na.rm = TRUE),
    n = n(),
    sd = sd(YldMassDry),
    se = sd / sqrt(n),
    PtCount_tally = sum(PtCount)
  )  %>%
  mutate(zone = "zone2")

mean_zone <- bind_rows(mean_zone1, mean_zone2)
mean_zone <- left_join(mean_zone,Rates_labels)

results_ttest <- left_join(mean_zone, results_ttest)
return(results_ttest)}

assign(("all_results"), function_all_results(zone_1rate_1,
                                            zone_1rate_2,
                                            zone_2rate_1,
                                            zone_2rate_2))
rm(zone_1rate_1,
   zone_1rate_2,
   zone_2rate_1,
   zone_2rate_2,)
##################################################################################################################
### Plotting the results
## step 1 complie the results avearge of segment per zone
names(strips)

for_plotting <- filter(strips, !is.na(zone_name)) %>% 
        group_by(Rate, Zone, rate_name, zone_name, zone_name2, name_Paddock,SegmentID, ) %>% 
        summarise_all(mean)

function_zone_plots <- function(for_plotting, zone_x){

for_plotting$rate_as_factor <- as.factor(for_plotting$Rate)
label_zone <- filter(for_plotting, zone_name == paste0("zone", zone_x)) 
label_zone<-   unique(label_zone$zone_name2)
label_zone <- str_split(label_zone, "_", simplify = TRUE)
label_zone <- label_zone[1,2]

max_yld <- max(for_plotting$YldMassDry, na.rm = TRUE)

zone_plot <- filter(for_plotting, zone_name == paste0("zone", zone_x)) %>% 
  ggplot( aes(rate_as_factor, YldMassDry))+
  geom_boxplot(alpha=0.1)+
  geom_point(colour = "blue", alpha = 0.1)+
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed")+
  theme_bw()+
  ylim(0,max_yld)+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10))+
  labs(x = "Fertiliser Rates",
       y= "Yield t/ha",
       title = paste0( label_zone))+
       #caption = "Below table reports mean values and significant differences compared to GSR"
       #)+
       theme(plot.caption = element_text(size=8, face="italic", color="black"))
       #+
       #annotate("text", x = 2, y= 0, size = 3,label = "box plot = 25%, 50%, 75%, dashed line = mean")

return(zone_plot)
}
names(for_plotting)


assign(("plot_zone1"), function_zone_plots(for_plotting, 1))
assign(("plot_zone2"), function_zone_plots(for_plotting, 2))
plot_zone1
plot_zone2

############################################################################################################
## pull out some info for the labels
#labels_graph Note that these are the same for all strips!!
Grower_rate_label_a <- labels_graph %>% 
  filter(rate_name == "Grower_rate") %>% 
  dplyr::select("Strip_Rate")
Grower_rate_label <- paste0("Grower rate: ", Grower_rate_label_a)
rm(Grower_rate_label_a)

Starter_label_a <- labels_graph %>% 
  distinct(Start_Fert) 
Starter_label <- paste0("Starter: ", Starter_label_a)
rm(Starter_label_a)

Topdress_label_a <- labels_graph %>% 
  distinct(Top_Dress) 
Topdress_label <- paste0("Top dress: ", Topdress_label_a)
rm(Topdress_label_a)

Grower_rate_label
Starter_label
Topdress_label

function_strip_plot <- function(for_plotting){

for_plotting$rate_as_factor <- as.factor(for_plotting$Rate)  

zone1_min <- filter(for_plotting, zone_name == "zone1") %>% summarise(min_zone = min(SegmentID))
zone1_min <- zone1_min[[7]]
zone1_min <- zone1_min[[1]]
zone1_max <- filter(for_plotting, zone_name == "zone1") %>% summarise(max_zone = max(SegmentID))
zone1_max <- zone1_max[[7]]  
zone1_max <- zone1_max[[1]]
zone2_min <- filter(for_plotting, zone_name == "zone2") %>% summarise(min_zone = min(SegmentID))
zone2_min <- zone2_min[[7]]
zone2_min <- zone2_min[[1]]
zone2_max <- filter(for_plotting, zone_name == "zone2") %>% summarise(max_zone = max(SegmentID))
zone2_max <- zone2_max[[7]] 
zone2_max <- zone2_max[[1]] 

zone1_range <- ((zone1_max - zone1_min)/2)+zone1_min
zone2_range <- ((zone2_max - zone2_min)/2)+zone2_min
zone1_range
zone2_range

max_yld <- max(for_plotting$YldMassDry, na.rm = TRUE)
min_yld <- min(for_plotting$YldMassDry, na.rm = TRUE)

label_paddock<-   unique(for_plotting$name_Paddock)
label_paddock <- str_split(label_paddock, "_", simplify = TRUE)
label_paddock <- label_paddock[1,1]

label_zone1 <- ungroup(for_plotting) %>% 
  dplyr::select(Zone, zone_name) %>%
  filter( zone_name == "zone1") %>% 
  distinct(Zone)

label_zone2 <- ungroup(for_plotting) %>% 
  dplyr::select(Zone, zone_name) %>%
  filter( zone_name == "zone2") %>% 
  distinct(Zone)

whole_strip <- ggplot(for_plotting, aes(SegmentID , YldMassDry, group = rate_as_factor))+
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
  annotate("text", x = zone2_range, y= 0,label =label_zone2)#+

return(whole_strip)}
assign(("plot_whole_strip"), function_strip_plot(for_plotting))

plot_whole_strip

##############################################################################################################
# make a table for the report
##############################################################################################################
# table 1 soil testing results



paddock_ID <- data.frame(distinct(strips,Zone_ID)) %>% 
   filter(!is.na(Zone_ID))
paddock_ID_1 <- paddock_ID[1,1]
paddock_ID_2 <- paddock_ID[2,1]
paddock_ID <- c(paddock_ID_1, paddock_ID_2)

function_tabel_soil_testing <- function( paddock_ID_1, paddock_ID_2){

harm_database <- read_excel( "C:/Users/ouz001/working_from_home/soil_testing/Streamline/GRDC 2020 Paddock Database_SA_VIC_Jan14 2021.xlsx")

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
                `Mineral N` = `Total N`,
                `Colwell rec rate`,
                `DGT rec rate`)
#remove the row that is missing..
harm_database <-filter(harm_database, Paddock_code != "NA")


#extract the paddock I want 
site <- filter(harm_database,
               Paddock_code == paddock_ID_1 |
                 Paddock_code == paddock_ID_2) %>% 
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
assign(("site"), function_tabel_soil_testing( paddock_ID_1, paddock_ID_2))


##############################################################################################################
# table 2 yield  results
#this is adding a clm that should be in the import file but is empty now
#all_results <- all_results %>% mutate(Details = NA)


str(all_results)
str(labels_graph)
## just drop a few clms
labels_graph <- labels_graph %>%  dplyr::select(Rate, Strip_Rate, Start_Fert, Top_Dress)
all_results <- left_join(all_results, labels_graph, by = "Rate")


all_results <- rename(all_results, Details = Strip_Rate)
## need to add in the zone ID

all_results_1 <- left_join(all_results, Zone_labels, by= c("zone"= "zone_name"))

function_tabel_yield <- function(all_results, Zone_labels){
#function_tabel_yield <- function(all_results){
  all_results <- left_join(all_results, Zone_labels, by= c("zone"= "zone_name"))
mean_zone_av_output_display <-all_results %>% 
  mutate(Significant = case_when(Significant == "significant" ~ "*",
                                 TRUE ~ "" ))
mean_zone_av_output_display <- mean_zone_av_output_display %>% mutate_if(is.numeric, ~round(., 1))
mean_zone_av_output_display <- mutate(mean_zone_av_output_display,
                                      Yld = paste0(yield, Significant))
mean_zone_av_output_display <- dplyr::select(mean_zone_av_output_display, Rate, Zone, Details, Yld)
mean_zone_av_output_display <- spread(mean_zone_av_output_display, Zone, Yld)
mean_zone_av_output_display <- mean_zone_av_output_display[c(1,3,4,2)] #record the clms

return(mean_zone_av_output_display)
}

assign(("tabel_yield"), function_tabel_yield(all_results, Zone_labels))

TSpecial <- ttheme_minimal(base_size = 8)
table1 <- tableGrob(site , rows = NULL, theme=TSpecial )
table2 <- tableGrob(tabel_yield, rows = NULL, theme=TSpecial)


note_on_graph <- paste0("The table reports mean values and * denotes significant differences compared to GSR (Pvalue <0.05). ",
                        "\n", 
                        "box plot = 25%, 50%, 75%, dashed line = mean .",
                        "\n",
                        Sys.Date())



collection <-
  grid.arrange(
    plot_zone1,
    plot_zone2,
    table2,
    table1,
    plot_whole_strip,
    nrow = 5,
    ncol = 2,
    layout_matrix = cbind(c(1, 1, 3, 5, 5), c(2, 2, 4, 5, 5)),
    bottom = textGrob(
      note_on_graph,
      gp = gpar(fontface = 3, fontsize = 9),
      #hjust = 2,
      #hjust = 0,
      #x = 1
    )
  )








##################################################################################################
## saving outputs the graph and the results ######

#a csv output file
str(all_results_1)
#add zone ID paddock code with trial type 
 names(strips)      

all_results_1 <- all_results_1 %>% 
  mutate(paddock_ID_Type = paste0(unique(strips$Paddock_ID),"_",
                                   unique(strips$Strip_Type) ),
         input_file = input_file)

#save the output
write.csv(all_results_1, paste0(outputDir, "/results_grower_", 
                              distinct(all_results_1,paddock_ID_Type),
                              "_",
                              input_file))

# the graph 
outputDir
collection
ggsave( filename = 
         paste0(outputDir,  "/",
                distinct(all_results_1,paddock_ID_Type),
                "_",
                str_replace(input_file, ".csv", ""),
          "_collection.png"), device = "png", 
        width = 35, height = 20, units = "cm", collection)



#######################################################################################################################################
################                        Extra analysis for Ricks tables this is added to below t test      ############################
######################################################################################################################################
#######################################################################################################################################
################                        Extra analysis for Ricks tables this is added to below t test      ############################
######################################################################################################################################

#str(strips)
#unique(strips$GSP) #just chceking we dont have Alt GSP
for_ricks_tables_1 <- strips %>% 
  filter(!is.na(zone_name)) %>%
  dplyr::select(Zone_ID, SegmentID , YldMassDry, Rate, rate_name_order)

str(for_ricks_tables_1)

# first I need to make anew clm for the comparsions
for_ricks_tables_1 <- for_ricks_tables_1 %>% 
  mutate(
    comparison_high_low = case_when(
      rate_name_order == "high" ~ "high_low",
      rate_name_order == "low" ~ "high_low",
      TRUE                      ~ "other"
    ),
    comparison_high_medium = case_when(
      rate_name_order == "high" ~   "high_medium",
      rate_name_order == "medium" ~  "high_medium",
      TRUE                      ~ "other"
    ),
    comparison_medium_low = case_when(
      rate_name_order == "low" ~   "medium_low",
      rate_name_order == "medium" ~  "medium_low",
      TRUE                      ~ "other"
    )
  )
######################################################################################################################################
### for each zone and comparsion what is the mean and st error



function_grand_mean_std_error <- function(comparison){

clm <- paste0("comparison_", comparison)
comparison_grand_mean <- paste0("grand_mean_", comparison)
comparison_se <- paste0("se_comp_", comparison)


grand_mean_std_error <- for_ricks_tables_1 %>%
  dplyr::filter(.data[[clm[[1]]]] == comparison) %>% 
  group_by(Zone_ID ) %>% 
  summarise(!!comparison_grand_mean := mean(YldMassDry,na.rm = TRUE ),
            sd = sd(YldMassDry),
            n = n(),
            !!comparison_se := sd / sqrt(n)) %>% 
  dplyr::select(-sd, -n)

grand_mean_std_error
}
#### assign name according to comparsion

assign(paste0("grand_mean_std_error_", "high_low"), function_grand_mean_std_error("high_low"))
assign(paste0("grand_mean_std_error_", "high_medium"), function_grand_mean_std_error("high_medium"))
assign(paste0("grand_mean_std_error_", "medium_low"), function_grand_mean_std_error("medium_low"))



grand_mean_se <- left_join(grand_mean_std_error_high_low,grand_mean_std_error_high_medium )
grand_mean_se <- left_join(grand_mean_se,grand_mean_std_error_medium_low )


## I need to generate mean yield value for the zone and Rate

for_ricks_tables_2 <- for_ricks_tables_1 %>% 
  group_by( Zone_ID, Rate, rate_name_order) %>% 
  summarise(zone_yld = mean(YldMassDry, na.rm = TRUE))



for_ricks_tables_wide <- pivot_wider(for_ricks_tables_2, 
                                     id_cols = c( Zone_ID),
                                     names_from =rate_name_order,
                                     values_from = zone_yld
)


## differences in yld clms
for_ricks_tables_wide <- for_ricks_tables_wide %>% 
  mutate(high_vs_low = high - low,
         high_vs_medium = high- medium,
         medium_vs_low =  medium - low)
str(for_ricks_tables_wide)

for_ricks_tables_wide <- ungroup(for_ricks_tables_wide)

for_ricks_tables_wide
grand_mean_se


for_ricks_tables_wide <- left_join(for_ricks_tables_wide, grand_mean_se)




#####
for_ricks_tables_summary <- for_ricks_tables_wide %>%
  mutate(
    yld_resposne_high_v_low =  case_when(
      high_vs_low > 0 + se_comp_high_low ~ "positive",
      high_vs_low < 0 - se_comp_high_low ~ "negative",
      TRUE ~ "no_response"
    ),
    yld_resposne_high_v_medium =  case_when(
      high_vs_medium  > 0 +  se_comp_high_medium ~ "positive",
      high_vs_medium  < 0 -  se_comp_high_medium ~ "negative",
      TRUE ~ "no_response"
    ),
    yld_resposne_medium_v_low =  case_when(
      medium_vs_low  > 0 +  se_comp_medium_low    ~ "positive",
      medium_vs_low  < 0 -  se_comp_medium_low    ~ "negative",
      TRUE ~ "no_response"
    )
  )



for_ricks_tables_summary <- for_ricks_tables_summary %>% 
  pivot_longer(cols = c("yld_resposne_high_v_low","yld_resposne_high_v_medium", "yld_resposne_medium_v_low"),
               names_to = "comparison",
               values_to = "yld_response") %>% 
  dplyr::select(Zone_ID, comparison, yld_response, low, medium, high,
                high_vs_low, high_vs_medium, medium_vs_low,
                se_comp_high_low, se_comp_medium_low, se_comp_high_medium) %>% 
  mutate(
    comparison = case_when(
      comparison == "yld_resposne_high_v_low" ~ "high_v_low",
      comparison == "yld_resposne_high_v_medium" ~ "high_v_medium",
      comparison == "yld_resposne_medium_v_low" ~ "medium_v_low"
    ))

for_ricks_tables_summary



### Extra t test #######################################################################################################################

#Prep the data making a sub selection of df for each zone and run the paired t test




function_paired_ttest_rate_order <- function(strips, zone_x){
  
  #select the zone data and the high vs low rates
  zone_x_high_vs_low <- strips %>% 
    filter(zone_name == paste0("zone", zone_x)) %>%
    filter(rate_name_order == "low" | rate_name_order == "high")
  
  #average the yld per segment and rate
  zone_x_high_vs_low_av <- group_by(zone_x_high_vs_low, SegmentID, Rate, Zone, rate_name, zone_name , rate_name_order) %>% 
    summarise_all(mean, na.rm= TRUE)
  #ensure that the dataset is duplictaed
  list_SegmentID_values_hvl <- zone_x_high_vs_low_av$SegmentID[duplicated(zone_x_high_vs_low$SegmentID)] #this returns a list of values I want to keep
  zone_x_high_vs_low_av <- zone_x_high_vs_low_av %>% filter(SegmentID %in% list_SegmentID_values_hvl)
  # run paired ttest
  zone_x_high_vs_low_res <- t.test(YldMassDry ~ rate_name_order, data = zone_x_high_vs_low_av, paired = TRUE)
  
  #####test results
  # Report values from the t.test
  zone_x_high_vs_low_res_sig <-
    data.frame(P_value = as.double(zone_x_high_vs_low_res$p.value),
               Mean_diff = (zone_x_high_vs_low_res$estimate)) %>%
    mutate(
      comparison = "high_v_low",
      zone = paste0("zone", zone_x),
      rounded = abs(round(Mean_diff, 2)),
      Significant = case_when(P_value < 0.05 ~ "significant",
                              TRUE ~ "not significant"))
  zone_x_high_vs_low_res_sig 
  
  ##########################################################################################################################
  #select the zone data and the high vs medium rates
  zone_x_high_vs_medium <- strips %>% 
    filter(zone_name == paste0("zone", zone_x)) %>%
    filter(rate_name_order == "medium" | rate_name_order == "high")
  
  #average the yld per segment and rate
  zone_x_high_vs_medium_av <- group_by(zone_x_high_vs_medium, SegmentID, Rate, Zone, rate_name, zone_name , rate_name_order) %>% 
    summarise_all(mean, na.rm= TRUE)
  #ensure that the dataset is duplictaed
  list_SegmentID_values_hvm <- zone_x_high_vs_medium_av$SegmentID[duplicated(zone_x_high_vs_medium$SegmentID)] #this returns a list of values I want to keep
  zone_x_high_vs_medium_av <- zone_x_high_vs_medium_av %>% filter(SegmentID %in% list_SegmentID_values_hvm)
  # run paired ttest
  zone_x_high_vs_medium_res <- t.test(YldMassDry ~ rate_name_order, data = zone_x_high_vs_medium_av, paired = TRUE)
  
  #####test results
  # Report values from the t.test
  zone_x_high_vs_medium_res_sig <-
    data.frame(P_value = as.double(zone_x_high_vs_medium_res$p.value),
               Mean_diff = (zone_x_high_vs_medium_res$estimate)) %>%
    mutate(
      comparison = "high_v_medium",
      zone = paste0("zone", zone_x),
      rounded = abs(round(Mean_diff, 2)),
      Significant = case_when(P_value < 0.05 ~ "significant",
                              TRUE ~ "not significant"))
  zone_x_high_vs_medium_res_sig 
  
  ##########################################################################################################################
  #select the zone data and the medium vs low rates
  zone_x_medium_vs_low <- strips %>% 
    filter(zone_name == paste0("zone", zone_x)) %>%
    filter(rate_name_order == "medium" | rate_name_order == "low")
  
  #average the yld per segment and rate
  zone_x_medium_vs_low_av <- group_by(zone_x_medium_vs_low, SegmentID, Rate, Zone, rate_name, zone_name , rate_name_order) %>% 
    summarise_all(mean, na.rm= TRUE)
  #ensure that the dataset is duplictaed
  list_SegmentID_values_mvl <- zone_x_medium_vs_low_av$SegmentID[duplicated(zone_x_medium_vs_low$SegmentID)] #this returns a list of values I want to keep
  zone_x_medium_vs_low_av <- zone_x_medium_vs_low_av %>% filter(SegmentID %in% list_SegmentID_values_mvl)
  # run paired ttest
  zone_x_medium_vs_low_res <- t.test(YldMassDry ~ rate_name_order, data = zone_x_medium_vs_low_av, paired = TRUE)
  
  #####test results
  # Report values from the t.test
  zone_x_medium_vs_low_res_sig <-
    data.frame(P_value = as.double(zone_x_medium_vs_low_res$p.value),
               Mean_diff = (zone_x_medium_vs_low_res$estimate)) %>%
    mutate(
      comparison = "medium_v_low",
      zone = paste0("zone", zone_x),
      rounded = abs(round(Mean_diff, 2)),
      Significant = case_when(P_value < 0.05 ~ "significant",
                              TRUE ~ "not significant"))
  
  zone_x_high_vs_low_res_sig
  zone_x_high_vs_medium_res_sig
  zone_x_medium_vs_low_res_sig 
  
  zone_x_high_vs_medium_vs_low_res_sig <- rbind(zone_x_high_vs_low_res_sig, 
                                                zone_x_high_vs_medium_res_sig,
                                                zone_x_medium_vs_low_res_sig)
  
  return(data.frame(zone_x_high_vs_medium_vs_low_res_sig))
}
assign(paste0("rate_order_", "zone_", "1"), function_paired_ttest_rate_order(strips, 1))
assign(paste0("rate_order_","zone_", "2"), function_paired_ttest_rate_order(strips, 2))



rate_order_all <- rbind(rate_order_zone_1, rate_order_zone_2) 
rate_order_all <- left_join(rate_order_all, Zone_labels, by = c("zone"=  "zone_name"))



## turn for_ricks_tables_summary_into_narrow_format
str(for_ricks_tables_summary)
str(rate_order_all)

for_ricks_tables_summary <- left_join(for_ricks_tables_summary, rate_order_all)

names(for_ricks_tables_summary)

for_ricks_tables_summary <- for_ricks_tables_summary %>% 
  dplyr::select(Zone_ID, Zone, comparison, yld_response,
                low,medium,high, 
                high_vs_low,high_vs_medium,medium_vs_low,
                Significant,
                yld_response,P_value )

## add in a few clms that help later
for_ricks_tables_summary <- for_ricks_tables_summary %>% 
  mutate(paddock_ID = unique(strips$Paddock_ID),
         Strip_Type = unique(strips$Strip_Type),
         input_file = input_file)


###################################################################################################################################
## what are we saving or have saved 
#for_ricks_tables_summary (saved below)
#all_results_1 (saved in line approx 556 )
#collection (saved in line approx 566 )



#save the output

name <- paste0("C:/Users/ouz001/working_from_home/soil_testing/Streamline/output/high_low_comparision/high_low_comp_", distinct(all_results_1,paddock_ID_Type), ".csv")

write.csv(for_ricks_tables_summary, name)
