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
Trial_type <- "P"  # "N" "P"
Number_of_strips <-   "strip3" # "strip2" "strip3"

baseDir <- file.path("W:","value_soil_testing_prj", "Yield_data", "2020","processing",  "spatial_output" , Trial_type, Number_of_strips)
outputDir <- file.path("W:","value_soil_testing_prj", "Yield_data", "2020","processing", "r_outputs")
baseDir
list.files(baseDir, full.names = FALSE)
###########################################################################################################
## I would be good to work on this step to run all the files in the directory at once.

input_file <-"NaradoonEast_Yld_SegID_zones.csv"
name_Paddock <- unlist(strsplit(input_file,"_"))[1]
## add this into the strips df
name_Paddock <- "Vallance - Naradoon East"
name_Paddock

################################################################################################################
#######################         Read in the yield data                               ########################### 

function_1_import_data <- function(input_file){
  strips <- read_csv(paste0(baseDir, "/",input_file),
                     col_types = cols(GSP = col_character(), 
                                      Zone = col_character(), 
                                      Zone_ID = col_number()))
  
  strips <- strips %>% 
    mutate(name_Paddock= name_Paddock,
           input_file = name_Paddock)
  
  return(strips)}

assign("strips", function_1_import_data(input_file))

###############################################################################################################
#############    This analysis doesnt include the Alt GSP strip   so i will remove it now  ####################


strips <-   strips %>% 
  dplyr::filter((GSP !="Alt GSP") %>% 
             tidyr::replace_na(TRUE))

################################################################################################################

#tidy up data frame
#make clms standard and remove the NA vlues in rate clm - having trouble with this as function because of the errors if the clm name doesnt exist
names(strips)

function_2_tidy_clm <- function(strips) {
  
  strips <- if (c("Yld_Mass_D") %in% names(strips) == TRUE) {
    rename(strips, YldMassDry = Yield)
  } else {
    rename(strips, YldMassDry = Yield)
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

unique(strips$Rate)
unique(strips$GSP)

Rates_labels <- data.frame(Rate = unique(strips$Rate)) %>% 
  arrange(Rate) %>% 
  #mutate(rate_name_order = c("low","medium" , "high", "very_high")) 
  mutate(rate_name_order = c("low","medium" , "high")) 

#what the rate of the GR?
GSP_rate <- strips %>% 
  filter(GSP == "GSP") %>% 
  distinct(Rate)
GSP_rate

Rates_labels <- Rates_labels %>% 
  mutate(
    rate_name = case_when(
      Rate == GSP_rate[[1]] ~ "Grower_rate",
      TRUE ~ rate_name_order))  
#and again to get the other names 
Rates_labels <- Rates_labels %>%
  mutate(rate_name = case_when(
    rate_name == "low" ~ "rate1",
    rate_name == "medium" ~ "rate2",
    rate_name == "high" ~ "rate2",
    #rate_name == "very_high" ~ "rate3",
    TRUE ~ rate_name
  ))

### !!! user to check that this is correct
Rates_labels




### need to add in the zone ID here

strips <- left_join(strips, Rates_labels, by= "Rate")
names(strips)
x <- Rates_labels[1,1]
y <- Rates_labels[2,1]
z <- Rates_labels[3,1]
zz <- Rates_labels[4,1]


str(strips)
unique(strips$Zone_ID)

labels_graph <- dplyr::select(strips, Rate, Strip_Rate, Start_Fert, Top_Dress) %>% 
  distinct(Strip_Rate, .keep_all = TRUE)
labels_graph <- left_join(labels_graph, Rates_labels, by = "Rate") 
labels_graph
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

Zone_labels

# join this to the strips data
strips <- left_join(strips, Zone_labels, by= "Zone")
names(strips)
strips <- strips %>% dplyr::select(-Zone_ID.x) %>% 
  rename(Zone_ID = Zone_ID.y)
  
  
strips <- strips %>% 
  mutate(zone_name2 = ifelse(is.na(zone_name), NA , paste0(strips$zone_name, "_", strips$Zone)  ))

unique(strips$rate_name)
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

assign(paste0("zone_", "1", "rate_", "3"), function_paired_ttest(strips, 1, 3))
assign(paste0("zone_", "2", "rate_", "3"), function_paired_ttest(strips, 2, 3))


#what ran?
zone_1rate_1
zone_1rate_2
zone_2rate_1
zone_2rate_2

zone_1rate_3
zone_2rate_3

################################################################################################################
## function to join everything  togther
#join these togther
function_all_results <- function(zone_1rate_1,
                                 zone_1rate_2,
                                 zone_2rate_1,
                                 zone_2rate_2) #,
                                 # zone_1rate_3,
                                 # zone_2rate_3)
                                 {
results_ttest <- bind_rows(zone_1rate_1,
                           zone_1rate_2,
                           zone_2rate_1,
                           zone_2rate_2) #,
                           #zone_1rate_3,
                           #zone_2rate_3)

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
#mean_zone <- mean_zone1
mean_zone <- left_join(mean_zone,Rates_labels)

results_ttest <- left_join(mean_zone, results_ttest)
return(results_ttest)}

assign(("all_results"), function_all_results(zone_1rate_1,
                                            zone_1rate_2,
                                            zone_2rate_1,
                                            zone_2rate_2
                                            #zone_1rate_3,
                                            #zone_2rate_3
                                            ))
rm(zone_1rate_1,
   zone_1rate_2,
   zone_2rate_1,
   zone_2rate_2,)
##################################################################################################################
### Plotting the results
## step 1 complie the results avearge of segment per zone
names(strips)

for_plotting <-
  #filter(strips, !is.na(zone_name)) %>%
  strips %>%
  group_by(Rate,
           Zone,
           rate_name,
           zone_name,
           zone_name2,
           name_Paddock,
           SegmentID,
  ) %>%
  summarise_all(mean)

function_zone_plots <- function(for_plotting, zone_x){

for_plotting$rate_as_factor <- as.factor(for_plotting$Rate)
label_zone <- filter(for_plotting, zone_name == paste0("zone", zone_x)) 
label_zone<-   unique(label_zone$zone_name2)
label_zone <- stringr::str_split(label_zone, "_", simplify = TRUE)
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

harm_database <- read_excel( "W:/value_soil_testing_prj/Yield_data/2020/processing/GRDC 2020 Paddock Database_SA_VIC_May25 2021.xlsx")

#fix up some names
names(harm_database)
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
    mutate(Significant = case_when(Significant == "significant"  & rounded > 0.1 ~ "*",
                                   TRUE ~ "" ))
mean_zone_av_output_display <- mean_zone_av_output_display %>% mutate_if(is.numeric, ~round(., 2))
mean_zone_av_output_display <- mutate(mean_zone_av_output_display,
                                      Yld = paste0(yield, Significant))
mean_zone_av_output_display <- dplyr::select(mean_zone_av_output_display, Rate, Zone, Details, Yld)
mean_zone_av_output_display <- tidyr::spread(mean_zone_av_output_display, Zone, Yld)
mean_zone_av_output_display <- mean_zone_av_output_display[c(1,3,4,2)] #record the clms
#mean_zone_av_output_display <- mean_zone_av_output_display[c(1,3,2)] #record the clms

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
write.csv(all_results_1, paste0(outputDir, "/grower_results/results_grower_", 
                              distinct(all_results_1,paddock_ID_Type),
                              "_",
                              input_file))

# the graph 
outputDir
collection
ggsave( filename = 
         paste0(outputDir,  "/png/",
                distinct(all_results_1,paddock_ID_Type),
                "_",
                stringr::str_replace(input_file, ".csv", ""),
          "_collection.png"), device = "png", 
        width = 35, height = 20, units = "cm", collection)



#######################################################################################################################################
################                        Extra analysis for Ricks tables this is added to below t test      ############################
######################################################################################################################################
#######################################################################################################################################
################                       compare high and low rates          ############################
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



function_grand_mean_std_error <- function(df, comparison){

clm <- paste0("comparison_", comparison)
comparison_grand_mean <- paste0("grand_mean_", comparison)
comparison_se <- paste0("se_comp_", comparison)


grand_mean_std_error <- df %>%
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

assign(paste0("grand_mean_std_error_", "high_low"), function_grand_mean_std_error(for_ricks_tables_1,"high_low"))
assign(paste0("grand_mean_std_error_", "high_medium"), function_grand_mean_std_error(for_ricks_tables_1,"high_medium"))
assign(paste0("grand_mean_std_error_", "medium_low"), function_grand_mean_std_error(for_ricks_tables_1,"medium_low"))



grand_mean_se <- left_join(grand_mean_std_error_high_low,grand_mean_std_error_high_medium )
grand_mean_se <- left_join(grand_mean_se,grand_mean_std_error_medium_low )


## I need to generate mean yield value for the zone and Rate

for_ricks_tables_2 <- for_ricks_tables_1 %>% 
  group_by( Zone_ID, Rate, rate_name_order) %>% 
  summarise(zone_yld = mean(YldMassDry, na.rm = TRUE))



for_ricks_tables_wide <- tidyr::pivot_wider(for_ricks_tables_2, 
                                     id_cols = c( Zone_ID),
                                     names_from =rate_name_order,
                                     values_from = zone_yld
)

str(for_ricks_tables_wide)
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
  tidyr::pivot_longer(cols = c("yld_resposne_high_v_low","yld_resposne_high_v_medium", "yld_resposne_medium_v_low"),
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
#rate_order_all <- rate_order_zone_1 
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
                yld_response,P_value,  rounded )

## add in a few clms that help later
for_ricks_tables_summary <- for_ricks_tables_summary %>% 
  mutate(paddock_ID = unique(strips$Paddock_ID),
         Strip_Type = unique(strips$Strip_Type),
         input_file = input_file)


assigned_names1 <- distinct(all_results_1,rate_name_order, .keep_all = TRUE) %>% 
  dplyr::select(rate_name_order, Details)
assigned_names2 <- pivot_wider(assigned_names1,
                               names_from = rate_name_order, 
                               names_prefix = "rate_",
                               values_from = Details)
assigned_names2

for_ricks_tables_summary <- cbind(for_ricks_tables_summary, assigned_names2)
for_ricks_tables_summary

###################################################################################################################################
## what are we saving or have saved 
#for_ricks_tables_summary (saved below)
#all_results_1 (saved in line approx 556 )
#collection (saved in line approx 566 )



#save the output
name <- paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/high_low_comparision/high_low_comp_", 
dplyr::distinct(all_results_1,paddock_ID_Type), ".csv")
name
View(for_ricks_tables_summary)
write.csv(for_ricks_tables_summary, name)



#########################################################################################################
####  ALt GSP analysis ######################################

 assign("strips_alt_analysis", function_1_import_data(input_file))
 assign("strips_alt_analysis", function_2_tidy_clm(strips_alt_analysis))
 
 ### need to add in the zone ID here
 strips_alt_analysis <- left_join(strips_alt_analysis, Rates_labels, by= "Rate")
 strips_alt_analysis <- left_join(strips_alt_analysis, Zone_labels, by= "Zone")
 
 names(strips_alt_analysis)
 ### select the clm
 strips_alt_analysis <- strips_alt_analysis %>% 
   dplyr::select(Zone_ID = Zone_ID.y, SegmentID, YldMassDry, GSP, zone_name, Zone )
 
 ## filter data just the zones and the GSP and Alt GSP strips
 strips_alt_analysis <-  strips_alt_analysis %>% 
   filter(Zone_ID != "NA") %>% 
   filter(GSP != "NA")
 
 
 
 str(strips_alt_analysis)
 strips_alt_analysis <- strips_alt_analysis %>%
   mutate(comparison_GSP_AltGSP = case_when(GSP == "GSP" ~ "GSP_AltGSP",
                                            TRUE ~ "other"))
 
 assign(paste0("grand_mean_std_error_", "GSP_AltGSP"), function_grand_mean_std_error(strips_alt_analysis,"GSP_AltGSP"))
 grand_mean_se_GSP_AltGSP <- grand_mean_std_error_GSP_AltGSP
 
 ## I need to generate mean yield value for the zone and Rate
 
 strips_alt_analysis_2 <- strips_alt_analysis %>% 
   group_by( Zone_ID, GSP) %>% 
   summarise(zone_yld = mean(YldMassDry, na.rm = TRUE))
 
 
 
 strips_alt_analysis_wide <- tidyr::pivot_wider(strips_alt_analysis_2, 
                                             id_cols = c( Zone_ID),
                                             names_from =GSP,
                                             values_from = zone_yld
 )
 
 str(strips_alt_analysis_wide)
 ## differences in yld clms
 strips_alt_analysis_wide <- strips_alt_analysis_wide %>% 
   mutate(GSP_vs_Alt_GSP = GSP - `Alt GSP`)
 str(strips_alt_analysis_wide)
 
 strips_alt_analysis_wide <- ungroup(strips_alt_analysis_wide)
 
 strips_alt_analysis_wide
 grand_mean_se_GSP_AltGSP
 
 
 strips_alt_analysis_wide <- left_join(strips_alt_analysis_wide, grand_mean_se_GSP_AltGSP)
 
 
 
 
 #####
 strips_alt_analysis_summary <- strips_alt_analysis_wide %>%
   mutate(
     yld_resposne_GSP_v_Alt_GSP =  case_when(
       GSP_vs_Alt_GSP > 0 + se_comp_GSP_AltGSP ~ "positive",
       GSP_vs_Alt_GSP < 0 - se_comp_GSP_AltGSP ~ "negative",
       TRUE ~ "no_response"
     )
   )
     
 
 strips_alt_analysis_summary
 
 strips_alt_analysis_summary <- strips_alt_analysis_summary %>% 
   tidyr::pivot_longer(cols = c("yld_resposne_GSP_v_Alt_GSP"),
                       names_to = "comparison",
                       values_to = "yld_response") %>% 
   dplyr::select(Zone_ID, comparison, yld_response, `Alt GSP`, GSP, 
                 GSP_vs_Alt_GSP, 
                 se_comp_GSP_AltGSP) 
 
 strips_alt_analysis_summary
 
 #### t test extra 
 
 names(strips_alt_analysis)
 
 function_paired_ttest_GSP <- function(strips_alt_analysis, zone_x){
   
   #select the zone data and the high vs low rates
   zone_x_GSP_vs_AltGSP <- strips_alt_analysis %>% 
     filter(zone_name == paste0("zone", zone_x)) %>%
     filter(GSP == "GSP" | GSP == "Alt GSP")
   
   #average the yld per segment and GSP
   zone_x_GSP_vs_AltGSP_av <- group_by(zone_x_GSP_vs_AltGSP, SegmentID, GSP, Zone, zone_name ) %>% 
     summarise_all(mean, na.rm= TRUE)
   #ensure that the dataset is duplictaed
   list_SegmentID_values_GSP <- zone_x_GSP_vs_AltGSP_av$SegmentID[duplicated(zone_x_GSP_vs_AltGSP$SegmentID)] #this returns a list of values I want to keep
   zone_x_GSP_vs_AltGSP_av <- zone_x_GSP_vs_AltGSP_av %>% filter(SegmentID %in% list_SegmentID_values_GSP)
   # run paired ttest
   zone_x_GSP_vs_AltGSP_res <- t.test(YldMassDry ~ GSP, data = zone_x_GSP_vs_AltGSP_av, paired = TRUE)
   
   #####test results
   # Report values from the t.test
   zone_x_GSP_vs_AltGSP_res_sig <-
     data.frame(P_value = as.double(zone_x_GSP_vs_AltGSP_res$p.value),
                Mean_diff = (zone_x_GSP_vs_AltGSP_res$estimate)) %>%
     mutate(
       comparison = "GSP_v_AltGSP",
       zone = paste0("zone", zone_x),
       rounded = abs(round(Mean_diff, 2)),
       Significant = case_when(P_value < 0.05 ~ "significant",
                               TRUE ~ "not significant"))
   zone_x_GSP_vs_AltGSP_res_sig 
   
   ###############################################################################################################
   
 }
 assign(paste0("GSP", "zone_", "1"), function_paired_ttest_GSP(strips_alt_analysis, 1))
 assign(paste0("GSP","zone_", "2"), function_paired_ttest_GSP(strips_alt_analysis, 2))
 
 GSP_all <- rbind(GSPzone_1, GSPzone_2)
 #GSP_all <- GSPzone_1
 GSP_all <- left_join(GSP_all, Zone_labels, by=c("zone" = "zone_name"))
 
 str(strips_alt_analysis_summary)
 str(GSP_all)
 
 strips_alt_analysis_summary <- left_join(strips_alt_analysis_summary, GSP_all, by= "Zone_ID")
 names(strips_alt_analysis_summary)
 
 strips_alt_analysis_summary <- strips_alt_analysis_summary %>%
   dplyr::mutate(
   comparison = comparison.y,
     paddock_ID = unique(strips$Paddock_ID),
     Strip_Type = unique(strips$Strip_Type),
     input_file = input_file
   ) %>% 
 dplyr::select(-comparison.x)
 
 ## save output
 View(strips_alt_analysis_summary)
 name_gsp <- paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/GSP/GSP_AltGSP_comp_", 
                dplyr::distinct(all_results_1,paddock_ID_Type), ".csv")
 name_gsp
 write.csv(strips_alt_analysis_summary, name_gsp)

###########################################################################################################################################
##########################################################################################################################################
### Extra analysis for ricks tables GSP vs low high comparision 

str(strips)
GR_vs_low_High_rate <- strips %>% 
  filter(!is.na(zone_name)) %>% 
  dplyr::select(Zone_ID, SegmentID, YldMassDry, Rate, rate_name_order, rate_name, zone_name, Zone)
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

###############################################################################################
### !!!! User input !!!! #####################################################################
###############################################################################################
#how many rates are lower_than_GSP - this is checking how may are lower and how many higher
GR_vs_low_High_rate %>%  group_by(GSP_high_low, Rate) %>% 
  summarise(count= n())


## filter out one rate so we only have gsp rate, lower than and higher than
# try and aim for sensible rates not zero if it can be avoided
# 1/2 the GSP rate and *2 GSP rate
GR_vs_low_High_rate %>%  group_by(GSP_high_low, Rate, Zone_ID, zone_name) %>% 
  summarise(count= n()) %>% 
  filter(GSP_high_low  == "the_GSP" ) %>% 
  mutate(double_GSP_rate = Rate*2,
         half_GPS_rate = Rate*.5)

## filter out one rate so we only have 3
# GR_vs_low_High_rate <- GR_vs_low_High_rate %>%
#   filter(Rate != 60)






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


## I need to generate mean yield value for the zone and Rate
str(GR_vs_low_High_rate)

GR_vs_low_High_rate_2 <- GR_vs_low_High_rate %>% 
  group_by( Zone_ID, GSP_high_low) %>% 
  summarise(zone_yld = mean(YldMassDry, na.rm = TRUE))

str(GR_vs_low_High_rate_2)

GR_vs_low_High_rate_wide <- tidyr::pivot_wider(GR_vs_low_High_rate_2, 
                                               id_cols = c( Zone_ID),
                                               names_from =GSP_high_low,
                                               values_from = zone_yld
)

str(GR_vs_low_High_rate_wide)
## differences in yld clms
GR_vs_low_High_rate_wide <- GR_vs_low_High_rate_wide %>% 
  mutate(GSP_vs_lower = the_GSP   - lower_than_GSP,
         GSP_vs_higher = the_GSP  - higher_than_GSP)
str(GR_vs_low_High_rate_wide)

GR_vs_low_High_rate_wide <- ungroup(GR_vs_low_High_rate_wide)

GR_vs_low_High_rate_wide
grand_mean_GSP_H_L_se


GR_vs_low_High_rate_wide <- left_join(GR_vs_low_High_rate_wide, grand_mean_GSP_H_L_se)

#####
str(GR_vs_low_High_rate_wide)
GR_vs_low_High_rate_summary <- GR_vs_low_High_rate_wide %>%
  mutate(
    yld_resposne_GSP_v_low =  case_when(
      GSP_vs_lower > 0 + se_comp_GSP_low ~ "positive",
      GSP_vs_lower < 0 - se_comp_GSP_low ~ "negative",
      TRUE ~ "no_response"
    ),
     #yld_resposne_GSP_v_high =  "NA")
   yld_resposne_GSP_v_high =  case_when(
   GSP_vs_higher  > 0 +  se_comp_GSP_high ~ "negative",
   GSP_vs_higher  < 0 -  se_comp_GSP_high ~ "positive",
   TRUE ~ "no_response"
   ))
  

str(GR_vs_low_High_rate_summary)

GR_vs_low_High_rate_summary <- GR_vs_low_High_rate_summary %>%
  tidyr::pivot_longer(
    cols = c("yld_resposne_GSP_v_low", "yld_resposne_GSP_v_high"),
    names_to = "comparison",
    values_to = "yld_response"
  ) %>%
  dplyr::select(
    Zone_ID,
    comparison,
    yld_response,
    higher_than_GSP,
    lower_than_GSP,
    the_GSP,
    GSP_vs_lower,
    GSP_vs_higher,
    se_comp_GSP_low,
    se_comp_GSP_high
  ) %>% 
  mutate(
    comparison = case_when(
      comparison == "yld_resposne_GSP_v_low"  ~ "GSP_v_low",
      comparison == "yld_resposne_GSP_v_high" ~ "GSP_v_high"
    ))

#Add the mising ones with NA
# GR_vs_low_High_rate_summary <- GR_vs_low_High_rate_summary %>%
# mutate(higher_than_GSP = NA,
#        GSP_vs_higher = NA)
  
GR_vs_low_High_rate_summary

### Extra t test #######################################################################################################################

#Prep the data making a sub selection of df for each zone and run the paired t test

str(GR_vs_low_High_rate)
unique(GR_vs_low_High_rate$GSP_high_low)

function_paired_ttest_GR_low_high <- function(GR_vs_low_High_rate, zone_x){
  
  #select the zone data and the high vs low rates
  zone_x_GSP_vs_low <- GR_vs_low_High_rate %>% 
    filter(zone_name == paste0("zone", zone_x)) %>%
    filter(GSP_high_low == "the_GSP" | GSP_high_low == "lower_than_GSP")
  
  #average the yld per segment and rate
  zone_x_GSP_vs_low_av <- group_by(zone_x_GSP_vs_low, SegmentID, Rate, Zone, rate_name, zone_name , GSP_high_low) %>% 
    summarise_all(mean, na.rm= TRUE)
  #ensure that the dataset is duplictaed
  list_SegmentID_values_GSP_l <- zone_x_GSP_vs_low_av$SegmentID[duplicated(zone_x_GSP_vs_low$SegmentID)] #this returns a list of values I want to keep
  zone_x_GSP_vs_low_av <- zone_x_GSP_vs_low_av %>% filter(SegmentID %in% list_SegmentID_values_GSP_l)
  # run paired ttest
  zone_x_GSP_vs_low_res <- t.test(YldMassDry ~ GSP_high_low, data = zone_x_GSP_vs_low_av, paired = TRUE)
  
  #####test results
  # Report values from the t.test
  zone_x_GSP_vs_low_res_sig <-
    data.frame(P_value = as.double(zone_x_GSP_vs_low_res$p.value),
               Mean_diff = (zone_x_GSP_vs_low_res$estimate)) %>%
    mutate(
      comparison = "GSP_v_low",
      zone = paste0("zone", zone_x),
      rounded = abs(round(Mean_diff, 2)),
      Significant = case_when(P_value < 0.05 ~ "significant",
                              TRUE ~ "not significant"))
  zone_x_GSP_vs_low_res_sig 
  
  ##########################################################################################################################
  #select the zone data and the GSP vs high rates
   zone_x_GSP_vs_high <- GR_vs_low_High_rate %>%
     filter(zone_name == paste0("zone", zone_x)) %>%
     filter(GSP_high_low == "the_GSP" | GSP_high_low == "higher_than_GSP")

   #average the yld per segment and rate
   zone_x_GSP_vs_high_av <- group_by(zone_x_GSP_vs_high, SegmentID, Rate, Zone, rate_name, zone_name , GSP_high_low) %>%
     summarise_all(mean, na.rm= TRUE)
   #ensure that the dataset is duplictaed
   list_SegmentID_values_GSP_h <- zone_x_GSP_vs_high_av$SegmentID[duplicated(zone_x_GSP_vs_high$SegmentID)] #this returns a list of values I want to keep
   zone_x_GSP_vs_high_av <- zone_x_GSP_vs_high_av %>% filter(SegmentID %in% list_SegmentID_values_GSP_h)
   # run paired ttest
   zone_x_GSP_vs_high_res <- t.test(YldMassDry ~ GSP_high_low, data = zone_x_GSP_vs_high_av, paired = TRUE)

   #####test results
   # Report values from the t.test
   zone_x_GSP_vs_high_res_sig <-
     data.frame(P_value = as.double(zone_x_GSP_vs_high_res$p.value),
                Mean_diff = (zone_x_GSP_vs_high_res$estimate)) %>%
     mutate(
       comparison = "GSP_v_high",
       zone = paste0("zone", zone_x),
       rounded = abs(round(Mean_diff, 2)),
       Significant = case_when(P_value < 0.05 ~ "significant",
                               TRUE ~ "not significant"))
   zone_x_GSP_vs_high_res_sig
  
  
  
  # zone_x_GSP_vs_low_res_sig
  # zone_x_GSP_vs_high_res_sig
  
  
    zone_x_GSP_vs_low_vs_high_res_sig <- rbind(zone_x_GSP_vs_low_res_sig, 
                                               zone_x_GSP_vs_high_res_sig)
  
  #zone_x_GSP_vs_low_vs_high_res_sig <- zone_x_GSP_vs_low_res_sig
  
  return(data.frame(zone_x_GSP_vs_low_vs_high_res_sig))
}
assign(paste0("GSP_low_vs_high", "zone_", "1"), function_paired_ttest_GR_low_high(GR_vs_low_High_rate, 1))
assign(paste0("GSP_low_vs_high","zone_", "2"), function_paired_ttest_GR_low_high(GR_vs_low_High_rate, 2))

GSP_low_vs_highzone_1
GSP_low_vs_highzone_2

GSP_low_vs_high_all <- rbind(GSP_low_vs_highzone_1, GSP_low_vs_highzone_2) 
#GSP_low_vs_high_all <- GSP_low_vs_highzone_1
GSP_low_vs_high_all <- left_join(GSP_low_vs_high_all, Zone_labels, by = c("zone"=  "zone_name"))


## turn GR_vs_low_High_rate_summary to narrow format
str(GR_vs_low_High_rate_summary)
str(GSP_low_vs_high_all)

GR_vs_low_High_rate_summary <- left_join(GR_vs_low_High_rate_summary, GSP_low_vs_high_all, by = c("Zone_ID", "comparison"))

names(GR_vs_low_High_rate_summary)

GR_vs_low_High_rate_summary <- GR_vs_low_High_rate_summary %>%
  dplyr::select(
    Zone_ID,
    Zone,
    comparison,
    yld_response,
    lower_than_GSP,
    higher_than_GSP,
    the_GSP,
    GSP_vs_lower,
    GSP_vs_higher ,
    se_comp_GSP_low,
    se_comp_GSP_high,
    Significant,
    P_value,
    rounded
  )

## add in a few clms that help later
GR_vs_low_High_rate_summary <- GR_vs_low_High_rate_summary %>% 
  mutate(paddock_ID = unique(strips$Paddock_ID),
         Strip_Type = unique(strips$Strip_Type),
         input_file = input_file)


GR_vs_low_High_rate_summary
assigned_names2
GR_vs_low_High_rate_summary <- cbind(GR_vs_low_High_rate_summary,assigned_names2)
GR_vs_low_High_rate_summary

#######################################################
GR_vs_low_High_rate <- data.frame(GR_vs_low_High_rate)

label_GR_v_rates <- GR_vs_low_High_rate %>%  group_by(GSP_high_low,
                                                      Rate,
                                                      #Strip_Rate,
                                                      Zone_ID) %>%
  summarise(count = n())


label_GR_v_rates <- ungroup(label_GR_v_rates) %>% 
  dplyr::select( GSP_high_low, Rate, Zone_ID)

label_GR_v_rates <- tidyr::pivot_wider(
  label_GR_v_rates,
  names_from = GSP_high_low ,
  values_from = Rate
)
label_GR_v_rates <- data.frame(label_GR_v_rates)

##!! make sure this is correct
label_GR_v_rates <-label_GR_v_rates %>% rename(
  higher_than_GSP_label = higher_than_GSP,
  lower_than_GSP_label = lower_than_GSP,
  the_GSP_label = the_GSP)

label_GR_v_rates
GR_vs_low_High_rate_summary <- full_join(GR_vs_low_High_rate_summary, label_GR_v_rates)




## remove rows that are place holders
GR_vs_low_High_rate_summary <- GR_vs_low_High_rate_summary %>% 
  filter(Zone != "NA")

View(GR_vs_low_High_rate_summary)

#save the output
name_CSP_low_high <- paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/GSP_low_high_comparision/GSP_low_high_comp_", 
                            dplyr::distinct(all_results_1,paddock_ID_Type), ".csv")

write.csv(GR_vs_low_High_rate_summary, name_CSP_low_high)

