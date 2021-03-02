

library(tidyverse)
library(readxl)
library(DT)
library(formattable)
library(rgdal)
library(sf)


paddock_done2020_t<- read_excel("C:/Users/ouz001/CSIRO/Ratcliff, Christina (A&F, Waite Campus) - Soil_Testing_Project/paddock_done2020.xlsx", 
                                sheet = "T-test_done")

paddock_done2020_t <- paddock_done2020_t %>% 
  dplyr::mutate(paddock5_digits = substr(`Paddock code`, start = 1, stop = 5))

#select orgaiastion

selection_organisation <- "Agrivision"

paddock_done2020 <- 
  filter(paddock_done2020_t, Organisation %in% selection_organisation) %>% 
  distinct(paddock5_digits, .keep_all= TRUE) %>% 
  dplyr::select(paddock = paddock5_digits,
                Organisation,
                Contact,
                Farmer,
                `Paddock tested`,
                'issues/ comments',
                'Rates excluded' = Rates_excluded)  

paddock_done2020_display <- 
  filter(paddock_done2020_t, Organisation %in% selection_organisation) %>% 
  distinct(paddock5_digits, .keep_all= TRUE) %>% 
  dplyr::select(Paddock = paddock5_digits,
                Organisation,
                Contact,
                Farmer,
                `Paddock tested`,
                'issues/ comments')  

# paddock_done2020_display %>% 
#   formattable(align = "l", - 1  )


#######################################################################################################################################################



## set up some folder to look for data:

png_folder <- "W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/png"

Growers_folder <- paste0("W:/value_soil_testing_prj/Yield_data/2020/", 
                         unique(paddock_done2020$Organisation), "/",unique(paddock_done2020$Contact), "/")


Growers_folder <- gsub(" ", "_", Growers_folder)
Growers_folder <- gsub("-", "_", Growers_folder)



#each folder for the sites (4 listed here)
site1 <- paste0((unique(paddock_done2020$Farmer)[1]),"/",
                (unique(paddock_done2020$`Paddock tested`)[1]),
                "/output/")

site2 <- paste0((unique(paddock_done2020$Farmer)[2]),"/",
                (unique(paddock_done2020$`Paddock tested`)[2]),
                "/output/")

site3 <- paste0((unique(paddock_done2020$Farmer)[3]),"/",
                (unique(paddock_done2020$`Paddock tested`)[3]),
                "/output/")

site4 <- paste0((unique(paddock_done2020$Farmer)[4]),"/",
                (unique(paddock_done2020$`Paddock tested`)[4]),
                "/output/")

site1 <- gsub(" ", "_", site1)
site1 <- gsub("-", "_", site1)
site2 <- gsub(" ", "_", site2)
site2 <- gsub("-", "_", site2)
site3 <- gsub(" ", "_", site3)
site3 <- gsub("-", "_", site3)
site4 <- gsub(" ", "_", site4)
site4 <- gsub("-", "_", site4)



#Lets look for a yld trace maps

#list files that conatin the word "trace" followed by anything and ending with the word ".png":
yld_trace_site1 <- list.files(path = paste0(Growers_folder, site1),pattern = "trace(.*).png$", ignore.case = TRUE)
yld_trace_site2 <- list.files(path = paste0(Growers_folder, site2),pattern = "trace(.*).png$", ignore.case = TRUE)
yld_trace_site3 <- list.files(path = paste0(Growers_folder, site3),pattern = "trace(.*).png$", ignore.case = TRUE)
yld_trace_site4 <- list.files(path = paste0(Growers_folder, site4),pattern = "trace(.*).png$", ignore.case = TRUE)

#list files that conatin the word "harvest" followed by anything and ending with the word ".png": and excluding pre (2 steps)


harvest_pre_post_site1 <- list.files(path = paste0(Growers_folder, site1),pattern = "harvest(.*).png$", ignore.case = TRUE)
harvest_pre_post_site2 <- list.files(path = paste0(Growers_folder, site2),pattern = "harvest(.*).png$", ignore.case = TRUE)
harvest_pre_post_site3 <- list.files(path = paste0(Growers_folder, site3),pattern = "harvest(.*).png$", ignore.case = TRUE)
harvest_pre_post_site4 <- list.files(path = paste0(Growers_folder, site4),pattern = "harvest(.*).png$", ignore.case = TRUE)

harvest_map_site1 <- grep(harvest_pre_post_site1, pattern = 'pre', invert=TRUE, value=TRUE)
harvest_map_site2 <- grep(harvest_pre_post_site2, pattern = 'pre', invert=TRUE, value=TRUE)
harvest_map_site3 <- grep(harvest_pre_post_site3, pattern = 'pre', invert=TRUE, value=TRUE)
harvest_map_site4 <- grep(harvest_pre_post_site4, pattern = 'pre', invert=TRUE, value=TRUE)



### paddock id details
site_name_1 <- paste0("^",paddock_done2020$paddock[1])
site_name_2 <- paste0("^",paddock_done2020$paddock[2])
site_name_3 <- paste0("^",paddock_done2020$paddock[3])
site_name_4 <- paste0("^",paddock_done2020$paddock[4])




Paddock_selected_site1 <- paddock_done2020$paddock[1]
Paddock_selected_site2 <- paddock_done2020$paddock[2]
Paddock_selected_site3 <- paddock_done2020$paddock[3]
Paddock_selected_site4 <- paddock_done2020$paddock[4]

#######################################################################################################################################################
spatial_data_no_yld <- st_read("W:/value_soil_testing_prj/Yield_data/2020/All_Strips_2020_wgs84.shp")
spatial_data_no_yld_df <- data.frame(spatial_data_no_yld)
spatial_data_no_yld_df <- spatial_data_no_yld_df %>% dplyr::select(-geometry)


#######################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################
fert_applied <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step2_fert_app_select_clm.csv")

check_fert_applied <- fert_applied %>%
  dplyr::filter(
    Paddock_ID == Paddock_selected_site1 |
      Paddock_ID == Paddock_selected_site2 |
      Paddock_ID == Paddock_selected_site3 |
      Paddock_ID == Paddock_selected_site4
  ) %>%
  dplyr::select(Paddock_ID,
                Rate,
                Strip_Rate,
                Strip_Type,
                Total_sum_N_content,
                Total_sum_P_content) %>%
  arrange(Paddock_ID, Rate)


check_fert_applied$Total_sum_N_content <- round(check_fert_applied$Total_sum_N_content, 2)
check_fert_applied$Total_sum_P_content <- round(check_fert_applied$Total_sum_P_content, 2)

check_fert_applied

# check_fert_applied %>% 
#   formattable(align = "l", - 1  )


#######################################################################################################################################################

comments_frm_spatial_data <- spatial_data_no_yld_df %>% 
  dplyr::filter(
    Paddock_ID == Paddock_selected_site1 |
      Paddock_ID == Paddock_selected_site2 |
      Paddock_ID == Paddock_selected_site3 |
      Paddock_ID == Paddock_selected_site4
  ) %>%
  distinct( Comments, .keep_all = TRUE) %>% 
  dplyr::select(Paddock_ID ,Comments )

# comments_frm_spatial_data %>% 
#   formattable(align = "l", - 1  )
comments_frm_spatial_data

#######################################################################################################################################################


GSP_AltGSP_t_test <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/GSP_AltGSP_t_test_merged_3c.csv")
names(GSP_AltGSP_t_test)


GSP_AltGSP_t_test <- GSP_AltGSP_t_test %>% 
  dplyr::filter(
    paddock_ID == Paddock_selected_site1 |
      paddock_ID == Paddock_selected_site2 |
      paddock_ID == Paddock_selected_site3 |
      paddock_ID == Paddock_selected_site4
  ) 

GSP_AltGSP_t_test <- GSP_AltGSP_t_test %>%  dplyr::select(Zone_ID ,yld_response, `mean difference` = rounded, Significant ) 
GSP_AltGSP_t_test
# GSP_AltGSP_t_test %>% 
#   formattable(align = "l", - 1  )



#######################################################################################################################################################
high_low_comp <- read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/hign_low_t_test_merged_3b.csv")



high_low_comp <- high_low_comp %>%
  dplyr::filter(
    paddock_ID == Paddock_selected_site1 |
      paddock_ID == Paddock_selected_site2 |
      paddock_ID == Paddock_selected_site3 |
      paddock_ID == Paddock_selected_site4
  )

high_low_comp_define <-distinct(high_low_comp,paddock_ID, .keep_all = TRUE  ) %>% 
  dplyr::select(paddock_ID, 
                'rate low' = rate_low,
                'rate medium' = rate_medium,
                'rate high' = rate_high,
                'rate very high' = rate_very_high)

# high_low_comp_define %>% 
#   formattable(align = "l", - 1  )
high_low_comp_define
#######################################################################################################################################################

high_low_comp$comparison <-
  factor(
    high_low_comp$comparison,
    levels = c("high_v_low", "high_v_medium", "medium_v_low"),
    labels = c(
      "high fert vs low fert",
      "high fert vs medium fert",
      "medium fert vs low fert"
    )
  )


high_low_plot <-
  ggplot(high_low_comp, mapping = aes(x = yld_response, fill = Significant)) +
  geom_bar() +
  facet_grid(Strip_Type ~ comparison)+ 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_x_discrete(
    labels = c(
      "positive" = "positive",
      "no_response" = "no response",
      "negative" = "negative"
    )
  ) +
  labs(
    title = "Comparison of yield response in strips with different fertiliser rate",
    subtitle = "The yield response is classed as positive when higher rates of fertiliser result in higher yields",
    y = "Count of paddocks in yield response class",
    x = "yield response"
  )
high_low_plot
#######################################################################################################################################################


rates_exluded <- paddock_done2020 %>% 
  distinct(paddock, .keep_all= TRUE) %>% 
  dplyr::select(paddock ,
                `Paddock tested`,
                'issues/ comments',
                'Rates excluded' = `Rates excluded`)



# rates_exluded %>% 
#   formattable(align = "l", - 1  )
rates_exluded

#######################################################################################################################################################

GR_comparison <-  read.csv("W:/value_soil_testing_prj/Yield_data/2020/processing/r_outputs/merged_comparision_output/GSP_low_high_comparision_t_test_merged_3d.csv")
GR_comparison <- GR_comparison %>%
  dplyr::filter(
    paddock_ID == Paddock_selected_site1 |
      paddock_ID == Paddock_selected_site2 |
      paddock_ID == Paddock_selected_site3 |
      paddock_ID == Paddock_selected_site4
  )

GR_comparison_plot <-
  ggplot(GR_comparison, mapping = aes(x = yld_response, fill = Significant)) +
  geom_bar() +
  facet_grid(Strip_Type ~ comparison)+ 
  #labeller(Strip_Type = label_wrap_gen(width = 16 ),
  #         comparison = label_wrap_gen(width = 16))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_x_discrete(
    labels = c(
      "positive" = "positive",
      "no_response" = "no response",
      "negative" = "negative"
    )
  ) +
  labs(
    title = "Comparison of yield response for GSP vs higher/lower rates",
    subtitle = "The yield response is classed as positive when higher rates of fertiliser result in higher yields",
    y = "Count of paddocks in yield response class",
    x = "yield response"
  )
GR_comparison_plot


#######################################################################################################################################################




