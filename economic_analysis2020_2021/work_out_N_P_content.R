
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)


library(readxl)


### 03/02/2021 Therese has suggested that she dosent want the fert applied as topdress or starter to be included in the N and P content calulations.
### see email on the 03/02/2021
## I have hashed out the code that relates to these steps.

#####################################################################################################
##############  what was the total amount of N or P applied per strip?  ############################
####################################################################################################
fert_app <- read_csv("W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/all_strips_centroid_2021-02-02.csv")


## start small with an example just extracting a few rows
 # fert_app <- fert_app %>%
 #   dplyr::filter(Paddock_ID == 31721 |
 #                   Paddock_ID == 31722 |
 #                   Paddock_ID == 52214 |
 #                   Paddock_ID == 52216)
 # fert_app <- fert_app %>%
 #   dplyr::filter(Paddock_ID == 33121 |
 #                   Paddock_ID == 31333 |
 #                   Paddock_ID == 31222 )

fert_app <- fert_app %>% 
  dplyr::select(Paddock_ID:Strip_Type, ID_Rate_GSP_type, av_rain)
#str(fert_app)


### in the fert_app clms Strip_Rate, Start_Fert, Top_Dress I need to remove the * and replace with nothing.
# if I have removed a * I need to make a new clm with comment.
fert_app <- fert_app1 %>%
  dplyr::mutate(
    more_info = case_when(
      str_detect(Strip_Rate, "\\*") == TRUE ~ "looks for comment",
      TRUE ~ "all good"
    ))


fert_app <- fert_app %>%
  dplyr::mutate(
    Start_Fert = str_replace(Start_Fert, "\\*", ""),
    Strip_Rate = str_replace(Strip_Rate, "\\*", ""),
    Top_Dress = str_replace(Top_Dress, "\\*", "")
  )



################################################################################################
### step 1 Strip trial
### work out how much was applied for the strip trial (not including the top dress or starter - do that later)
### trial rates and products 3 application / types
### using the Strip_Rate split at the "and" "&" "+" - note my example dataset doesnt have & so chcek that it works

## how do I deal with other sep - looks like its working need to have \\ to say a literal + not a regex +
fert_app <- fert_app %>%
  separate(Strip_Rate, c("rate_product_fert1", "rate_product_fert2", "rate_product_fert3"), sep = "(\\+ | and | \\&)", remove=FALSE)


# tidy up cols and remove the brackets
fert_app <- fert_app %>%
  dplyr::mutate(rate_product_fert1 = str_replace(rate_product_fert1, "\\(", ""),
                rate_product_fert1 = str_replace(rate_product_fert1, "\\)", ""),
                rate_product_fert2 = str_replace(rate_product_fert2, "\\(", ""),
                rate_product_fert2 = str_replace(rate_product_fert2, "\\)", ""),
                rate_product_fert3 = str_replace(rate_product_fert3, "\\(", ""),
                rate_product_fert3 = str_replace(rate_product_fert3, "\\)", "")
  )
#################################################################################################
#### rate_product_fert1 fet2 and fert3

## now split the rate_product1 into two clm rate_fert1 and product_fert1
fert_app <- fert_app %>%
  separate(rate_product_fert1, c("product_fert1", "rate_fert1"), sep = "\\@", remove=FALSE) %>% 
  separate(rate_product_fert2, c("product_fert2", "rate_fert2"), sep = "\\@", remove=FALSE) %>%
  separate(rate_product_fert3, c("product_fert3", "rate_fert3"), sep = "\\@", remove=FALSE)
## remove the units

fert_app$product_fert1 <- casefold(fert_app$product_fert1, upper=FALSE)
fert_app$product_fert2 <- casefold(fert_app$product_fert2, upper=FALSE)
fert_app$product_fert3 <- casefold(fert_app$product_fert3, upper=FALSE)

fert_app <- fert_app %>%
  dplyr::mutate(
    rate_fert1 = str_replace(rate_fert1, "kg/ha", ""),
    rate_fert1 = str_replace(rate_fert1, "l/ha", ""),
    rate_fert1 = str_replace(rate_fert1, "L/ha", ""),
    rate_fert2 = str_replace(rate_fert2, "kg/ha", ""),
    rate_fert2 = str_replace(rate_fert2, "l/ha", ""),
    rate_fert2 = str_replace(rate_fert2, "L/ha", ""),
    rate_fert3 = str_replace(rate_fert3, "kg/ha", ""),
    rate_fert3 = str_replace(rate_fert3, "l/ha", ""),
    rate_fert3 = str_replace(rate_fert3, "L/ha", "")
  
  )


#make the rate numeric
fert_app$rate_fert1 <- as.numeric(fert_app$rate_fert1)
fert_app$rate_fert2 <- as.numeric(fert_app$rate_fert2)
fert_app$rate_fert3 <- as.numeric(fert_app$rate_fert3)


## now tidy the clm up - remove the ones I don't want.

fert_app <- fert_app %>% 
  dplyr::select(-rate_product_fert1,
                -rate_product_fert2,
                -rate_product_fert3)


################################################################################################
######## Therese has suggested we dont need this step ##########################################


### step 2 starter 

### trial rates and products 3 application / types
### using the Strip_Rate split at the "and" "&" "+" - note my example dataset doesnt have & so chcek that it works

## how do I deal with other sep - looks like its working need to have \\ to say a literal + not a regex +
# str(fert_app)
# fert_app <- fert_app %>%
#   separate(Start_Fert, c("S_rate_product_fert1", "S_rate_product_fert2", "S_rate_product_fert3"), sep = "(\\+ | and | \\&)", remove=FALSE)
# 
# 
# # tidy up cols and remove the brackets
# fert_app <- fert_app %>%
#   dplyr::mutate(S_rate_product_fert1 = str_replace(S_rate_product_fert1, "\\(", ""),
#                 S_rate_product_fert1 = str_replace(S_rate_product_fert1, "\\)", ""),
#                 S_rate_product_fert2 = str_replace(S_rate_product_fert2, "\\(", ""),
#                 S_rate_product_fert2 = str_replace(S_rate_product_fert2, "\\)", ""),
#                 S_rate_product_fert3 = str_replace(S_rate_product_fert3, "\\(", ""),
#                 S_rate_product_fert3 = str_replace(S_rate_product_fert3, "\\)", "")
#   )
# #################################################################################################
# #### rate_product_fert1 fet2 and fert3
# 
# ## now split the rate_product1 into two clm rate_fert1 and product_fert1
# fert_app <- fert_app %>%
#   separate(S_rate_product_fert1, c("S_product_fert1", "S_rate_fert1"), sep = "\\@", remove=FALSE) %>% 
#   separate(S_rate_product_fert2, c("S_product_fert2", "S_rate_fert2"), sep = "\\@", remove=FALSE) %>%
#   separate(S_rate_product_fert3, c("S_product_fert3", "S_rate_fert3"), sep = "\\@", remove=FALSE)
# ## remove the units
# fert_app$S_product_fert1 <- casefold(fert_app$S_product_fert1, upper=FALSE)
# fert_app$S_product_fert2 <- casefold(fert_app$S_product_fert2, upper=FALSE)
# fert_app$S_product_fert3 <- casefold(fert_app$S_product_fert3, upper=FALSE)
# 
# fert_app <- fert_app %>%
#   dplyr::mutate(S_rate_fert1 = str_replace(S_rate_fert1, "kg/ha", ""),
#                 S_rate_fert1 = str_replace(S_rate_fert1, "l/ha", ""),
#                 S_rate_fert1 = str_replace(S_rate_fert1, "L/ha", ""),
#                 S_rate_fert2 = str_replace(S_rate_fert2, "kg/ha", ""),
#                 S_rate_fert2 = str_replace(S_rate_fert2, "l/ha", ""),
#                 S_rate_fert2 = str_replace(S_rate_fert2, "L/ha", ""),
#                 S_rate_fert3 = str_replace(S_rate_fert3, "kg/ha", ""),
#                 S_rate_fert3 = str_replace(S_rate_fert3, "l/ha", ""),
#                 S_rate_fert3 = str_replace(S_rate_fert3, "L/ha", "")
#   )
# 
# 
# #make the rate numeric
# fert_app$S_rate_fert1 <- as.numeric(fert_app$S_rate_fert1)
# fert_app$S_rate_fert2 <- as.numeric(fert_app$S_rate_fert2)
# fert_app$S_rate_fert3 <- as.numeric(fert_app$S_rate_fert3)
# 
# 
# ## now tidy the clm up - remove the ones I don't want.
# 
# fert_app <- fert_app %>% 
#   dplyr::select(-S_rate_product_fert1,
#                 -S_rate_product_fert2,
#                 -S_rate_product_fert3)
# 
# ################################################################################################
# ### step 3 topdress 
# 
# ### trial rates and products 3 application / types
# ### using the Strip_Rate split at the "and" "&" "+" - note my example dataset doesnt have & so chcek that it works
# 
# ## how do I deal with other sep - looks like its working need to have \\ to say a literal + not a regex +
# str(fert_app)
# fert_app <- fert_app %>%
#   separate(Top_Dress, c("TD_rate_product_fert1", "TD_rate_product_fert2", "TD_rate_product_fert3"), sep = "(\\+ | and | \\&)", remove=FALSE)
# 
# 
# # tidy up cols and remove the brackets
# fert_app <- fert_app %>%
#   dplyr::mutate(TD_rate_product_fert1 = str_replace(TD_rate_product_fert1, "\\(", ""),
#                 TD_rate_product_fert1 = str_replace(TD_rate_product_fert1, "\\)", ""),
#                 TD_rate_product_fert2 = str_replace(TD_rate_product_fert2, "\\(", ""),
#                 TD_rate_product_fert2 = str_replace(TD_rate_product_fert2, "\\)", ""),
#                 TD_rate_product_fert3 = str_replace(TD_rate_product_fert3, "\\(", ""),
#                 TD_rate_product_fert3 = str_replace(TD_rate_product_fert3, "\\)", "")
#   )
# #################################################################################################
# #### rate_product_fert1 fet2 and fert3
# 
# ## now split the rate_product1 into two clm rate_fert1 and product_fert1
# fert_app <- fert_app %>%
#   separate(TD_rate_product_fert1, c("TD_product_fert1", "TD_rate_fert1"), sep = "\\@", remove=FALSE) %>% 
#   separate(TD_rate_product_fert2, c("TD_product_fert2", "TD_rate_fert2"), sep = "\\@", remove=FALSE) %>%
#   separate(TD_rate_product_fert3, c("TD_product_fert3", "TD_rate_fert3"), sep = "\\@", remove=FALSE)
# ## remove the units
# fert_app$TD_product_fert1 <- casefold(fert_app$TD_product_fert1, upper=FALSE)
# fert_app$TD_product_fert2 <- casefold(fert_app$TD_product_fert2, upper=FALSE)
# fert_app$TD_product_fert3 <- casefold(fert_app$TD_product_fert3, upper=FALSE)
# 
# fert_app <- fert_app %>%
#   dplyr::mutate(TD_rate_fert1 = str_replace(TD_rate_fert1, "kg/ha", ""),
#                 TD_rate_fert1 = str_replace(TD_rate_fert1, "l/ha", ""),
#                 TD_rate_fert1 = str_replace(TD_rate_fert1, "L/ha", ""),
#                 TD_rate_fert2 = str_replace(TD_rate_fert2, "kg/ha", ""),
#                 TD_rate_fert2 = str_replace(TD_rate_fert2, "l/ha", ""),
#                 TD_rate_fert2 = str_replace(TD_rate_fert2, "L/ha", ""),
#                 TD_rate_fert3 = str_replace(TD_rate_fert3, "kg/ha", ""),
#                 TD_rate_fert3 = str_replace(TD_rate_fert3, "l/ha", ""),
#                 TD_rate_fert3 = str_replace(TD_rate_fert3, "L/ha", "")
#                 
#   )
# 
# 
# #make the rate numeric
# fert_app$TD_rate_fert1 <- as.numeric(fert_app$TD_rate_fert1)
# fert_app$TD_rate_fert2 <- as.numeric(fert_app$TD_rate_fert2)
# fert_app$TD_rate_fert3 <- as.numeric(fert_app$TD_rate_fert3)
# 
# 
# ## now tidy the clm up - remove the ones I don't want.
# 
# fert_app <- fert_app %>% 
#   dplyr::select(-TD_rate_product_fert1,
#                 -TD_rate_product_fert2,
#                 -TD_rate_product_fert3)
# 
# ## remove the leading and trailing white spaces  (this will make some names a bit odd looking)
# fert_app <- fert_app %>%
#   mutate(product_fert1  = str_trim(product_fert1, side = c("both", "left", "right")),
#          product_fert2  = str_trim(product_fert2, side = c("both", "left", "right")),
#          product_fert3  = str_trim(product_fert3, side = c("both", "left", "right")),
#          
#          S_product_fert1  = str_trim(S_product_fert1, side = c("both", "left", "right")),
#          S_product_fert2  = str_trim(S_product_fert2, side = c("both", "left", "right")),
#          S_product_fert3  = str_trim(S_product_fert3, side = c("both", "left", "right")),
#          
#          TD_product_fert1  = str_trim(TD_product_fert1, side = c("both", "left", "right")),
#          TD_product_fert2  = str_trim(TD_product_fert2, side = c("both", "left", "right")),
#          TD_product_fert3  = str_trim(TD_product_fert3, side = c("both", "left", "right")),
#          
#   )
####################################################################################################
### note I am having trouble with more text and no @ signs which will be fixed by Christina and moving forward?

####################################################################################################
###### Assign cost to each product
##### split the df into N and P trials
####################################################################################################
############   N      ##############################################################################

### content of N per product for trial
fert_app <- fert_app %>%
  mutate(
    content_N_fert1 = 
      case_when(
        product_fert1 == "map" ~ 0.1,
        product_fert1 == "prime dsz" ~ 0.16,
        product_fert1 == "dap" ~ 0.18,
        product_fert1 == "urea" ~ 0.46, 
        product_fert1 == "ssp" ~ 0.0, 
        product_fert1 == "granulock" ~ 0.1, 
        product_fert1 == "granulock z" ~ 0.1, 
        product_fert1 == "map zn" ~ 0.1, 
        product_fert1 == "zincguardd2" ~ 0.164,
        product_fert1 == "SOA" ~ 0.21, 
        product_fert1 == "mesz" ~ 0.12, 
        product_fert1 == "acremax" ~ 0.26, 
        product_fert1 == "27:12:00" ~ 0.27, 
        product_fert1 == "24:16:00" ~ 0.24,
        product_fert1 == "prime zn" ~ 0.14,
        TRUE ~ 0))
fert_app <- fert_app %>%
  mutate(
    content_N_fert2 = 
      case_when(
        product_fert2 == "map" ~ 0.1,
        product_fert2 == "prime dsz" ~ 0.16,
        product_fert2 == "dap" ~ 0.18,
        product_fert2 == "urea" ~ 0.46, 
        product_fert2 == "ssp" ~ 0.0, 
        product_fert2 == "granulock" ~ 0.1, 
        product_fert2 == "granulock z" ~ 0.1, 
        product_fert2 == "map zn" ~ 0.1, 
        product_fert2 == "zincguardd2" ~ 0.164,
        product_fert2 == "SOA" ~ 0.21, 
        product_fert2 == "mesz" ~ 0.12, 
        product_fert2 == "acremax" ~ 0.26, 
        product_fert2 == "27:12:00" ~ 0.27, 
        product_fert2 == "24:16:00" ~ 0.24,
        product_fert2 == "prime zn" ~ 0.14,
        TRUE ~ 0))
fert_app <- fert_app %>%
  mutate(
    content_N_fert3 = 
      case_when(
        product_fert3 == "map" ~ 0.1,
        product_fert3 == "prime dsz" ~ 0.16,
        product_fert3 == "dap" ~ 0.18,
        product_fert3 == "urea" ~ 0.46, 
        product_fert3 == "ssp" ~ 0.0, 
        product_fert3 == "granulock" ~ 0.1, 
        product_fert3 == "granulock z" ~ 0.1, 
        product_fert3 == "map zn" ~ 0.1, 
        product_fert3 == "zincguardd2" ~ 0.164,
        product_fert3 == "SOA" ~ 0.21, 
        product_fert3 == "mesz" ~ 0.12, 
        product_fert3 == "acremax" ~ 0.26, 
        product_fert3 == "27:12:00" ~ 0.27, 
        product_fert3 == "24:16:00" ~ 0.24,
        product_fert3 == "prime zn" ~ 0.14,
        TRUE ~ 0))
#rate * content for trial

str(fert_app)
fert_app <- fert_app %>% 
  mutate(content_N_fert_rate1 = (content_N_fert1 * rate_fert1),
         content_N_fert_rate2 = (content_N_fert2 * rate_fert2),
         content_N_fert_rate3 = (content_N_fert3 * rate_fert3))

fert_app$content_N_fert_rate1[is.na(fert_app$content_N_fert_rate1)] <- 0 
fert_app$content_N_fert_rate2[is.na(fert_app$content_N_fert_rate2)] <- 0 
fert_app$content_N_fert_rate3[is.na(fert_app$content_N_fert_rate3)] <- 0 

fert_app <- fert_app %>%
  mutate(
    sum_N_content = content_N_fert_rate1 + 
                        content_N_fert_rate2 + 
                        content_N_fert_rate3)



### content of N per product for starter
### Therese has suggested we dont need this step #####

# str(fert_app)
# fert_app <- fert_app %>%
#   mutate(
#     S_content_N_fert1 = 
#       case_when(
#         S_product_fert1 == "map" ~ 0.1,
#         S_product_fert1 == "prime dsz" ~ 0.16,
#         S_product_fert1 == "dap" ~ 0.18,
#         S_product_fert1 == "urea" ~ 0.46, 
#         S_product_fert1 == "ssp" ~ 0.0, 
#         S_product_fert1 == "granulock" ~ 0.1, 
#         S_product_fert1 == "granulock z" ~ 0.1, 
#         S_product_fert1 == "map zn" ~ 0.1, 
#         S_product_fert1 == "zincguardd2" ~ 0.164,
#         S_product_fert1 == "SOA" ~ 0.21, 
#         S_product_fert1 == "mesz" ~ 0.12, 
#         S_product_fert1 == "acremax" ~ 0.26, 
#         S_product_fert1 == "27:12:00" ~ 0.27, 
#         S_product_fert1 == "24:16:00" ~ 0.24,
#         S_product_fert1 == "prime zn" ~ 0.14,
#         TRUE ~ 0))
# fert_app <- fert_app %>%
#   mutate(
#     S_content_N_fert2 = 
#       case_when(
#         S_product_fert2 == "map" ~ 0.1,
#         S_product_fert2 == "prime dsz" ~ 0.16,
#         S_product_fert2 == "dap" ~ 0.18,
#         S_product_fert2 == "urea" ~ 0.46, 
#         S_product_fert2 == "ssp" ~ 0.0, 
#         S_product_fert2 == "granulock" ~ 0.1, 
#         S_product_fert2 == "granulock Z" ~ 0.1, 
#         S_product_fert2 == "map zn" ~ 0.1, 
#         S_product_fert2 == "zincguardd2" ~ 0.164,
#         S_product_fert2 == "SOA" ~ 0.21, 
#         S_product_fert2 == "mesz" ~ 0.12, 
#         S_product_fert2 == "acremax" ~ 0.26, 
#         S_product_fert2 == "27:12:00" ~ 0.27, 
#         S_product_fert2 == "24:16:00" ~ 0.24,
#         S_product_fert2 == "prime zn" ~ 0.14,
#         TRUE ~ 0))
# fert_app <- fert_app %>%
#   mutate(
#     S_content_N_fert3 = 
#       case_when(
#         S_product_fert3 == "map" ~ 0.1,
#         S_product_fert3 == "prime dsz" ~ 0.16,
#         S_product_fert3 == "dap" ~ 0.18,
#         S_product_fert3 == "urea" ~ 0.46, 
#         S_product_fert3 == "ssp" ~ 0.0, 
#         S_product_fert3 == "granulock" ~ 0.1, 
#         S_product_fert3 == "granulock Z" ~ 0.1, 
#         S_product_fert3 == "map zn" ~ 0.1, 
#         S_product_fert3 == "zincguardd2" ~ 0.164,
#         S_product_fert3 == "SOA" ~ 0.21, 
#         S_product_fert3 == "mesz" ~ 0.12, 
#         S_product_fert3 == "acremax" ~ 0.26, 
#         S_product_fert3 == "27:12:00" ~ 0.27, 
#         S_product_fert3 == "24:16:00" ~ 0.24,
#         S_product_fert3 == "prime zn" ~ 0.14,
#         TRUE ~ 0))
# #rate * content for trial
# 
# str(fert_app)
# fert_app <- fert_app %>% 
#   mutate(S_content_N_fert_rate1 = (S_content_N_fert1 * S_rate_fert1),
#          S_content_N_fert_rate2 = (S_content_N_fert2 * S_rate_fert2),
#          S_content_N_fert_rate3 = (S_content_N_fert3 * S_rate_fert3))
# 
# fert_app$S_content_N_fert_rate1[is.na(fert_app$S_content_N_fert_rate1)] <- 0 
# fert_app$S_content_N_fert_rate2[is.na(fert_app$S_content_N_fert_rate2)] <- 0 
# fert_app$S_content_N_fert_rate3[is.na(fert_app$S_content_N_fert_rate3)] <- 0 
# 
# fert_app <- fert_app %>%
#   mutate(
#     S_sum_N_content = S_content_N_fert_rate1 + 
#       S_content_N_fert_rate2 + 
#       S_content_N_fert_rate3)
# 
# 
# 
# ### content of N per product for topdress
# 
# str(fert_app)
# fert_app <- fert_app %>%
#   mutate(
#     TD_content_N_fert1 = 
#       case_when(
#         TD_product_fert1 == "map" ~ 0.1,
#         TD_product_fert1 == "prime dsz" ~ 0.16,
#         TD_product_fert1 == "dap" ~ 0.18,
#         TD_product_fert1 == "urea" ~ 0.46, 
#         TD_product_fert1 == "ssp" ~ 0.0, 
#         TD_product_fert1 == "granulock" ~ 0.1, 
#         TD_product_fert1 == "granulock z" ~ 0.1, 
#         TD_product_fert1 == "map zn" ~ 0.1, 
#         TD_product_fert1 == "zincguardd2" ~ 0.164,
#         TD_product_fert1 == "SOA" ~ 0.21, 
#         TD_product_fert1 == "mesz" ~ 0.12, 
#         TD_product_fert1 == "acremax" ~ 0.26, 
#         TD_product_fert1 == "27:12:00" ~ 0.27, 
#         TD_product_fert1 == "24:16:00" ~ 0.24,
#         TD_product_fert1 == "prime zn" ~ 0.14,
#         TRUE ~ 0))
# 
# fert_app <- fert_app %>%
#   mutate(
#     TD_content_N_fert2 = 
#       case_when(
#         TD_product_fert2 == "map" ~ 0.1,
#         TD_product_fert2 == "prime dsz" ~ 0.16,
#         TD_product_fert2 == "dap" ~ 0.18,
#         TD_product_fert2 == "urea" ~ 0.46, 
#         TD_product_fert2 == "ssp" ~ 0.0, 
#         TD_product_fert2 == "granulock" ~ 0.1, 
#         TD_product_fert2 == "granulock z" ~ 0.1, 
#         TD_product_fert2 == "map zn" ~ 0.1, 
#         TD_product_fert2 == "zincguardd2" ~ 0.164,
#         TD_product_fert2 == "SOA" ~ 0.21, 
#         TD_product_fert2 == "mesz" ~ 0.12, 
#         TD_product_fert2 == "acremax" ~ 0.26, 
#         TD_product_fert2 == "27:12:00" ~ 0.27, 
#         TD_product_fert2 == "24:16:00" ~ 0.24,
#         TD_product_fert2 == "prime zn" ~ 0.14,
#         TRUE ~ 0))
# fert_app <- fert_app %>%
#   mutate(
#     TD_content_N_fert3 = 
#       case_when(
#         TD_product_fert3 == "map" ~ 0.1,
#         TD_product_fert3 == "prime dsz" ~ 0.16,
#         TD_product_fert3 == "dap" ~ 0.18,
#         TD_product_fert3 == "urea" ~ 0.46, 
#         TD_product_fert3 == "ssp" ~ 0.0, 
#         TD_product_fert3 == "granulock" ~ 0.1, 
#         TD_product_fert3 == "granulock z" ~ 0.1, 
#         TD_product_fert3 == "map zn" ~ 0.1, 
#         TD_product_fert3 == "zincguardd2" ~ 0.164,
#         TD_product_fert3 == "SOA" ~ 0.21, 
#         TD_product_fert3 == "mesz" ~ 0.12, 
#         TD_product_fert3 == "acremax" ~ 0.26, 
#         TD_product_fert3 == "27:12:00" ~ 0.27, 
#         TD_product_fert3 == "24:16:00" ~ 0.24,
#         TD_product_fert3 == "primezn" ~ 0.14,
#         TRUE ~ 0))
# #rate * content for trial
# 
# str(fert_app)
# fert_app <- fert_app %>% 
#   mutate(TD_content_N_fert_rate1 = (TD_content_N_fert1 * TD_rate_fert1),
#          TD_content_N_fert_rate2 = (TD_content_N_fert2 * TD_rate_fert2),
#          TD_content_N_fert_rate3 = (TD_content_N_fert3 * TD_rate_fert3))
# 
# fert_app$TD_content_N_fert_rate1[is.na(fert_app$TD_content_N_fert_rate1)] <- 0 
# fert_app$TD_content_N_fert_rate2[is.na(fert_app$TD_content_N_fert_rate2)] <- 0 
# fert_app$TD_content_N_fert_rate3[is.na(fert_app$TD_content_N_fert_rate3)] <- 0 
# 
# fert_app <- fert_app %>%
#   mutate(
#     TD_sum_N_content = TD_content_N_fert_rate1 + 
#       TD_content_N_fert_rate2 + 
#       TD_content_N_fert_rate3)


################################################################################
###############  sum the contnet for all N applications trial + S and TD #######
################################################################################
## Therese has suggested that we dont need the starter and the topdress applications

# fert_app <- fert_app %>%
#   mutate(
#     Total_sum_N_content = sum_N_content +
#       S_sum_N_content +
#       TD_sum_N_content)

fert_app <- fert_app %>%
  mutate(
    Total_sum_N_content = sum_N_content)

####################################################################################################
##################            Assign cost to each product N first        ###########################
####################################################################################################

str(fert_app)

fert_app <- fert_app %>%
  mutate(
    content_P_fert1 = 
      case_when(
        product_fert1 == "map" ~ 0.22,
        product_fert1 == "prime dsz" ~ 0.17,
        product_fert1 == "dap" ~ 0.2,
        product_fert1 == "urea" ~ 0.0, 
        product_fert1 == "ssp" ~ 0.088, 
        product_fert1 == "granulock" ~ 0.175, 
        product_fert1 == "granulock z" ~ 0.218, 
        product_fert1 == "map zn" ~ 0.22, 
        product_fert1 == "zincguardd2" ~ 0.193,
        product_fert1 == "soa" ~ 0.0, 
        product_fert1 == "mesz" ~ 0.175, 
        product_fert1 == "acremax" ~ 0.11,
        product_fert1 == "27:12:00" ~ 0.12, 
        product_fert1 == "24:16:00" ~ 0.16,
        product_fert1 == "prime zn" ~ 0.13,
        TRUE ~ 0))
fert_app <- fert_app %>%
  mutate(
    content_P_fert2 = 
      case_when(
        product_fert2 == "map" ~ 0.22,
        product_fert2 == "prime dsz" ~ 0.17,
        product_fert2 == "dap" ~ 0.2,
        product_fert2 == "urea" ~ 0.0, 
        product_fert2 == "ssp" ~ 0.088, 
        product_fert2 == "granulock" ~ 0.175, 
        product_fert2 == "granulockz" ~ 0.218, 
        product_fert2 == "map zn" ~ 0.22, 
        product_fert2 == "zincguardd2" ~ 0.193,
        product_fert2 == "soa" ~ 0.0, 
        product_fert2 == "mesz" ~ 0.175, 
        product_fert2 == "acremax" ~ 0.11,
        product_fert2 == "27:12:00" ~ 0.12, 
        product_fert2 == "24:16:00" ~ 0.16,
        product_fert2 == "prime zn" ~ 0.13,
        TRUE ~ 0))
fert_app <- fert_app %>%
  mutate(
    content_P_fert3 = 
      case_when(
        product_fert3 == "map" ~ 0.22,
        product_fert3 == "prime dsz" ~ 0.17,
        product_fert3 == "dap" ~ 0.2,
        product_fert3 == "urea" ~ 0.0, 
        product_fert3 == "ssp" ~ 0.088, 
        product_fert3 == "granulock" ~ 0.175, 
        product_fert3 == "granulockz" ~ 0.218, 
        product_fert3 == "map zn" ~ 0.22, 
        product_fert3 == "zincguardd2" ~ 0.193,
        product_fert3 == "soa" ~ 0.0, 
        product_fert3 == "mesz" ~ 0.175, 
        product_fert3 == "acremax" ~ 0.11,
        product_fert3 == "27:12:00" ~ 0.12, 
        product_fert3 == "24:16:00" ~ 0.16,
        product_fert3 == "prime zn" ~ 0.13,
        TRUE ~ 0))

#rate * content for trial

str(fert_app)
fert_app <- fert_app %>% 
  mutate(content_P_fert_rate1 = (content_P_fert1 * rate_fert1),
         content_P_fert_rate2 = (content_P_fert2 * rate_fert2),
         content_P_fert_rate3 = (content_P_fert3 * rate_fert3))

fert_app$content_P_fert_rate1[is.na(fert_app$content_P_fert_rate1)] <- 0 
fert_app$content_P_fert_rate2[is.na(fert_app$content_P_fert_rate2)] <- 0 
fert_app$content_P_fert_rate3[is.na(fert_app$content_P_fert_rate3)] <- 0 

fert_app <- fert_app %>%
  mutate(
    sum_P_content = content_P_fert_rate1 + 
      content_P_fert_rate2 + 
      content_P_fert_rate3)




#################################################################################################
#### starter ####################################################################################
## Therese has suggested we dont need this step ################################################

# fert_app <- fert_app %>%
#   mutate(
#     S_content_P_fert1 = 
#       case_when(
#         S_product_fert1 == "map" ~ 0.22,
#         S_product_fert1 == "prime dsz" ~ 0.17,
#         S_product_fert1 == "dap" ~ 0.2,
#         S_product_fert1 == "urea" ~ 0.0, 
#         S_product_fert1 == "ssp" ~ 0.088, 
#         S_product_fert1 == "granulock" ~ 0.175, 
#         S_product_fert1 == "granulock z" ~ 0.218, 
#         S_product_fert1 == "map zn" ~ 0.22, 
#         S_product_fert1 == "zincguardd2" ~ 0.193,
#         S_product_fert1 == "soa" ~ 0.0, 
#         S_product_fert1 == "mesz" ~ 0.175, 
#         S_product_fert1 == "acremax" ~ 0.11,
#         S_product_fert1 == "27:12:00" ~ 0.12, 
#         S_product_fert1 == "24:16:00" ~ 0.16,
#         S_product_fert1 == "prime zn" ~ 0.13,
#         TRUE ~ 0))
# fert_app <- fert_app %>%
#   mutate(
#     S_content_P_fert2 = 
#       case_when(
#         S_product_fert2 == "map" ~ 0.22,
#         S_product_fert2 == "prime dsz" ~ 0.17,
#         S_product_fert2 == "dap" ~ 0.2,
#         S_product_fert2 == "urea" ~ 0.0, 
#         S_product_fert2 == "ssp" ~ 0.088, 
#         S_product_fert2 == "granulock" ~ 0.175, 
#         S_product_fert2 == "granulock z" ~ 0.218, 
#         S_product_fert2 == "map zn" ~ 0.22, 
#         S_product_fert2 == "zincguardd2" ~ 0.193,
#         S_product_fert2 == "soa" ~ 0.0, 
#         S_product_fert2 == "mesz" ~ 0.175, 
#         S_product_fert2 == "acremax" ~ 0.11,
#         S_product_fert2 == "27:12:00" ~ 0.12, 
#         S_product_fert2 == "24:16:00" ~ 0.16,
#         S_product_fert2 == "prime zn" ~ 0.13,
#         TRUE ~ 0))
# fert_app <- fert_app %>%
#   mutate(
#     S_content_P_fert3 = 
#       case_when(
#         S_product_fert3 == "map" ~ 0.22,
#         S_product_fert3 == "prime dsz" ~ 0.17,
#         S_product_fert3 == "dap" ~ 0.2,
#         S_product_fert3 == "urea" ~ 0.0, 
#         S_product_fert3 == "ssp" ~ 0.088, 
#         S_product_fert3 == "granulock" ~ 0.175, 
#         S_product_fert3 == "granulock z" ~ 0.218, 
#         S_product_fert3 == "map zn" ~ 0.22, 
#         S_product_fert3 == "zincguardd2" ~ 0.193,
#         S_product_fert3 == "soa" ~ 0.0, 
#         S_product_fert3 == "mesz" ~ 0.175, 
#         S_product_fert3 == "acremax" ~ 0.11,
#         S_product_fert3 == "27:12:00" ~ 0.12, 
#         S_product_fert3 == "24:16:00" ~ 0.16,
#         S_product_fert3 == "prime zn" ~ 0.13,
#         TRUE ~ 0))
# 
# str(fert_app)
# fert_app <- fert_app %>% 
#   mutate(S_content_P_fert_rate1 = (S_content_P_fert1 * S_rate_fert1),
#          S_content_P_fert_rate2 = (S_content_P_fert2 * S_rate_fert2),
#          S_content_P_fert_rate3 = (S_content_P_fert3 * S_rate_fert3))
# 
# fert_app$S_content_P_fert_rate1[is.na(fert_app$S_content_P_fert_rate1)] <- 0 
# fert_app$S_content_P_fert_rate2[is.na(fert_app$S_content_P_fert_rate2)] <- 0 
# fert_app$S_content_P_fert_rate3[is.na(fert_app$S_content_P_fert_rate3)] <- 0 
# 
# fert_app <- fert_app %>%
#   mutate(
#     S_sum_P_content = S_content_P_fert_rate1 + 
#       S_content_P_fert_rate2 + 
#       S_content_P_fert_rate3)
# 
# 
# #######################################################################################################
# #############   Topdress
# fert_app <- fert_app %>%
#   mutate(
#     TD_content_P_fert1 = 
#       case_when(
#         TD_product_fert1 == "map" ~ 0.22,
#         TD_product_fert1 == "prime dsz" ~ 0.17,
#         TD_product_fert1 == "dap" ~ 0.2,
#         TD_product_fert1 == "urea" ~ 0.0, 
#         TD_product_fert1 == "ssp" ~ 0.088, 
#         TD_product_fert1 == "granulock" ~ 0.175, 
#         TD_product_fert1 == "granulock z" ~ 0.218, 
#         TD_product_fert1 == "map zn" ~ 0.22, 
#         TD_product_fert1 == "zincguardd2" ~ 0.193,
#         TD_product_fert1 == "soa" ~ 0.0, 
#         TD_product_fert1 == "mesz" ~ 0.175, 
#         TD_product_fert1 == "acremax" ~ 0.11,
#         TD_product_fert1 == "27:12:00" ~ 0.12, 
#         TD_product_fert1 == "24:16:00" ~ 0.16,
#         TD_product_fert1 == "prime zn" ~ 0.13,
#         TRUE ~ 0))
# fert_app <- fert_app %>%
#   mutate(
#     TD_content_P_fert2 = 
#       case_when(
#         TD_product_fert2 == "map" ~ 0.22,
#         TD_product_fert2 == "prime dsz" ~ 0.17,
#         TD_product_fert2 == "dap" ~ 0.2,
#         TD_product_fert2 == "urea" ~ 0.0, 
#         TD_product_fert2 == "ssp" ~ 0.088, 
#         TD_product_fert2 == "granulock" ~ 0.175, 
#         TD_product_fert2 == "granulock z" ~ 0.218, 
#         TD_product_fert2 == "map zn" ~ 0.22, 
#         TD_product_fert2 == "zincguardd2" ~ 0.193,
#         TD_product_fert2 == "soa" ~ 0.0, 
#         TD_product_fert2 == "mesz" ~ 0.175, 
#         TD_product_fert2 == "acremax" ~ 0.11,
#         TD_product_fert2 == "27:12:00" ~ 0.12, 
#         TD_product_fert2 == "24:16:00" ~ 0.16,
#         TD_product_fert2 == "prime zn" ~ 0.13,
#         TRUE ~ 0))
# fert_app <- fert_app %>%
#   mutate(
#     TD_content_P_fert3 = 
#       case_when(
#         TD_product_fert3 == "map" ~ 0.22,
#         TD_product_fert3 == "prime dsz" ~ 0.17,
#         TD_product_fert3 == "dap" ~ 0.2,
#         TD_product_fert3 == "urea" ~ 0.0, 
#         TD_product_fert3 == "ssp" ~ 0.088, 
#         TD_product_fert3 == "granulock" ~ 0.175, 
#         TD_product_fert3 == "granulock z" ~ 0.218, 
#         TD_product_fert3 == "map zn" ~ 0.22, 
#         TD_product_fert3 == "zincguardd2" ~ 0.193,
#         TD_product_fert3 == "soa" ~ 0.0, 
#         TD_product_fert3 == "mesz" ~ 0.175, 
#         TD_product_fert3 == "acremax" ~ 0.11,
#         TD_product_fert3 == "27:12:00" ~ 0.12, 
#         TD_product_fert3 == "24:16:00" ~ 0.16,
#         TD_product_fert3 == "prime zn" ~ 0.13,
#         TRUE ~ 0))
# 
# str(fert_app)
# fert_app <- fert_app %>% 
#   mutate(TD_content_P_fert_rate1 = (TD_content_P_fert1 * TD_rate_fert1),
#          TD_content_P_fert_rate2 = (TD_content_P_fert2 * TD_rate_fert2),
#          TD_content_P_fert_rate3 = (TD_content_P_fert3 * TD_rate_fert3))
# 
# fert_app$TD_content_P_fert_rate1[is.na(fert_app$TD_content_P_fert_rate1)] <- 0 
# fert_app$TD_content_P_fert_rate2[is.na(fert_app$TD_content_P_fert_rate2)] <- 0 
# fert_app$TD_content_P_fert_rate3[is.na(fert_app$TD_content_P_fert_rate3)] <- 0 
# 
# fert_app <- fert_app %>%
#   mutate(
#     TD_sum_P_content = TD_content_P_fert_rate1 + 
#       TD_content_P_fert_rate2 + 
#       TD_content_P_fert_rate3)

################################################################################
###############  sum the contnet for all N applications trial + S and TD #######
################################################################################
## Therese has suggested we dont need this step

# fert_app <- fert_app %>%
#   mutate(
#     Total_sum_P_content = sum_P_content +
#       S_sum_P_content +
#       TD_sum_P_content)

fert_app <- fert_app %>%
  mutate(Total_sum_P_content = sum_P_content)

################################################################################################
##### export working and heaps less than workings
################################################################################################

#getwd()
## all of workings
write.csv(fert_app,
          "W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/economic_analysis2020_2021/fert_app_all_steps.csv")
str(fert_app)


selection_fert_app <- fert_app %>%
  dplyr::select(
    Paddock_ID:Strip_Rate,
    av_rain,
    Strip_Type,
    Start_Fert,
    Top_Dress,
    
    sum_N_content,
    #S_sum_N_content,
    #TD_sum_N_content,
    Total_sum_N_content,
    
    sum_P_content,
    #S_sum_P_content,
    #TD_sum_P_content,
    Total_sum_P_content
    
  )


write.csv(selection_fert_app,
          "W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/economic_analysis2020_2021/fert_app_select_clm.csv")

fert_app$S_product_fert1

Strip_product1 <- data_frame(product =unique(fert_app$product_fert1))
Strip_product2 <- data_frame(product =unique(fert_app$product_fert2))
Strip_product3 <- data_frame(product =unique(fert_app$product_fert3))

Start_product1 <- data_frame(product =unique(fert_app$S_product_fert1))
Start_product2 <- data_frame(product =unique(fert_app$S_product_fert2))
Start_product3 <- data_frame(product =unique(fert_app$S_product_fert3))


Top_dress_product1 <- data_frame(product =unique(fert_app$TD_product_fert1))
Top_dress_product2 <- data_frame(product =unique(fert_app$TD_product_fert2))
Top_dress_product3 <- data_frame(product =unique(fert_app$TD_product_fert3))


#make it into one long list...

whats_used <- rbind(Strip_product1,Strip_product2, Strip_product3,
              Start_product1, Start_product2, Start_product3,
              Top_dress_product1, Top_dress_product2, Top_dress_product3)


whats_used <- distinct(whats_used, product)
