
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)


library(rgdal)
library(sf)

#install.packages("plotKML")
library(plotKML)
library(knitr)
library(png)

library(readxl)
library(raster)

#####################################################################################################
##############  what was the total amount of N or P applied per strip?  ############################
####################################################################################################
test <- read_csv("W:/value_soil_testing_prj/Yield_data/analysis_strip_trials_April/all_strips_centroid.csv")


## start small with an example just extracting a few rows


test <- test %>%
  dplyr::filter(Paddock_ID == 31336 |
                  Paddock_ID == 51611 |
                  Paddock_ID == 31223 |
                  Paddock_ID == 33111)
test <- test %>% 
  dplyr::select(Paddock_ID:Strip_Type, ID_Rate_GSP_type)
str(test)



################################################################################################
### step 1 Strip trial
### work out how much was applied for the strip trial (not including the top dress or starter - do that later)
### trial rates and products 3 application / types
### using the Strip_Rate split at the "and" "&" "+" - note my example dataset doesnt have & so chcek that it works

## how do I deal with other sep - looks like its working need to have \\ to say a literal + not a regex +
test <- test %>%
  separate(Strip_Rate, c("rate_product_fert1", "rate_product_fert2", "rate_product_fert3"), sep = "(\\+ | and | \\&)", remove=FALSE)


# tidy up cols and remove the brackets
test <- test %>%
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
test <- test %>%
  separate(rate_product_fert1, c("product_fert1", "rate_fert1"), sep = "\\@", remove=FALSE) %>% 
  separate(rate_product_fert2, c("product_fert2", "rate_fert2"), sep = "\\@", remove=FALSE) %>%
  separate(rate_product_fert3, c("product_fert3", "rate_fert3"), sep = "\\@", remove=FALSE)
## remove the units

test$rate_fert1 <- casefold(test$rate_fert1, upper=FALSE)
test$rate_fert2 <- casefold(test$rate_fert2, upper=FALSE)
test$rate_fert3 <- casefold(test$rate_fert3, upper=FALSE)

test <- test %>%
  dplyr::mutate(rate_fert1 = str_replace(rate_fert1, "kg/ha", ""),
                rate_fert1 = str_replace(rate_fert1, "l/ha", ""),
                rate_fert2 = str_replace(rate_fert2, "kg/ha", ""),
                rate_fert2 = str_replace(rate_fert2, "l/ha", ""),
                rate_fert3 = str_replace(rate_fert3, "kg/ha", ""),
                rate_fert3 = str_replace(rate_fert3, "l/ha", "")
  )


#make the rate numeric
test$rate_fert1 <- as.numeric(test$rate_fert1)
test$rate_fert2 <- as.numeric(test$rate_fert2)
test$rate_fert3 <- as.numeric(test$rate_fert3)


## now tidy the clm up - remove the ones I don't want.

test <- test %>% 
  dplyr::select(-rate_product_fert1,
                -rate_product_fert2,
                -rate_product_fert3)


################################################################################################
### step 2 starter 

### trial rates and products 3 application / types
### using the Strip_Rate split at the "and" "&" "+" - note my example dataset doesnt have & so chcek that it works

## how do I deal with other sep - looks like its working need to have \\ to say a literal + not a regex +
str(test)
test <- test %>%
  separate(Start_Fert, c("S_rate_product_fert1", "S_rate_product_fert2", "S_rate_product_fert3"), sep = "(\\+ | and | \\&)", remove=FALSE)


# tidy up cols and remove the brackets
test <- test %>%
  dplyr::mutate(S_rate_product_fert1 = str_replace(S_rate_product_fert1, "\\(", ""),
                S_rate_product_fert1 = str_replace(S_rate_product_fert1, "\\)", ""),
                S_rate_product_fert2 = str_replace(S_rate_product_fert2, "\\(", ""),
                S_rate_product_fert2 = str_replace(S_rate_product_fert2, "\\)", ""),
                S_rate_product_fert3 = str_replace(S_rate_product_fert3, "\\(", ""),
                S_rate_product_fert3 = str_replace(S_rate_product_fert3, "\\)", "")
  )
#################################################################################################
#### rate_product_fert1 fet2 and fert3

## now split the rate_product1 into two clm rate_fert1 and product_fert1
test <- test %>%
  separate(S_rate_product_fert1, c("S_product_fert1", "S_rate_fert1"), sep = "\\@", remove=FALSE) %>% 
  separate(S_rate_product_fert2, c("S_product_fert2", "S_rate_fert2"), sep = "\\@", remove=FALSE) %>%
  separate(S_rate_product_fert3, c("S_product_fert3", "S_rate_fert3"), sep = "\\@", remove=FALSE)
## remove the units
test$S_rate_fert1 <- casefold(test$S_rate_fert1, upper=FALSE)
test$S_rate_fert2 <- casefold(test$S_rate_fert2, upper=FALSE)
test$vrate_fert3 <- casefold(test$S_rate_fert3, upper=FALSE)

test <- test %>%
  dplyr::mutate(S_rate_fert1 = str_replace(S_rate_fert1, "kg/ha", ""),
                S_rate_fert1 = str_replace(S_rate_fert1, "l/ha", ""),
                S_rate_fert2 = str_replace(S_rate_fert2, "kg/ha", ""),
                S_rate_fert2 = str_replace(S_rate_fert2, "l/ha", ""),
                S_rate_fert3 = str_replace(S_rate_fert3, "kg/ha", ""),
                S_rate_fert3 = str_replace(S_rate_fert3, "l/ha", "")
  )


#make the rate numeric
test$S_rate_fert1 <- as.numeric(test$S_rate_fert1)
test$S_rate_fert2 <- as.numeric(test$S_rate_fert2)
test$S_rate_fert3 <- as.numeric(test$S_rate_fert3)


## now tidy the clm up - remove the ones I don't want.

test <- test %>% 
  dplyr::select(-S_rate_product_fert1,
                -S_rate_product_fert2,
                -S_rate_product_fert3)

################################################################################################
### step 3 topdress 

### trial rates and products 3 application / types
### using the Strip_Rate split at the "and" "&" "+" - note my example dataset doesnt have & so chcek that it works

## how do I deal with other sep - looks like its working need to have \\ to say a literal + not a regex +
str(test)
test <- test %>%
  separate(Top_Dress, c("TD_rate_product_fert1", "TD_rate_product_fert2", "TD_rate_product_fert3"), sep = "(\\+ | and | \\&)", remove=FALSE)


# tidy up cols and remove the brackets
test <- test %>%
  dplyr::mutate(TD_rate_product_fert1 = str_replace(TD_rate_product_fert1, "\\(", ""),
                TD_rate_product_fert1 = str_replace(TD_rate_product_fert1, "\\)", ""),
                TD_rate_product_fert2 = str_replace(TD_rate_product_fert2, "\\(", ""),
                TD_rate_product_fert2 = str_replace(TD_rate_product_fert2, "\\)", ""),
                TD_rate_product_fert3 = str_replace(TD_rate_product_fert3, "\\(", ""),
                TD_rate_product_fert3 = str_replace(TD_rate_product_fert3, "\\)", "")
  )
#################################################################################################
#### rate_product_fert1 fet2 and fert3

## now split the rate_product1 into two clm rate_fert1 and product_fert1
test <- test %>%
  separate(TD_rate_product_fert1, c("TD_product_fert1", "TD_rate_fert1"), sep = "\\@", remove=FALSE) %>% 
  separate(TD_rate_product_fert2, c("TD_product_fert2", "TD_rate_fert2"), sep = "\\@", remove=FALSE) %>%
  separate(TD_rate_product_fert3, c("TD_product_fert3", "TD_rate_fert3"), sep = "\\@", remove=FALSE)
## remove the units
test$TD_rate_fert1 <- casefold(test$TD_rate_fert1, upper=FALSE)
test$TD_rate_fert2 <- casefold(test$TD_rate_fert2, upper=FALSE)
test$TDrate_fert3 <- casefold(test$TD_rate_fert3, upper=FALSE)

test <- test %>%
  dplyr::mutate(TD_rate_fert1 = str_replace(TD_rate_fert1, "kg/ha", ""),
                TD_rate_fert1 = str_replace(TD_rate_fert1, "l/ha", ""),
                TD_rate_fert2 = str_replace(TD_rate_fert2, "kg/ha", ""),
                TD_rate_fert2 = str_replace(TD_rate_fert2, "l/ha", ""),
                TD_rate_fert3 = str_replace(TD_rate_fert3, "kg/ha", ""),
                TD_rate_fert3 = str_replace(TD_rate_fert3, "l/ha", "")
  )


#make the rate numeric
test$TD_rate_fert1 <- as.numeric(test$TD_rate_fert1)
test$TD_rate_fert2 <- as.numeric(test$TD_rate_fert2)
test$TD_rate_fert3 <- as.numeric(test$TD_rate_fert3)


## now tidy the clm up - remove the ones I don't want.

test <- test %>% 
  dplyr::select(-TD_rate_product_fert1,
                -TD_rate_product_fert2,
                -TD_rate_product_fert3)


### note I am having trouble with more text and no @ signs which will be fixed by Christina and moving forward?


# Next step is to assign cost to each product




#### N
testN <- test %>% filter(Strip_Type == "N Strip")
str(testN)

testN$product_fert1 <- casefold(testN$product_fert1, upper=FALSE)
testN$product_fert2 <- casefold(testN$product_fert2, upper=FALSE)
testN$product_fert3 <- casefold(v$product_fert3, upper=FALSE)

testN <- testN %>%
  mutate(
    content_N_fert1 = 
      case_when(
        product_fert1 == "map" ~ 0.1,
        product_fert1 == "prime dsz" ~ 0.16,
        product_fert1 == "dap" ~ 0.18,
        product_fert1 == "urea" ~ 0.46, 
        product_fert1 == "ssp" ~ 0.0, 
        product_fert1 == "granulock" ~ 0.1, 
        product_fert1 == "granulock Z" ~ 0.1, 
        product_fert1 == "map zn" ~ 0.1, 
        product_fert1 == "zincguard d2" ~ 0.164,
        product_fert1 == "SOA" ~ 0.21, 
        product_fert1 == "mesz" ~ 0.12, 
        product_fert1 == "acremax" ~ 0.26, 
        product_fert1 == "27:12:00" ~ 0.27, 
        product_fert1 == "24:16:00" ~ 0.24,
        product_fert1 == "prime zn" ~ 0.14,
        TRUE ~ 0))
testN <- testN %>%
  mutate(
    content_N_fert2 = 
      case_when(
        product_fert2 == "MAP" ~ 0.1,
        product_fert2 == "Prime DSZ" ~ 0.16,
        product_fert2 == "DAP" ~ 0.18,
        product_fert2 == "Urea" ~ 0.46, 
        product_fert2 == "SSP" ~ 0.0, 
        product_fert2 == "Granulock" ~ 0.1, 
        product_fert2 == "Granulock Z" ~ 0.1, 
        product_fert2 == "MAP Zn" ~ 0.1, 
        product_fert2 == "zincguard d2" ~ 0.164,
        product_fert2 == "SOA" ~ 0.21, 
        product_fert2 == "MESZ" ~ 0.12, 
        product_fert2 == "AcreMax" ~ 0.26, 
        product_fert2 == "27:12:00" ~ 0.27, 
        product_fert2 == "24:16:00" ~ 0.24,
        product_fert2 == "Prime Zn" ~ 0.14,
        TRUE ~ 0))
testN <- testN %>%
  mutate(
    content_N_fert3 = 
      case_when(
        product_fert3 == "MAP" ~ 0.1,
        product_fert3 == "Prime DSZ" ~ 0.16,
        product_fert3 == "DAP" ~ 0.18,
        product_fert3 == "Urea" ~ 0.46, 
        product_fert3 == "SSP" ~ 0.0, 
        product_fert3 == "Granulock" ~ 0.1, 
        product_fert3 == "Granulock Z" ~ 0.1, 
        product_fert3 == "MAP Zn" ~ 0.1, 
        product_fert3 == "zincguard d2" ~ 0.164,
        product_fert3 == "SOA" ~ 0.21, 
        product_fert3 == "MESZ" ~ 0.12, 
        product_fert3 == "AcreMax" ~ 0.26, 
        product_fert3 == "27:12:00" ~ 0.27, 
        product_fert3 == "24:16:00" ~ 0.24,
        product_fert3 == "Prime Zn" ~ 0.14,
        TRUE ~ 0))
