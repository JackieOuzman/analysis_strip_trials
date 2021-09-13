
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

fert_app <- read_csv(paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step1_spatial_data_no_yld_2019_","2021-09-10", ".csv"))

## If run today use this code:
#fert_app <- read_csv(paste0("W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step1_spatial_data_no_yld_",Sys.Date(), ".csv"))

#step1_spatial_data_no_yld_2021-03-19

names(fert_app)
### in the fert_app clms Strip_Rate, Start_Fert, Top_Dress I need to remove the * and replace with nothing.
# if I have removed a * I need to make a new clm with comment.

#################################################################

#1. for strip
fert_app <- fert_app %>%
  dplyr::mutate(
    more_info = case_when(
      str_detect(Strip_Rate, "\\*") == TRUE ~ "looks for comment",
      TRUE ~ "all good"
    ))
# #2. for started
# fert_app <- fert_app %>%
#   dplyr::mutate(
#     more_info = case_when(
#       str_detect(Start_Fert, "\\*") == TRUE ~ "looks for comment",
#       TRUE ~ "all good"
#     ))
# #2. for started
# fert_app <- fert_app %>%
#   dplyr::mutate(
#     more_info = case_when(
#       str_detect(Top_Dress, "\\*") == TRUE ~ "looks for comment",
#       TRUE ~ "all good"
#     ))

###########################################################


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
#1. for strip
fert_app <- fert_app %>%
  separate(Strip_Rate, c("rate_product_fert1", "rate_product_fert2", "rate_product_fert3"), 
           sep = "(\\+ | and | \\&)", remove=FALSE)
#2. for started
fert_app <- fert_app %>%
  separate(Start_Fert, c("rate_product_fert1_start", "rate_product_fert2_start", "rate_product_fert3_start"), 
           sep = "(\\+ | and | \\&)", remove=FALSE)
#3. for topdress
fert_app <- fert_app %>%
  separate(Top_Dress, c("rate_product_fert1_top", "rate_product_fert2_top", "rate_product_fert3_top"), 
           sep = "(\\+ | and | \\&)", remove=FALSE)



# tidy up cols and remove the brackets
#1. for strip
fert_app <- fert_app %>%
  dplyr::mutate(rate_product_fert1 = str_replace(rate_product_fert1, "\\(", ""),
                rate_product_fert1 = str_replace(rate_product_fert1, "\\)", ""),
                rate_product_fert2 = str_replace(rate_product_fert2, "\\(", ""),
                rate_product_fert2 = str_replace(rate_product_fert2, "\\)", ""),
                rate_product_fert3 = str_replace(rate_product_fert3, "\\(", ""),
                rate_product_fert3 = str_replace(rate_product_fert3, "\\)", "")
  )

#2. for starter
fert_app <- fert_app %>%
  dplyr::mutate(rate_product_fert1_start = str_replace(rate_product_fert1_start, "\\(", ""),
                rate_product_fert1_start = str_replace(rate_product_fert1_start, "\\)", ""),
                rate_product_fert2_start = str_replace(rate_product_fert2_start, "\\(", ""),
                rate_product_fert2_start = str_replace(rate_product_fert2_start, "\\)", ""),
                rate_product_fert3_start = str_replace(rate_product_fert3_start, "\\(", ""),
                rate_product_fert3_start = str_replace(rate_product_fert3_start, "\\)", "")
  )
#3. for topdress
fert_app <- fert_app %>%
  dplyr::mutate(rate_product_fert1_top = str_replace(rate_product_fert1_top, "\\(", ""),
                rate_product_fert1_top = str_replace(rate_product_fert1_top, "\\)", ""),
                rate_product_fert2_top = str_replace(rate_product_fert2_top, "\\(", ""),
                rate_product_fert2_top = str_replace(rate_product_fert2_top, "\\)", ""),
                rate_product_fert3_top = str_replace(rate_product_fert3_top, "\\(", ""),
                rate_product_fert3_top = str_replace(rate_product_fert3_top, "\\)", "")
  )



#################################################################################################
#### rate_product_fert1 fet2 and fert3

## now split the rate_product1 into two clm rate_fert1 and product_fert1
fert_app <- fert_app %>%
  separate(rate_product_fert1, c("product_fert1", "rate_fert1"), sep = "\\@", remove=FALSE) %>% 
  separate(rate_product_fert2, c("product_fert2", "rate_fert2"), sep = "\\@", remove=FALSE) %>%
  separate(rate_product_fert3, c("product_fert3", "rate_fert3"), sep = "\\@", remove=FALSE) %>% 
  
  separate(rate_product_fert1_start, c("product_fert1_start", "rate_fert1_start"), sep = "\\@", remove=FALSE) %>% 
  separate(rate_product_fert2_start, c("product_fert2_start", "rate_fert2_start"), sep = "\\@", remove=FALSE) %>% 
  separate(rate_product_fert3_start, c("product_fert3_start", "rate_fert3_start"), sep = "\\@", remove=FALSE) %>% 
  
  separate(rate_product_fert1_top, c("product_fert1_top", "rate_fert1_top"), sep = "\\@", remove=FALSE) %>% 
  separate(rate_product_fert2_top, c("product_fert2_top", "rate_fert2_top"), sep = "\\@", remove=FALSE) %>% 
  separate(rate_product_fert3_top, c("product_fert3_top", "rate_fert3_top"), sep = "\\@", remove=FALSE)




## remove the units
# 1. for strip
fert_app$product_fert1 <- casefold(fert_app$product_fert1, upper=FALSE)
fert_app$product_fert2 <- casefold(fert_app$product_fert2, upper=FALSE)
fert_app$product_fert3 <- casefold(fert_app$product_fert3, upper=FALSE)
# 2. for starter
fert_app$product_fert1_start <- casefold(fert_app$product_fert1_start, upper=FALSE)
fert_app$product_fert2_start <- casefold(fert_app$product_fert2_start, upper=FALSE)
fert_app$product_fert3_start <- casefold(fert_app$product_fert3_start, upper=FALSE)
# 3. for top
fert_app$product_fert1_top <- casefold(fert_app$product_fert1_top, upper=FALSE)
fert_app$product_fert2_top <- casefold(fert_app$product_fert2_top, upper=FALSE)
fert_app$product_fert3_top <- casefold(fert_app$product_fert3_top, upper=FALSE)


# 1. for strip
fert_app <- fert_app %>%
  dplyr::mutate(
    rate_fert1 = str_replace(rate_fert1, "kg/ha", ""),
    rate_fert1 = str_replace(rate_fert1, "kg", ""),
    rate_fert1 = str_replace(rate_fert1, "Kg/ha", ""),
    rate_fert1 = str_replace(rate_fert1, "kg/h", ""),
    rate_fert1 = str_replace(rate_fert1, "l/ha", ""),
    rate_fert1 = str_replace(rate_fert1, "L/ha", ""),
    
    rate_fert2 = str_replace(rate_fert2, "kg/ha", ""),
    rate_fert2 = str_replace(rate_fert2, "kg", ""),
    rate_fert2 = str_replace(rate_fert2, "Kg/ha", ""),
    rate_fert2 = str_replace(rate_fert2, "kg/h", ""),
    rate_fert2 = str_replace(rate_fert2, "l/ha", ""),
    rate_fert2 = str_replace(rate_fert2, "L/ha", ""),
    
    rate_fert3 = str_replace(rate_fert3, "kg/ha", ""),
    rate_fert3 = str_replace(rate_fert3, "kg", ""),
    rate_fert3 = str_replace(rate_fert3, "Kg/ha", ""),
    rate_fert3 = str_replace(rate_fert3, "kg/h", ""),
    rate_fert3 = str_replace(rate_fert3, "l/ha", ""),
    rate_fert3 = str_replace(rate_fert3, "L/ha", "")
  )
# 2. for starter
fert_app <- fert_app %>%
  dplyr::mutate(
    rate_fert1_start = str_replace(rate_fert1_start, "kg/ha", ""),
    rate_fert1_start = str_replace(rate_fert1_start, "kg", ""),
    rate_fert1_start = str_replace(rate_fert1_start, "Kg/ha", ""),
    rate_fert1_start = str_replace(rate_fert1_start, "kg/h", ""),
    rate_fert1_start = str_replace(rate_fert1_start, "l/ha", ""),
    rate_fert1_start = str_replace(rate_fert1_start, "L/ha", ""),
    
    rate_fert2_start = str_replace(rate_fert2_start, "kg/ha", ""),
    rate_fert2_start = str_replace(rate_fert2_start, "kg", ""),
    rate_fert2_start = str_replace(rate_fert2_start, "Kg/ha", ""),
    rate_fert2_start = str_replace(rate_fert2_start, "kg/h", ""),
    rate_fert2_start = str_replace(rate_fert2_start, "l/ha", ""),
    rate_fert2_start = str_replace(rate_fert2_start, "L/ha", ""),
    
    rate_fert3_start = str_replace(rate_fert3_start, "kg/ha", ""),
    rate_fert3_start = str_replace(rate_fert3_start, "kg", ""),
    rate_fert3_start = str_replace(rate_fert3_start, "Kg/ha", ""),
    rate_fert3_start = str_replace(rate_fert3_start, "kg/h", ""),
    rate_fert3_start = str_replace(rate_fert3_start, "l/ha", ""),
    rate_fert3_start = str_replace(rate_fert3_start, "L/ha", "")
  )

# 3. for top
fert_app <- fert_app %>%
  dplyr::mutate(
    rate_fert1_top = str_replace(rate_fert1_top, "kg/ha", ""),
    rate_fert1_top = str_replace(rate_fert1_top, "kg", ""),
    rate_fert1_top = str_replace(rate_fert1_top, "Kg/ha", ""),
    rate_fert1_top = str_replace(rate_fert1_top, "kg/h", ""),
    rate_fert1_top = str_replace(rate_fert1_top, "l/ha", ""),
    rate_fert1_top = str_replace(rate_fert1_top, "L/ha", ""),
    
    rate_fert2_top = str_replace(rate_fert2_top, "kg/ha", ""),
    rate_fert2_top = str_replace(rate_fert2_top, "kg", ""),
    rate_fert2_top = str_replace(rate_fert2_top, "Kg/ha", ""),
    rate_fert2_top = str_replace(rate_fert2_top, "kg/h", ""),
    rate_fert2_top = str_replace(rate_fert2_top, "l/ha", ""),
    rate_fert2_top = str_replace(rate_fert2_top, "L/ha", ""),
    
    rate_fert3_top = str_replace(rate_fert3_top, "kg/ha", ""),
    rate_fert3_top = str_replace(rate_fert3_top, "kg", ""),
    rate_fert3_top = str_replace(rate_fert3_top, "Kg/ha", ""),
    rate_fert3_top = str_replace(rate_fert3_top, "kg/h", ""),
    rate_fert3_top = str_replace(rate_fert3_top, "l/ha", ""),
    rate_fert3_top = str_replace(rate_fert3_top, "L/ha", "")
  )




#make the rate numeric
#1.for strip
fert_app$rate_fert1 <- as.numeric(fert_app$rate_fert1)
fert_app$rate_fert2 <- as.numeric(fert_app$rate_fert2)
fert_app$rate_fert3 <- as.numeric(fert_app$rate_fert3)

#2.for starter
fert_app$rate_fert1_start <- as.numeric(fert_app$rate_fert1_start)
fert_app$rate_fert2_start <- as.numeric(fert_app$rate_fert2_start)
fert_app$rate_fert3_start <- as.numeric(fert_app$rate_fert3_start)

#3.for top
fert_app$rate_fert1_top <- as.numeric(fert_app$rate_fert1_top)
fert_app$rate_fert2_top <- as.numeric(fert_app$rate_fert2_top)
fert_app$rate_fert3_top <- as.numeric(fert_app$rate_fert3_top)


## now tidy the clm up - remove the ones I don't want.
#1. for strip
fert_app <- fert_app %>% 
  dplyr::select(-rate_product_fert1,
                -rate_product_fert2,
                -rate_product_fert3)
#2. for starter
fert_app <- fert_app %>% 
  dplyr::select(-rate_product_fert1_start,
                -rate_product_fert2_start,
                -rate_product_fert3_start)
#3. for top
fert_app <- fert_app %>% 
  dplyr::select(-rate_product_fert1_top,
                -rate_product_fert2_top,
                -rate_product_fert3_top)

## remove the leading and trailing white spaces  (this will make some names a bit odd looking)
#1. for strip
fert_app <- fert_app %>%
  mutate(product_fert1  = str_trim(product_fert1, side = c("both", "left", "right")),
         product_fert2  = str_trim(product_fert2, side = c("both", "left", "right")),
         product_fert3  = str_trim(product_fert3, side = c("both", "left", "right"))
   )

#2. for starter
fert_app <- fert_app %>%
  mutate(product_fert1_start  = str_trim(product_fert1_start, side = c("both", "left", "right")),
         product_fert2_start  = str_trim(product_fert2_start, side = c("both", "left", "right")),
         product_fert3_start  = str_trim(product_fert3_start, side = c("both", "left", "right"))
  )
#3. for top
fert_app <- fert_app %>%
  mutate(product_fert1_top  = str_trim(product_fert1_top, side = c("both", "left", "right")),
         product_fert2_top  = str_trim(product_fert2_top, side = c("both", "left", "right")),
         product_fert3_top  = str_trim(product_fert3_top, side = c("both", "left", "right"))
  )
unique(fert_app$product_fert1)
unique(fert_app$product_fert2)
unique(fert_app$product_fert3)

unique(fert_app$product_fert1_start)
unique(fert_app$product_fert2_start)
unique(fert_app$product_fert2_start)

unique(fert_app$product_fert1_top)
unique(fert_app$product_fert2_top)
unique(fert_app$product_fert3_top)


####################################################################################################
###### Assign cost to each product
##### split the df into N and P trials
####################################################################################################
############   N      ##############################################################################

### content of N per product for trial
#1, for strip
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
        product_fert1 == "granuloc z" ~ 0.1, # I assume this is a typo?
        product_fert1 == "map zn" ~ 0.1, 
        product_fert1 == "zincguard d2" ~ 0.164,
        product_fert1 == "soa" ~ 0.21, 
        product_fert1 == "mesz" ~ 0.12, 
        product_fert1 == "acremax" ~ 0.26, 
        product_fert1 == "27:12:00" ~ 0.27, 
        product_fert1 == "24:16:00" ~ 0.24,
        product_fert1 == "24:16" ~ 0.24,
        product_fert1 == "prime zn" ~ 0.14,
        product_fert1 == "meszn n12; p17.5; k0; zn10" ~ .12,
        product_fert1 == "uan" ~ .32,
        product_fert1 == "prime msz" ~ .1,
        product_fert1 == "map/soa 14:15:0:9" ~ .14,
        product_fert1 == "29:10:s7%:zn(1%)" ~ .29, 
        product_fert1 == "phosphoric acid" ~ 0.0, 
        product_fert1 == "dap / urea 28-13-0-1" ~ 0.28, 
        product_fert1 == "granulock zs 24-11-0-6" ~ 0.11,#was 0.24 Thereses thinks this is error?
        product_fert1 == "soa/urea" ~ 0.385,
        product_fert1 == "urea / map 27:12" ~ 0.27,
        product_fert1 == "urea/soa 60/40" ~ 0.354,
        product_fert1 == "king mix" ~ 0.35,
        
        product_fert1 == "prime dsz/urea 70:30 blend" ~ 0.25, # from TB worksheet
        product_fert1 == "ssp 8.8p,0n,11s,19ca" ~ 0,
        product_fert1 == "dsz 5p,5n" ~ 0.05,
        product_fert1 == "dsz 20p,19n" ~ 0.19,
        product_fert1 == "zinc guard d2" ~ 0.164,
        product_fert1 == "acre max" ~ 0.26,
        product_fert1 == "primezn" ~ 0.14,
        product_fert1 == "map 22:11:0:4" ~ .22,#11P
        product_fert1 == "granulock z22:11:0:04" ~ .22 ,
        product_fert1 == "uan" ~ 0.28,
        product_fert1 == "27:12" ~ 0.27,
        product_fert1 == "urea / soa 50/50" ~ 0.335,
        
        
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
        product_fert2 == "granuloc z" ~ 0.1, # I assume this is a typo?
        product_fert2 == "map zn" ~ 0.1, 
        product_fert2 == "zincguard d2" ~ 0.164,
        product_fert2 == "soa" ~ 0.21, 
        product_fert2 == "mesz" ~ 0.12, 
        product_fert2 == "acremax" ~ 0.26, 
        product_fert2 == "27:12:00" ~ 0.27, 
        product_fert2 == "24:16:00" ~ 0.24,
        product_fert2 == "24:16" ~ 0.24,
        product_fert2 == "prime zn" ~ 0.14,
        product_fert2 == "meszn n12; p17.5; k0; zn10" ~ .12,
        product_fert2 == "uan" ~ .32,
        product_fert2 == "prime msz" ~ .1,
        product_fert2 == "map/soa 14:15:0:9" ~ .14,
        product_fert2 == "29:10:s7%:zn(1%)" ~ .29,
        product_fert2 == "phosphoric acid" ~ 0.0,
        product_fert2 == "dap / urea 28-13-0-1" ~ 0.28,
        product_fert2 == "granulock zs 24-11-0-6" ~ 0.11,#was 0.24 Thereses thinks this is error?
        product_fert2 == "soa/urea" ~ 0.385,
        product_fert2 == "urea / map 27:12" ~ 0.27,
        product_fert2 == "urea/soa 60/40" ~ 0.354,
        product_fert2 == "king mix" ~ 0.35,
        
        product_fert2 == "prime dsz/urea 70:30 blend" ~ 0.25, # from TB worksheet
        product_fert2 == "ssp 8.8p,0n,11s,19ca" ~ 0,
        product_fert2 == "dsz 5p,5n" ~ 0.05,
        product_fert2 == "dsz 20p,19n" ~ 0.19,
        product_fert2 == "zinc guard d2" ~ 0.164,
        product_fert2 == "acre max" ~ 0.26,
        product_fert2 == "primezn" ~ 0.14,
        product_fert2 == "map 22:11:0:4" ~ .22,#11P
        product_fert2 == "granulock z22:11:0:04" ~ .22 ,
        product_fert2 == "uan" ~ 0.28,
        product_fert2 == "27:12" ~ 0.27,
        product_fert2 == "urea / soa 50/50" ~ 0.335,
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
        product_fert3 == "granuloc z" ~ 0.1, # I assume this is a typo?
        product_fert3 == "map zn" ~ 0.1, 
        product_fert3 == "zincguard d2" ~ 0.164,
        product_fert3 == "soa" ~ 0.21, 
        product_fert3 == "mesz" ~ 0.12, 
        product_fert3 == "acremax" ~ 0.26, 
        product_fert3 == "27:12:00" ~ 0.27, 
        product_fert3 == "24:16:00" ~ 0.24,
        product_fert3 == "24:16" ~ 0.24,
        product_fert3 == "prime zn" ~ 0.14,
        product_fert3 == "meszn n12; p17.5; k0; zn10" ~ .12,
        product_fert3 == "uan" ~ .32,
        product_fert3 == "prime msz" ~ .1,
        product_fert3 == "map/soa 14:15:0:9" ~ .14,
        product_fert3 == "29:10:s7%:zn(1%)" ~ .29, 
        product_fert3 == "phosphoric acid" ~ 0.0,
        product_fert3 == "dap / urea 28-13-0-1" ~ 0.28,
        product_fert3 == "granulock zs 24-11-0-6" ~ 0.11,#was 0.24 Thereses thinks this is error?
        product_fert3 == "soa/urea" ~ 0.385,
        product_fert3 == "urea / map 27:12" ~ 0.27,
        product_fert3 == "urea/soa 60/40" ~ 0.354,
        product_fert3 == "king mix" ~ 0.35,
        
        product_fert3 == "prime dsz/urea 70:30 blend" ~ 0.25, # from TB worksheet
        product_fert3 == "ssp 8.8p,0n,11s,19ca" ~ 0,
        product_fert3 == "dsz 5p,5n" ~ 0.05,
        product_fert3 == "dsz 20p,19n" ~ 0.19,
        product_fert3 == "zinc guard d2" ~ 0.164,
        product_fert3 == "acre max" ~ 0.26,
        product_fert3 == "primezn" ~ 0.14,
        product_fert3 == "map 22:11:0:4" ~ .22,#11P
        product_fert3 == "granulock z22:11:0:04" ~ .22 ,
        product_fert3 == "uan" ~ 0.28,
        product_fert3 == "27:12" ~ 0.27,
        product_fert3 == "urea / soa 50/50" ~ 0.335,
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

#2. for starter

fert_app <- fert_app %>%
  mutate(
    content_N_fert1_start = 
      case_when(
        product_fert1_start == "map" ~ 0.1,
        product_fert1_start == "prime dsz" ~ 0.16,
        product_fert1_start == "dap" ~ 0.18,
        product_fert1_start == "urea" ~ 0.46, 
        product_fert1_start == "ssp" ~ 0.0, 
        product_fert1_start == "granulock" ~ 0.1, 
        product_fert1_start == "granulock z" ~ 0.1,
        product_fert1_start == "granuloc z" ~ 0.1, # I assume this is a typo?
        product_fert1_start == "map zn" ~ 0.1, 
        product_fert1_start == "zincguard d2" ~ 0.164,
        product_fert1_start == "soa" ~ 0.21, 
        product_fert1_start == "mesz" ~ 0.12, 
        product_fert1_start == "acremax" ~ 0.26, 
        product_fert1_start == "27:12:00" ~ 0.27, 
        product_fert1_start == "24:16:00" ~ 0.24,
        product_fert1_start == "24:16" ~ 0.24,
        product_fert1_start == "prime zn" ~ 0.14,
        product_fert1_start == "meszn n12; p17.5; k0; zn10" ~ .12,
        product_fert1_start == "uan" ~ .32,
        product_fert1_start == "prime msz" ~ .1,
        product_fert1_start == "map/soa 14:15:0:9" ~ .14,
        product_fert1_start == "29:10:s7%:zn(1%)" ~ .29, 
        product_fert1_start == "phosphoric acid" ~ 0.0, 
        product_fert1_start == "dap / urea 28-13-0-1" ~ 0.28, 
        product_fert1_start == "granulock zs 24-11-0-6" ~ 0.11,#was 0.24 Thereses thinks this is error?
        product_fert1_start == "soa/urea" ~ 0.385,
        product_fert1_start == "urea / map 27:12" ~ 0.27,
        product_fert1_start == "urea/soa 60/40" ~ 0.354,
        product_fert1_start == "king mix" ~ 0.35,
        
        product_fert1_start == "prime dsz/urea 70:30 blend" ~ 0.25, # from TB worksheet
        product_fert1_start == "ssp 8.8p,0n,11s,19ca" ~ 0,
        product_fert1_start == "dsz 5p,5n" ~ 0.05,
        product_fert1_start == "dsz 20p,19n" ~ 0.19,
        product_fert1_start == "zinc guard d2" ~ 0.164,
        product_fert1_start == "acre max" ~ 0.26,
        product_fert1_start == "primezn" ~ 0.14,
        product_fert1_start == "map 22:11:0:4" ~ .22,#11P
        product_fert1_start == "granulock z22:11:0:04" ~ .22 ,
        product_fert1_start == "uan" ~ 0.28,
        product_fert1_start == "27:12" ~ 0.27,
        product_fert1_start == "urea / soa 50/50" ~ 0.335,
        
        TRUE ~ 0))

fert_app %>% filter( Paddock_ID == "31123") %>%
  dplyr::select(content_N_fert1_start)



fert_app <- fert_app %>%
  mutate(
    content_N_fert2_start = 
      case_when(
        product_fert2_start == "map" ~ 0.1,
        product_fert2_start == "prime dsz" ~ 0.16,
        product_fert2_start == "dap" ~ 0.18,
        product_fert2_start == "urea" ~ 0.46, 
        product_fert2_start == "ssp" ~ 0.0, 
        product_fert2_start == "granulock" ~ 0.1, 
        product_fert2_start == "granulock z" ~ 0.1, 
        product_fert2_start == "granuloc z" ~ 0.1, # I assume this is a typo?
        product_fert2_start == "map zn" ~ 0.1, 
        product_fert2_start == "zincguard d2" ~ 0.164,
        product_fert2_start == "soa" ~ 0.21, 
        product_fert2_start == "mesz" ~ 0.12, 
        product_fert2_start == "acremax" ~ 0.26, 
        product_fert2_start == "27:12:00" ~ 0.27, 
        product_fert2_start == "24:16:00" ~ 0.24,
        product_fert2_start == "24:16" ~ 0.24,
        product_fert2_start == "prime zn" ~ 0.14,
        product_fert2_start == "meszn n12; p17.5; k0; zn10" ~ .12,
        product_fert2_start == "uan" ~ .32,
        product_fert2_start == "prime msz" ~ .1,
        product_fert2_start == "map/soa 14:15:0:9" ~ .14,
        product_fert2_start == "29:10:s7%:zn(1%)" ~ .29,
        product_fert2_start == "phosphoric acid" ~ 0.0,
        product_fert2_start == "dap / urea 28-13-0-1" ~ 0.28,
        product_fert2_start == "granulock zs 24-11-0-6" ~ 0.11,#was 0.24 Thereses thinks this is error?
        product_fert2_start == "soa/urea" ~ 0.385,
        product_fert2_start == "urea / map 27:12" ~ 0.27,
        product_fert2_start == "urea/soa 60/40" ~ 0.354,
        product_fert2_start == "king mix" ~ 0.35,
        
        product_fert2_start == "prime dsz/urea 70:30 blend" ~ 0.25, # from TB worksheet
        product_fert2_start == "ssp 8.8p,0n,11s,19ca" ~ 0,
        product_fert2_start == "dsz 5p,5n" ~ 0.05,
        product_fert2_start == "dsz 20p,19n" ~ 0.19,
        product_fert2_start == "zinc guard d2" ~ 0.164,
        product_fert2_start == "acre max" ~ 0.26,
        product_fert2_start == "primezn" ~ 0.14,
        product_fert2_start == "map 22:11:0:4" ~ .22,#11P
        product_fert2_start == "granulock z22:11:0:04" ~ .22 ,
        product_fert2_start == "uan" ~ 0.28,
        product_fert2_start == "27:12" ~ 0.27,
        product_fert2_start == "urea / soa 50/50" ~ 0.335,
        TRUE ~ 0))
fert_app <- fert_app %>%
  mutate(
    content_N_fert3_start = 
      case_when(
        product_fert3_start == "map" ~ 0.1,
        product_fert3_start == "prime dsz" ~ 0.16,
        product_fert3_start == "dap" ~ 0.18,
        product_fert3_start == "urea" ~ 0.46, 
        product_fert3_start == "ssp" ~ 0.0, 
        product_fert3_start == "granulock" ~ 0.1, 
        product_fert3_start == "granulock z" ~ 0.1, 
        product_fert3_start == "granuloc z" ~ 0.1, # I assume this is a typo?
        product_fert3_start == "map zn" ~ 0.1, 
        product_fert3_start == "zincguard d2" ~ 0.164,
        product_fert3_start == "soa" ~ 0.21, 
        product_fert3_start == "mesz" ~ 0.12, 
        product_fert3_start == "acremax" ~ 0.26, 
        product_fert3_start == "27:12:00" ~ 0.27, 
        product_fert3_start == "24:16:00" ~ 0.24,
        product_fert3_start == "24:16" ~ 0.24,
        product_fert3_start == "prime zn" ~ 0.14,
        product_fert3_start == "meszn n12; p17.5; k0; zn10" ~ .12,
        product_fert3_start == "uan" ~ .32,
        product_fert3_start == "prime msz" ~ .1,
        product_fert3_start == "map/soa 14:15:0:9" ~ .14,
        product_fert3_start == "29:10:s7%:zn(1%)" ~ .29, 
        product_fert3_start == "phosphoric acid" ~ 0.0,
        product_fert3_start == "dap / urea 28-13-0-1" ~ 0.28,
        product_fert3_start == "granulock zs 24-11-0-6" ~ 0.11,#was 0.24 Thereses thinks this is error?
        product_fert3_start == "soa/urea" ~ 0.385,
        product_fert3_start == "urea / map 27:12" ~ 0.27,
        product_fert3_start == "urea/soa 60/40" ~ 0.354,
        product_fert3_start == "king mix" ~ 0.35,
        
        product_fert3_start == "prime dsz/urea 70:30 blend" ~ 0.25, # from TB worksheet
        product_fert3_start == "ssp 8.8p,0n,11s,19ca" ~ 0,
        product_fert3_start == "dsz 5p,5n" ~ 0.05,
        product_fert3_start == "dsz 20p,19n" ~ 0.19,
        product_fert3_start == "zinc guard d2" ~ 0.164,
        product_fert3_start == "acre max" ~ 0.26,
        product_fert3_start == "primezn" ~ 0.14,
        product_fert3_start == "map 22:11:0:4" ~ .22,#11P
        product_fert3_start == "granulock z22:11:0:04" ~ .22 ,
        product_fert3_start == "uan" ~ 0.28,
        product_fert3_start == "27:12" ~ 0.27,
        product_fert3_start == "urea / soa 50/50" ~ 0.335,
        TRUE ~ 0))
#rate * content for trial
fert_app %>% filter( Paddock_ID == "31123") %>%
  dplyr::select(content_N_fert1_start, 
                content_N_fert2_start,
                content_N_fert3_start,
                rate_fert1_start,
                rate_fert2_start,
                rate_fert3_start,
                content_N_fert_rate1_start,
                content_N_fert_rate2_start,
                content_N_fert_rate3_start,
                sum_N_content_start) %>% 
  str()

str(fert_app)
fert_app <- fert_app %>% 
  mutate(content_N_fert_rate1_start = (content_N_fert1_start * rate_fert1_start),
         content_N_fert_rate2_start = (content_N_fert2_start * rate_fert2_start),
         content_N_fert_rate3_start = (content_N_fert3_start * rate_fert3_start))

fert_app$content_N_fert_rate1_start[is.na(fert_app$content_N_fert_rate1_start)] <- 0 
fert_app$content_N_fert_rate2_start[is.na(fert_app$content_N_fert_rate2_start)] <- 0 
fert_app$content_N_fert_rate3_start[is.na(fert_app$content_N_fert_rate3_start)] <- 0 



fert_app <- fert_app %>%
    mutate(
    sum_N_content_start = (content_N_fert_rate1_start+
                              content_N_fert_rate2_start+
                              content_N_fert_rate3_start))

      

#3. for top
fert_app <- fert_app %>%
  mutate(
    content_N_fert1_top = 
      case_when(
        product_fert1_top == "map" ~ 0.1,
        product_fert1_top == "prime dsz" ~ 0.16,
        product_fert1_top == "dap" ~ 0.18,
        product_fert1_top == "urea" ~ 0.46, 
        product_fert1_top == "ssp" ~ 0.0, 
        product_fert1_top == "granulock" ~ 0.1, 
        product_fert1_top == "granulock z" ~ 0.1,
        product_fert1_top == "granuloc z" ~ 0.1, # I assume this is a typo?
        product_fert1_top == "map zn" ~ 0.1, 
        product_fert1_top == "zincguard d2" ~ 0.164,
        product_fert1_top == "soa" ~ 0.21, 
        product_fert1_top == "mesz" ~ 0.12, 
        product_fert1_top == "acremax" ~ 0.26, 
        product_fert1_top == "27:12:00" ~ 0.27, 
        product_fert1_top == "24:16:00" ~ 0.24,
        product_fert1_top == "24:16" ~ 0.24,
        product_fert1_top == "prime zn" ~ 0.14,
        product_fert1_top == "meszn n12; p17.5; k0; zn10" ~ .12,
        product_fert1_top == "uan" ~ .32,
        product_fert1_top == "prime msz" ~ .1,
        product_fert1_top == "map/soa 14:15:0:9" ~ .14,
        product_fert1_top == "29:10:s7%:zn(1%)" ~ .29, 
        product_fert1_top == "phosphoric acid" ~ 0.0, 
        product_fert1_top == "dap / urea 28-13-0-1" ~ 0.28, 
        product_fert1_top == "granulock zs 24-11-0-6" ~ 0.11,#was 0.24 Thereses thinks this is error?
        product_fert1_top == "soa/urea" ~ 0.385,
        product_fert1_top == "urea / map 27:12" ~ 0.27,
        product_fert1_top == "urea/soa 60/40" ~ 0.354,
        product_fert1_top == "king mix" ~ 0.35,
        
        product_fert1_top == "prime dsz/urea 70:30 blend" ~ 0.25, # from TB worksheet
        product_fert1_top == "ssp 8.8p,0n,11s,19ca" ~ 0,
        product_fert1_top == "dsz 5p,5n" ~ 0.05,
        product_fert1_top == "dsz 20p,19n" ~ 0.19,
        product_fert1_top == "zinc guard d2" ~ 0.164,
        product_fert1_top == "acre max" ~ 0.26,
        product_fert1_top == "primezn" ~ 0.14,
        product_fert1_top == "map 22:11:0:4" ~ .22,#11P
        product_fert1_top == "granulock z22:11:0:04" ~ .22 ,
        product_fert1_top == "uan" ~ 0.28,
        product_fert1_top == "27:12" ~ 0.27,
        product_fert1_top == "urea / soa 50/50" ~ 0.335,
        TRUE ~ 0))


fert_app <- fert_app %>%
  mutate(
    content_N_fert2_top = 
      case_when(
        product_fert2_top == "map" ~ 0.1,
        product_fert2_top == "prime dsz" ~ 0.16,
        product_fert2_top == "dap" ~ 0.18,
        product_fert2_top == "urea" ~ 0.46, 
        product_fert2_top == "ssp" ~ 0.0, 
        product_fert2_top == "granulock" ~ 0.1, 
        product_fert2_top == "granulock z" ~ 0.1, 
        product_fert2_top == "granuloc z" ~ 0.1, # I assume this is a typo?
        product_fert2_top == "map zn" ~ 0.1, 
        product_fert2_top == "zincguard d2" ~ 0.164,
        product_fert2_top == "soa" ~ 0.21, 
        product_fert2_top == "mesz" ~ 0.12, 
        product_fert2_top == "acremax" ~ 0.26, 
        product_fert2_top == "27:12:00" ~ 0.27, 
        product_fert2_top == "24:16:00" ~ 0.24,
        product_fert2_top == "24:16" ~ 0.24,
        product_fert2_top == "prime zn" ~ 0.14,
        product_fert2_top == "meszn n12; p17.5; k0; zn10" ~ .12,
        product_fert2_top == "uan" ~ .32,
        product_fert2_top == "prime msz" ~ .1,
        product_fert2_top == "map/soa 14:15:0:9" ~ .14,
        product_fert2_top == "29:10:s7%:zn(1%)" ~ .29,
        product_fert2_top == "phosphoric acid" ~ 0.0,
        product_fert2_top == "dap / urea 28-13-0-1" ~ 0.28,
        product_fert2_top == "granulock zs 24-11-0-6" ~ 0.11,#was 0.24 Thereses thinks this is error?
        product_fert2_top == "soa/urea" ~ 0.385,
        product_fert2_top == "urea / map 27:12" ~ 0.27,
        product_fert2_top == "urea/soa 60/40" ~ 0.354,
        product_fert2_top == "king mix" ~ 0.35,
        
        product_fert2_top == "prime dsz/urea 70:30 blend" ~ 0.25, # from TB worksheet
        product_fert2_top == "ssp 8.8p,0n,11s,19ca" ~ 0,
        product_fert2_top == "dsz 5p,5n" ~ 0.05,
        product_fert2_top == "dsz 20p,19n" ~ 0.19,
        product_fert2_top == "zinc guard d2" ~ 0.164,
        product_fert2_top == "acre max" ~ 0.26,
        product_fert2_top == "primezn" ~ 0.14,
        product_fert2_top == "map 22:11:0:4" ~ .22,#11P
        product_fert2_top == "granulock z22:11:0:04" ~ .22 ,
        product_fert2_top == "uan" ~ 0.28,
        product_fert2_top == "27:12" ~ 0.27,
        product_fert2_top == "urea / soa 50/50" ~ 0.335,
        TRUE ~ 0))
fert_app <- fert_app %>%
  mutate(
    content_N_fert3_top = 
      case_when(
        product_fert3_top == "map" ~ 0.1,
        product_fert3_top == "prime dsz" ~ 0.16,
        product_fert3_top == "dap" ~ 0.18,
        product_fert3_top == "urea" ~ 0.46, 
        product_fert3_top == "ssp" ~ 0.0, 
        product_fert3_top == "granulock" ~ 0.1, 
        product_fert3_top == "granulock z" ~ 0.1, 
        product_fert3_top == "granuloc z" ~ 0.1, # I assume this is a typo?
        product_fert3_top == "map zn" ~ 0.1, 
        product_fert3_top == "zincguard d2" ~ 0.164,
        product_fert3_top == "soa" ~ 0.21, 
        product_fert3_top == "mesz" ~ 0.12, 
        product_fert3_top == "acremax" ~ 0.26, 
        product_fert3_top == "27:12:00" ~ 0.27, 
        product_fert3_top == "24:16:00" ~ 0.24,
        product_fert3_top == "24:16" ~ 0.24,
        product_fert3_top == "prime zn" ~ 0.14,
        product_fert3_top == "meszn n12; p17.5; k0; zn10" ~ .12,
        product_fert3_top == "uan" ~ .32,
        product_fert3_top == "prime msz" ~ .1,
        product_fert3_top == "map/soa 14:15:0:9" ~ .14,
        product_fert3_top == "29:10:s7%:zn(1%)" ~ .29, 
        product_fert3_top == "phosphoric acid" ~ 0.0,
        product_fert3_top == "dap / urea 28-13-0-1" ~ 0.28,
        product_fert3_top == "granulock zs 24-11-0-6" ~ 0.11,#was 0.24 Thereses thinks this is error?
        product_fert3_top == "soa/urea" ~ 0.385,
        product_fert3_top == "urea / map 27:12" ~ 0.27,
        product_fert3_top == "urea/soa 60/40" ~ 0.354,
        product_fert3_top == "king mix" ~ 0.35,
        
        product_fert3_top == "prime dsz/urea 70:30 blend" ~ 0.25, # from TB worksheet
        product_fert3_top == "ssp 8.8p,0n,11s,19ca" ~ 0,
        product_fert3_top == "dsz 5p,5n" ~ 0.05,
        product_fert3_top == "dsz 20p,19n" ~ 0.19,
        product_fert3_top == "zinc guard d2" ~ 0.164,
        product_fert3_top == "acre max" ~ 0.26,
        product_fert3_top == "primezn" ~ 0.14,
        product_fert3_top == "map 22:11:0:4" ~ .22,#11P
        product_fert3_top == "granulock z22:11:0:04" ~ .22 ,
        product_fert3_top == "uan" ~ 0.28,
        product_fert3_top == "27:12" ~ 0.27,
        product_fert3_top == "urea / soa 50/50" ~ 0.335,
        TRUE ~ 0))
#rate * content for trial

str(fert_app)
fert_app <- fert_app %>% 
  mutate(content_N_fert_rate1_top = (content_N_fert1_top * rate_fert1_top),
         content_N_fert_rate2_top = (content_N_fert2_top * rate_fert2_top),
         content_N_fert_rate3_top = (content_N_fert3_top * rate_fert3_top))

fert_app$content_N_fert_rate1_top[is.na(fert_app$content_N_fert_rate1_top)] <- 0 
fert_app$content_N_fert_rate2_top[is.na(fert_app$content_N_fert_rate2_top)] <- 0 
fert_app$content_N_fert_rate3_top[is.na(fert_app$content_N_fert_rate3_top)] <- 0 

fert_app <- fert_app %>%
  mutate(
    sum_N_content_top = content_N_fert_rate1_top + 
      content_N_fert_rate2_top + 
      content_N_fert_rate3_top)


################################################################################
###############  sum the contnet for all N applications trial  #######
################################################################################
## Therese has suggested that we dont need the starter and the topdress applications

#1. for strip
fert_app <- fert_app %>%
  mutate(
    Total_sum_N_content_strip = sum_N_content)
#2. for starter
fert_app <- fert_app %>%
  mutate(
    Total_sum_N_content_start = sum_N_content_start)
#3. for top
fert_app <- fert_app %>%
  mutate(
    Total_sum_N_content_top = sum_N_content_top)

fert_app <- fert_app %>%
  mutate(
    Total_sum_N_content = Total_sum_N_content_strip + Total_sum_N_content_start + Total_sum_N_content_top)

str(fert_app)

####################################################################################################
##################            Assign cost to each product P first        ###########################
####################################################################################################

str(fert_app)
#1. for strip
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
        product_fert1 == "granuloc z" ~ 0.218, # I assume this is a typo?
        product_fert1 == "map zn" ~ 0.22, 
        product_fert1 == "zincguard d2" ~ 0.193,
        product_fert1 == "soa" ~ 0.0, 
        product_fert1 == "mesz" ~ 0.175, 
        product_fert1 == "acremax" ~ 0.11,
        product_fert1 == "27:12:00" ~ 0.12, 
        product_fert1 == "24:16:00" ~ 0.16,
        product_fert1 == "24:16" ~ 0.16,
        product_fert1 == "prime zn" ~ 0.13,
        product_fert1 == "meszn n12; p17.5; k0; zn10" ~ .175,
        product_fert1 == "prime msz" ~ .18,
        product_fert1 == "map/soa 14:15:0:9" ~ .15,
        product_fert1 == "29:10:s7%:zn(1%)" ~ .10,
        product_fert1 == "phosphoric acid" ~ 1.0,
        product_fert1 == "dap / urea 28-13-0-1" ~ 0.13,
        product_fert1 == "granulock zs 24-11-0-6" ~ 0.24,# therese think this is an error0.24
        product_fert1 == "urea / map 27:12" ~ 0.12,
        product_fert1 == "king mix" ~ 0.07,
        
        product_fert1 == "prime dsz/urea 70:30 blend" ~ 0.119, # from TB worksheet
        product_fert1 == "ssp 8.8p,0n,11s,19ca" ~ 0.088,
        product_fert1 == "dsz 5p,5n" ~ 0.05,
        product_fert1 == "dsz 20p,19n" ~ 0.2,
        product_fert1 == "zinc guard d2" ~ 0.193,
        product_fert1 == "acre max" ~ 0.11,
        product_fert1 == "primezn" ~ 0.13,
        product_fert1 == "map 22:11:0:4" ~ 0.11,
        product_fert1 == "granulock z22:11:0:04" ~ .11 ,
        product_fert1 == "uan" ~ 0.0,
        product_fert1 == "27:12" ~ 0.12,
        product_fert1 == "urea / soa 50/50" ~ 0.0,
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
        product_fert2 == "granuloc z" ~ 0.218, # I assume this is a typo?
        product_fert2 == "map zn" ~ 0.22, 
        product_fert2 == "zincguard d2" ~ 0.193,
        product_fert2 == "soa" ~ 0.0, 
        product_fert2 == "mesz" ~ 0.175, 
        product_fert2 == "acremax" ~ 0.11,
        product_fert2 == "27:12:00" ~ 0.12, 
        product_fert2 == "24:16:00" ~ 0.16,
        product_fert2 == "24:16" ~ 0.16,
        product_fert2 == "prime zn" ~ 0.13,
        product_fert2 == "meszn n12; p17.5; k0; zn10" ~ .175,
        product_fert2 == "prime msz" ~ .18,
        product_fert2 == "map/soa 14:15:0:9" ~ .15,
        product_fert2 == "29:10:s7%:zn(1%)" ~ .10,
        product_fert2 == "phosphoric acid" ~ 1.0,
        product_fert2 == "dap / urea 28-13-0-1" ~ 0.13,
        product_fert2 == "granulock zs 24-11-0-6" ~ 0.24,# therese think this is an error0.24
        product_fert2 == "urea / map 27:12" ~ 0.12,
        product_fert2 == "king mix" ~ 0.07,
        
        product_fert2 == "prime dsz/urea 70:30 blend" ~ 0.119, # from TB worksheet
        product_fert2 == "ssp 8.8p,0n,11s,19ca" ~ 0.088,
        product_fert2 == "dsz 5p,5n" ~ 0.05,
        product_fert2 == "dsz 20p,19n" ~ 0.2,
        product_fert2 == "zinc guard d2" ~ 0.193,
        product_fert2 == "acre max" ~ 0.11,
        product_fert2 == "primezn" ~ 0.13,
        product_fert2 == "map 22:11:0:4" ~ 0.11,
        product_fert2 == "granulock z22:11:0:04" ~ .11 ,
        product_fert2 == "uan" ~ 0.0,
        product_fert2 == "27:12" ~ 0.12,
        product_fert2 == "urea / soa 50/50" ~ 0.0,
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
        product_fert3 == "granuloc z" ~ 0.218, # I assume this is a typo?
        product_fert3 == "map zn" ~ 0.22, 
        product_fert3 == "zincguard d2" ~ 0.193,
        product_fert3 == "soa" ~ 0.0, 
        product_fert3 == "mesz" ~ 0.175, 
        product_fert3 == "acremax" ~ 0.11,
        product_fert3 == "27:12:00" ~ 0.12, 
        product_fert3 == "24:16:00" ~ 0.16,
        product_fert3 == "24:16" ~ 0.16,
        product_fert3 == "prime zn" ~ 0.13,
        product_fert3 == "meszn n12; p17.5; k0; zn10" ~ .175,
        product_fert3 == "prime msz" ~ .18,
        product_fert3 == "map/soa 14:15:0:9" ~ .15,
        product_fert3 == "29:10:s7%:zn(1%)" ~ .10,
        product_fert3 == "phosphoric acid" ~ 1.0, 
        product_fert3 == "dap / urea 28-13-0-1" ~ 0.13,
        product_fert3 == "granulock zs 24-11-0-6" ~ 0.24,# therese think this is an error0.24??
        product_fert3 == "urea / map 27:12" ~ 0.12,
        product_fert3 == "king mix" ~ 0.07,
        
        product_fert3 == "prime dsz/urea 70:30 blend" ~ 0.119, # from TB worksheet
        product_fert3 == "ssp 8.8p,0n,11s,19ca" ~ 0.088,
        product_fert3 == "dsz 5p,5n" ~ 0.05,
        product_fert3 == "dsz 20p,19n" ~ 0.2,
        product_fert3 == "zinc guard d2" ~ 0.193,
        product_fert3 == "acre max" ~ 0.11,
        product_fert3 == "primezn" ~ 0.13,
        product_fert3 == "map 22:11:0:4" ~ 0.11,
        product_fert3 == "granulock z22:11:0:04" ~ .11 ,
        product_fert3 == "uan" ~ 0.0,
        product_fert3 == "27:12" ~ 0.12,
        product_fert3 == "urea / soa 50/50" ~ 0.0,
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

#2. for start

fert_app <- fert_app %>%
  mutate(
    content_P_fert1_start = 
      case_when(
        product_fert1_start == "map" ~ 0.22,
        product_fert1_start == "prime dsz" ~ 0.17,
        product_fert1_start == "dap" ~ 0.2,
        product_fert1_start == "urea" ~ 0.0, 
        product_fert1_start == "ssp" ~ 0.088, 
        product_fert1_start == "granulock" ~ 0.175, 
        product_fert1_start == "granulock z" ~ 0.218, 
        product_fert1_start == "granuloc z" ~ 0.218, # I assume this is a typo?
        product_fert1_start == "map zn" ~ 0.22, 
        product_fert1_start == "zincguard d2" ~ 0.193,
        product_fert1_start == "soa" ~ 0.0, 
        product_fert1_start == "mesz" ~ 0.175, 
        product_fert1_start == "acremax" ~ 0.11,
        product_fert1_start == "27:12:00" ~ 0.12, 
        product_fert1_start == "24:16:00" ~ 0.16,
        product_fert1_start == "24:16" ~ 0.16,
        product_fert1_start == "prime zn" ~ 0.13,
        product_fert1_start == "meszn n12; p17.5; k0; zn10" ~ .175,
        product_fert1_start == "prime msz" ~ .18,
        product_fert1_start == "map/soa 14:15:0:9" ~ .15,
        product_fert1_start == "29:10:s7%:zn(1%)" ~ .10,
        product_fert1_start == "phosphoric acid" ~ 1.0,
        product_fert1_start == "dap / urea 28-13-0-1" ~ 0.13,
        product_fert1_start == "granulock zs 24-11-0-6" ~ 0.24,# therese think this is an error0.24
        product_fert1_start == "urea / map 27:12" ~ 0.12,
        product_fert1_start == "king mix" ~ 0.07,
        
        product_fert1_start == "prime dsz/urea 70:30 blend" ~ 0.119, # from TB worksheet
        product_fert1_start == "ssp 8.8p,0n,11s,19ca" ~ 0.088,
        product_fert1_start == "dsz 5p,5n" ~ 0.05,
        product_fert1_start == "dsz 20p,19n" ~ 0.2,
        product_fert1_start == "zinc guard d2" ~ 0.193,
        product_fert1_start == "acre max" ~ 0.11,
        product_fert1_start == "primezn" ~ 0.13,
        product_fert1_start == "map 22:11:0:4" ~ 0.11,
        product_fert1_start == "granulock z22:11:0:04" ~ .11 ,
        product_fert1_start == "uan" ~ 0.0,
        product_fert1_start == "27:12" ~ 0.12,
        product_fert1_start == "urea / soa 50/50" ~ 0.0,
        TRUE ~ 0))
fert_app <- fert_app %>%
  mutate(
    content_P_fert2_start = 
      case_when(
        product_fert2_start == "map" ~ 0.22,
        product_fert2_start == "prime dsz" ~ 0.17,
        product_fert2_start == "dap" ~ 0.2,
        product_fert2_start == "urea" ~ 0.0, 
        product_fert2_start == "ssp" ~ 0.088, 
        product_fert2_start == "granulock" ~ 0.175, 
        product_fert2_start == "granulockz" ~ 0.218,
        product_fert2_start == "granuloc z" ~ 0.218, # I assume this is a typo?
        product_fert2_start == "map zn" ~ 0.22, 
        product_fert2_start == "zincguard d2" ~ 0.193,
        product_fert2_start == "soa" ~ 0.0, 
        product_fert2_start == "mesz" ~ 0.175, 
        product_fert2_start == "acremax" ~ 0.11,
        product_fert2_start == "27:12:00" ~ 0.12, 
        product_fert2_start == "24:16:00" ~ 0.16,
        product_fert2_start == "24:16" ~ 0.16,
        product_fert2_start == "prime zn" ~ 0.13,
        product_fert2_start == "meszn n12; p17.5; k0; zn10" ~ .175,
        product_fert2_start == "prime msz" ~ .18,
        product_fert2_start == "map/soa 14:15:0:9" ~ .15,
        product_fert2_start == "29:10:s7%:zn(1%)" ~ .10,
        product_fert2_start == "phosphoric acid" ~ 1.0,
        product_fert2_start == "dap / urea 28-13-0-1" ~ 0.13,
        product_fert2_start == "granulock zs 24-11-0-6" ~ 0.24,# therese think this is an error0.24
        product_fert2_start == "urea / map 27:12" ~ 0.12,
        product_fert2_start == "king mix" ~ 0.07,
        
        product_fert2_start == "prime dsz/urea 70:30 blend" ~ 0.119, # from TB worksheet
        product_fert2_start == "ssp 8.8p,0n,11s,19ca" ~ 0.088,
        product_fert2_start == "dsz 5p,5n" ~ 0.05,
        product_fert2_start == "dsz 20p,19n" ~ 0.2,
        product_fert2_start == "zinc guard d2" ~ 0.193,
        product_fert2_start == "acre max" ~ 0.11,
        product_fert2_start == "primezn" ~ 0.13,
        product_fert2_start == "map 22:11:0:4" ~ 0.11,
        product_fert2_start == "granulock z22:11:0:04" ~ .11 ,
        product_fert2_start == "uan" ~ 0.0,
        product_fert2_start == "27:12" ~ 0.12,
        product_fert2_start == "urea / soa 50/50" ~ 0.0,
        TRUE ~ 0))
fert_app <- fert_app %>%
  mutate(
    content_P_fert3_start = 
      case_when(
        product_fert3_start == "map" ~ 0.22,
        product_fert3_start == "prime dsz" ~ 0.17,
        product_fert3_start == "dap" ~ 0.2,
        product_fert3_start == "urea" ~ 0.0, 
        product_fert3_start == "ssp" ~ 0.088, 
        product_fert3_start == "granulock" ~ 0.175, 
        product_fert3_start == "granulockz" ~ 0.218,
        product_fert3_start == "granuloc z" ~ 0.218, # I assume this is a typo?
        product_fert3_start == "map zn" ~ 0.22, 
        product_fert3_start == "zincguard d2" ~ 0.193,
        product_fert3_start == "soa" ~ 0.0, 
        product_fert3_start == "mesz" ~ 0.175, 
        product_fert3_start == "acremax" ~ 0.11,
        product_fert3_start == "27:12:00" ~ 0.12, 
        product_fert3_start == "24:16:00" ~ 0.16,
        product_fert3_start == "24:16" ~ 0.16,
        product_fert3_start == "prime zn" ~ 0.13,
        product_fert3_start == "meszn n12; p17.5; k0; zn10" ~ .175,
        product_fert3_start == "prime msz" ~ .18,
        product_fert3_start == "map/soa 14:15:0:9" ~ .15,
        product_fert3_start == "29:10:s7%:zn(1%)" ~ .10,
        product_fert3_start == "phosphoric acid" ~ 1.0, 
        product_fert3_start == "dap / urea 28-13-0-1" ~ 0.13,
        product_fert3_start == "granulock zs 24-11-0-6" ~ 0.24,# therese think this is an error0.24??
        product_fert3_start == "urea / map 27:12" ~ 0.12,
        product_fert3_start == "king mix" ~ 0.07,
        
        product_fert3_start == "prime dsz/urea 70:30 blend" ~ 0.119, # from TB worksheet
        product_fert3_start == "ssp 8.8p,0n,11s,19ca" ~ 0.088,
        product_fert3_start == "dsz 5p,5n" ~ 0.05,
        product_fert3_start == "dsz 20p,19n" ~ 0.2,
        product_fert3_start == "zinc guard d2" ~ 0.193,
        product_fert3_start == "acre max" ~ 0.11,
        product_fert3_start == "primezn" ~ 0.13,
        product_fert3_start == "map 22:11:0:4" ~ 0.11,
        product_fert3_start == "granulock z22:11:0:04" ~ .11 ,
        product_fert3_start == "uan" ~ 0.0,
        product_fert3_start == "27:12" ~ 0.12,
        product_fert3_start == "urea / soa 50/50" ~ 0.0,
        TRUE ~ 0))

#rate * content for trial

str(fert_app)
fert_app <- fert_app %>% 
  mutate(content_P_fert_rate1_start = (content_P_fert1_start * rate_fert1_start),
         content_P_fert_rate2_start = (content_P_fert2_start * rate_fert2_start),
         content_P_fert_rate3_start = (content_P_fert3_start * rate_fert3_start))

fert_app$content_P_fert_rate1_start[is.na(fert_app$content_P_fert_rate1_start)] <- 0 
fert_app$content_P_fert_rate2_start[is.na(fert_app$content_P_fert_rate2_start)] <- 0 
fert_app$content_P_fert_rate3_start[is.na(fert_app$content_P_fert_rate3_start)] <- 0 

fert_app <- fert_app %>%
  mutate(
    sum_P_content_start = content_P_fert_rate1_start + 
      content_P_fert_rate2_start + 
      content_P_fert_rate3_start)

#3. for topdress
fert_app <- fert_app %>%
  mutate(
    content_P_fert1_top = 
      case_when(
        product_fert1_top == "map" ~ 0.22,
        product_fert1_top == "prime dsz" ~ 0.17,
        product_fert1_top == "dap" ~ 0.2,
        product_fert1_top == "urea" ~ 0.0, 
        product_fert1_top == "ssp" ~ 0.088, 
        product_fert1_top == "granulock" ~ 0.175, 
        product_fert1_top == "granulock z" ~ 0.218, 
        product_fert1_top == "granuloc z" ~ 0.218, # I assume this is a typo?
        product_fert1_top == "map zn" ~ 0.22, 
        product_fert1_top == "zincguard d2" ~ 0.193,
        product_fert1_top == "soa" ~ 0.0, 
        product_fert1_top == "mesz" ~ 0.175, 
        product_fert1_top == "acremax" ~ 0.11,
        product_fert1_top == "27:12:00" ~ 0.12, 
        product_fert1_top == "24:16:00" ~ 0.16,
        product_fert1_top == "24:16" ~ 0.16,
        product_fert1_top == "prime zn" ~ 0.13,
        product_fert1_top == "meszn n12; p17.5; k0; zn10" ~ .175,
        product_fert1_top == "prime msz" ~ .18,
        product_fert1_top == "map/soa 14:15:0:9" ~ .15,
        product_fert1_top == "29:10:s7%:zn(1%)" ~ .10,
        product_fert1_top == "phosphoric acid" ~ 1.0,
        product_fert1_top == "dap / urea 28-13-0-1" ~ 0.13,
        product_fert1_top == "granulock zs 24-11-0-6" ~ 0.24,# therese think this is an error0.24
        product_fert1_top == "urea / map 27:12" ~ 0.12,
        product_fert1_top == "king mix" ~ 0.07,
        
        product_fert1_top == "prime dsz/urea 70:30 blend" ~ 0.119, # from TB worksheet
        product_fert1_top == "ssp 8.8p,0n,11s,19ca" ~ 0.088,
        product_fert1_top == "dsz 5p,5n" ~ 0.05,
        product_fert1_top == "dsz 20p,19n" ~ 0.2,
        product_fert1_top == "zinc guard d2" ~ 0.193,
        product_fert1_top == "acre max" ~ 0.11,
        product_fert1_top == "primezn" ~ 0.13,
        product_fert1_top == "map 22:11:0:4" ~ 0.11,
        product_fert1_top == "granulock z22:11:0:04" ~ .11 ,
        product_fert1_top == "uan" ~ 0.0,
        product_fert1_top == "27:12" ~ 0.12,
        product_fert1_top == "urea / soa 50/50" ~ 0.0,
        TRUE ~ 0))
fert_app <- fert_app %>%
  mutate(
    content_P_fert2_top = 
      case_when(
        product_fert2_top == "map" ~ 0.22,
        product_fert2_top == "prime dsz" ~ 0.17,
        product_fert2_top == "dap" ~ 0.2,
        product_fert2_top == "urea" ~ 0.0, 
        product_fert2_top == "ssp" ~ 0.088, 
        product_fert2_top == "granulock" ~ 0.175, 
        product_fert2_top == "granulockz" ~ 0.218,
        product_fert2_top == "granuloc z" ~ 0.218, # I assume this is a typo?
        product_fert2_top == "map zn" ~ 0.22, 
        product_fert2_top == "zincguard d2" ~ 0.193,
        product_fert2_top == "soa" ~ 0.0, 
        product_fert2_top == "mesz" ~ 0.175, 
        product_fert2_top == "acremax" ~ 0.11,
        product_fert2_top == "27:12:00" ~ 0.12, 
        product_fert2_top == "24:16:00" ~ 0.16,
        product_fert2_top == "24:16" ~ 0.16,
        product_fert2_top == "prime zn" ~ 0.13,
        product_fert2_top == "meszn n12; p17.5; k0; zn10" ~ .175,
        product_fert2_top == "prime msz" ~ .18,
        product_fert2_top == "map/soa 14:15:0:9" ~ .15,
        product_fert2_top == "29:10:s7%:zn(1%)" ~ .10,
        product_fert2_top == "phosphoric acid" ~ 1.0,
        product_fert2_top == "dap / urea 28-13-0-1" ~ 0.13,
        product_fert2_top == "granulock zs 24-11-0-6" ~ 0.24,# therese think this is an error0.24
        product_fert2_top == "urea / map 27:12" ~ 0.12,
        product_fert2_top == "king mix" ~ 0.07,
        
        product_fert2_top == "prime dsz/urea 70:30 blend" ~ 0.119, # from TB worksheet
        product_fert2_top == "ssp 8.8p,0n,11s,19ca" ~ 0.088,
        product_fert2_top == "dsz 5p,5n" ~ 0.05,
        product_fert2_top == "dsz 20p,19n" ~ 0.2,
        product_fert2_top == "zinc guard d2" ~ 0.193,
        product_fert2_top == "acre max" ~ 0.11,
        product_fert2_top == "primezn" ~ 0.13,
        product_fert2_top == "map 22:11:0:4" ~ 0.11,
        product_fert2_top == "granulock z22:11:0:04" ~ .11 ,
        product_fert2_top == "uan" ~ 0.0,
        product_fert2_top == "27:12" ~ 0.12,
        product_fert2_top == "urea / soa 50/50" ~ 0.0,
        TRUE ~ 0))
fert_app <- fert_app %>%
  mutate(
    content_P_fert3_top = 
      case_when(
        product_fert3_top == "map" ~ 0.22,
        product_fert3_top == "prime dsz" ~ 0.17,
        product_fert3_top == "dap" ~ 0.2,
        product_fert3_top == "urea" ~ 0.0, 
        product_fert3_top == "ssp" ~ 0.088, 
        product_fert3_top == "granulock" ~ 0.175, 
        product_fert3_top == "granulockz" ~ 0.218,
        product_fert3_top == "granuloc z" ~ 0.218, # I assume this is a typo?
        product_fert3_top == "map zn" ~ 0.22, 
        product_fert3_top == "zincguard d2" ~ 0.193,
        product_fert3_top == "soa" ~ 0.0, 
        product_fert3_top == "mesz" ~ 0.175, 
        product_fert3_top == "acremax" ~ 0.11,
        product_fert3_top == "27:12:00" ~ 0.12, 
        product_fert3_top == "24:16:00" ~ 0.16,
        product_fert3_top == "24:16" ~ 0.16,
        product_fert3_top == "prime zn" ~ 0.13,
        product_fert3_top == "meszn n12; p17.5; k0; zn10" ~ .175,
        product_fert3_top == "prime msz" ~ .18,
        product_fert3_top == "map/soa 14:15:0:9" ~ .15,
        product_fert3_top == "29:10:s7%:zn(1%)" ~ .10,
        product_fert3_top == "phosphoric acid" ~ 1.0, 
        product_fert3_top == "dap / urea 28-13-0-1" ~ 0.13,
        product_fert3_top == "granulock zs 24-11-0-6" ~ 0.24,# therese think this is an error0.24??
        product_fert3_top == "urea / map 27:12" ~ 0.12,
        product_fert3_top == "king mix" ~ 0.07,
        
        product_fert3_top == "prime dsz/urea 70:30 blend" ~ 0.119, # from TB worksheet
        product_fert3_top == "ssp 8.8p,0n,11s,19ca" ~ 0.088,
        product_fert3_top == "dsz 5p,5n" ~ 0.05,
        product_fert3_top == "dsz 20p,19n" ~ 0.2,
        product_fert3_top == "zinc guard d2" ~ 0.193,
        product_fert3_top == "acre max" ~ 0.11,
        product_fert3_top == "primezn" ~ 0.13,
        product_fert3_top == "map 22:11:0:4" ~ 0.11,
        product_fert3_top == "granulock z22:11:0:04" ~ .11 ,
        product_fert3_top == "uan" ~ 0.0,
        product_fert3_top == "27:12" ~ 0.12,
        product_fert3_top == "urea / soa 50/50" ~ 0.0,
        TRUE ~ 0))

#rate * content for trial

str(fert_app)
fert_app <- fert_app %>% 
  mutate(content_P_fert_rate1_top = (content_P_fert1_top * rate_fert1_top),
         content_P_fert_rate2_top = (content_P_fert2_top * rate_fert2_top),
         content_P_fert_rate3_top = (content_P_fert3_top * rate_fert3_top))

fert_app$content_P_fert_rate1_top[is.na(fert_app$content_P_fert_rate1_top)] <- 0 
fert_app$content_P_fert_rate2_top[is.na(fert_app$content_P_fert_rate2_top)] <- 0 
fert_app$content_P_fert_rate3_top[is.na(fert_app$content_P_fert_rate3_top)] <- 0 

fert_app <- fert_app %>%
  mutate(
    sum_P_content_top = content_P_fert_rate1_top + 
      content_P_fert_rate2_top + 
      content_P_fert_rate3_top)


################################################################################
###############  sum the contnet for all P applications trial            #######
################################################################################
## Therese has suggested we dont need this step


#1. strip and starter and top
fert_app <- fert_app %>%
  mutate(Total_sum_P_content = sum_P_content + sum_P_content_start + sum_P_content_top)

################################################################################################
##### export working and heaps less than workings
################################################################################################

#getwd()
## all of workings
write.csv(fert_app,
          "W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step2_fert_app_all_step_2019.csv")
str(fert_app)


selection_fert_app <- fert_app %>%
  dplyr::select(
    Paddock_ID:Strip_Rate,
    av_rain,
    Strip_Type,
    Start_Fert,
    Top_Dress,
    
    #sum_N_content,
    #S_sum_N_content,
    #TD_sum_N_content,
    Total_sum_N_content,
    
    #sum_P_content,
    #S_sum_P_content,
    #TD_sum_P_content,
    Total_sum_P_content,
    more_info
    
  )


write.csv(selection_fert_app,
          "W:/value_soil_testing_prj/Yield_data/2020/processing/processing_files/step2_fert_app_select_clm_2019.csv")
          

### What was used by growers - not I need to have the starter and topdress cals to run for this to work.


