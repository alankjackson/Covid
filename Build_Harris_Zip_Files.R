
#   Build the time-consuming files required by the Covid app for Harris County

#   outputs are:
#      Harris_zip - Zipcode level data with all calculations averaged per week
#      Harris_map - Zipcode level weekly data for map and animation
#  Harris_schools - School district data
#  Harris_schools_map - School district data

library(tidyverse)
library(stringr)
library(lubridate)
#library(broom)
#library(purrr)


cat("\n\n=============== Build Covid Files started =========\n\n")
print(lubridate::now())
cat("\n=============== Build Covid Files started =========\n\n")


###################################
#   get and set up the basic data
###################################

#   Directory where data is stored
DataLocation <- "/home/ajackson/Dropbox/Rprojects/Covid/"
DataArchive <- "/home/ajackson/Dropbox/mirrors/ajackson/SharedData/"
DataStatic <- "/home/ajackson/Dropbox/Rprojects/Datasets/"

#   Case data
DF <- readRDS(paste0(DataLocation, "HarrisZip.rds"))

#   Static Data

Harris_Census <- readRDS(paste0(DataStatic, "HarrisCounty_CensusByZip.rds"))
Harris_ZipPoly <- readRDS(paste0(DataStatic, "HarrisCounty_CensusByZip_polys.rds"))

School_Values <- readRDS(paste0(DataStatic, 
                                "SchoolDistricts/Harris_School_Values.rds"))
School_Polys <- readRDS(paste0(DataStatic, 
                                "SchoolDistricts/Harris_School_Polys.rds"))
School_Frac <- readRDS(paste0(DataStatic, 
                                "SchoolDistricts/Fraction_Zip_Per_School_District.rds"))

###  Average case data into school districts

foo <- left_join(School_Frac, DF, by=c("Zip_Code"="Zip")) 

DF_school <- foo %>% 
  select(-area, -total_area) %>% 
  mutate(Cases=Cases*fraction_of_zip) %>% 
  group_by(District, Date) %>% 
    summarize(Cases=sum(Cases, na.rm=TRUE)) %>% 
  mutate(Cases=round(Cases, 0)) %>% 
  drop_na() # drop zipcodes not in Harris County


######################################################
#         Functions
######################################################
######################################################################
#          Calculate doubling time for a vector
######################################################################
doubling <- function(cases, window=7, grouper) {
  if (length(cases)<7){ # must have >= 7 points
    return(rep(NA,length(cases)))
  }
 # halfwidth <- as.integer(window/2)
 # rolling_lm <- tibbletime::rollify(.f = function(logcases, Days) {
 #   lm(logcases ~ Days)
 # }, 
 # window = window, 
 # unlist = FALSE) 
  
  foo <- 
    tibble(Days = 1:length(cases), logcases = log10(cases)) %>%
    mutate(roll_lm = rolling_lm(logcases, Days)) %>% 
    filter(!is.na(roll_lm)) %>%
    mutate(tidied = purrr::map(roll_lm, broom::tidy)) %>%
    unnest(tidied) %>%
    filter(term=="Days") %>% 
    mutate(m=estimate) %>% 
    #   calculate doubling time
    mutate(m=signif(log10(2)/m,3)) %>% 
    mutate(m=replace(m, m>200, NA)) %>%  
    mutate(m=replace(m, m<=0, NA)) %>% 
    select(m)
  return(matlab::padarray(foo[[1]], c(0,halfwidth), "replicate"))
}  ######################  end doubling







