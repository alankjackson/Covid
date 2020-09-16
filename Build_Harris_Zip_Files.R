
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
DF <- readRDS(paste0(DataLocation, "HarrisZip.rds")) %>% 
  mutate(week=week(Date))

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
  ungroup() %>% 
  mutate(Cases=round(Cases, 0)) %>% 
  drop_na() %>% # drop zipcodes not in Harris County
  mutate(week=week(Date))


######################################################
#         Functions
######################################################

######################################################################
#          Calculate doubling time for a vector
######################################################################
doubling <- function(df, Grouper) {
  Grouper_str <- Grouper
  Grouper <- rlang::sym(Grouper)
  
  foo <- df %>% 
    group_by(!!Grouper, week) %>%
      mutate(logcases=log10(Cases)) %>%
      mutate(Days=1:n()) %>% 
      do(model = lm(logcases ~ Days, data = .)) %>% 
    ungroup() %>% 
    mutate(tidied = purrr::map(model, broom::tidy)) %>%
    unnest(tidied) %>%
    filter(term=="Days") %>% 
    drop_na() %>% 
    mutate(m=estimate) %>% 
    #   calculate doubling time
    mutate(m=signif(log10(2)/m,3)) %>% 
    mutate(m=replace(m, m>200, NA)) %>%  # Doubling too large
    mutate(m=replace(m, m<=0, NA)) %>% # doubling negative
    select(!!grouper, week, m)
  
  #---------------  Clean up and calc base quantities
  foo2 <- df %>%     
    replace_na(list(Cases=0)) %>% 
    group_by(!!Grouper, week) %>%
      arrange(Date) %>% 
      mutate(new_cases=(Cases-lag(Cases, default=first(Cases)))) %>%
      mutate(new_cases=pmax(new_cases, 0)) %>% # truncate negative numbers
      mutate(pct_chg=100*new_cases/lag(Cases, default=first(Cases))) %>%
      mutate(active_cases=Cases-lag(Cases, n=9, default=0)) %>%
    ungroup()   
  
  
  
  return(foo2)
}  ######################  end doubling

double <- doubling(DF_school, "District")






