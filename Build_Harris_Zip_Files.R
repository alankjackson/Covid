
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


cat("\n\n=============== Build Harris Files started =========\n\n")
print(lubridate::now())
cat("\n=============== Build Harris Files started =========\n\n")


###################################
#   get and set up the basic data
###################################

#   Directory where data is stored
DataLocation <- "/home/ajackson/Dropbox/Rprojects/Covid/"
DataArchive <- "/home/ajackson/Dropbox/mirrors/ajackson/SharedData/"
DataStatic <- "/home/ajackson/Dropbox/Rprojects/Datasets/"

#   Case data
DF <- readRDS(paste0(DataLocation, "HarrisZip.rds")) %>% 
  mutate(week=week(Date)) %>% 
  filter(!Zip=="77002") # drop downtown because of jail

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
#          Calculate stuff for a vector
######################################################################

Calculate <- function(df, Grouper) {
  Grouper_str <- Grouper
  Grouper <- rlang::sym(Grouper)
  
  foo <- df %>% 
    group_by(!!Grouper, week) %>%
      mutate(logcases=log10(Cases)) %>%
      mutate(Days=1:n()) %>% 
      do(model = try(lm(logcases ~ Days, data = .),
                     silent=TRUE)
         ) %>% 
    ungroup() %>% 
    gather(model, lm, starts_with("model")) %>%
    mutate(error=purrr::map_lgl(lm, ~inherits(., "try-error"))) %>% 
    filter(error == FALSE) %>%
    mutate(tidied = purrr::map(lm, broom::tidy)) %>%
    unnest(tidied) %>%
    filter(term=="Days") %>% 
    drop_na() %>% 
    mutate(m=estimate) %>% 
    #   calculate doubling time
    mutate(m=signif(log10(2)/m,3)) %>% 
    mutate(m=replace(m, m>200, NA)) %>%  # Doubling too large
    mutate(m=replace(m, m<=0, NA)) %>% # doubling negative
    select(!!Grouper, week, m) %>% 
    rename(Doubling=m) %>% 
    group_by(!!Grouper, week) %>% 
       summarize(Doubling=mean(Doubling, na.rm=TRUE)) %>% 
    ungroup()
  
  foo$Doubling[is.nan(foo$Doubling)]<-NA
  
  #---------------  Clean up and calc base quantities
  foo2 <- df %>%     
    replace_na(list(Cases=0)) %>% 
    group_by(!!Grouper, week) %>% 
      summarize(Cases=max(Cases)) %>% 
    ungroup() %>% 
    group_by(!!Grouper) %>%
      mutate(new_cases=(Cases-lag(Cases, default=first(Cases)))) %>%
      mutate(new_cases=pmax(new_cases, 0)) %>% # truncate negative numbers
      mutate(pct_chg=100*new_cases/lag(Cases, default=first(Cases))) %>%
      mutate(active_cases=Cases-lag(Cases, n=2, default=0)) %>%
    ungroup()   
  
  foo2 <- left_join(foo, foo2, by=c(Grouper_str, "week"))
  
  foo2 <- foo2 %>% 
    mutate(Date=ymd("2020-05-24")+(week-21)*7)
  
  return(foo2)
}  ######################  end Calculate

#################################################################
######################   Calculations for School Districts
#################################################################

District_Calc <- Calculate(DF_school, "District")

  # Calculate per 1,000 numbers

District_Calc <- School_Values %>% 
  select(District, Pop) %>% 
  left_join(District_Calc, ., by="District") %>% 
  mutate(Cases_percap=signif(1000*Cases/Pop,3), 
         new_cases_percap=signif(1000*new_cases/Pop,3), 
         active_cases_percap=signif(1000*active_cases/Pop,3))

#   Add date in-person classes began

start_dates <- tribble(
  ~ District, ~ Start_Date,
  "Aldine", "2020-09-21", 
  "Alief", "2020-09-29", 
  "Channelview", "2020-10-05", 
  "Crosby", "2020-09-08", 
  "Cypress-Fairbanks", "2020-09-08", 
  "Deer Park", "2020-09-16", 
  "Galena Park", "2020-10-05", 
  "Goose Creek", "2020-09-21", 
  "Houston", "2020-10-19", 
  "Humble", "2020-08-24", 
  "Klein", "2020-09-08", 
  "La Porte", "2020-09-08",
  "Pasadena", "2020-09-08", 
  "Sheldon", "2020-10-05", 
  "Spring", "2020-09-14",
  "Spring Branch", "2020-09-08", 
  "Clear Creek", "2020-08-31",
  "Katy", "2020-09-08" 
)

start_dates$Start_Date <- lubridate::ymd(start_dates$Start_Date)

District_Calc <- left_join(District_Calc, start_dates, by="District")

tail(District_Calc)
#################################################################
######################   Calculations for Zip Codes
#################################################################

Zip_Calc <- Calculate(DF, "Zip")

  # Calculate per 1,000 numbers

Zip_Calc <- Harris_Census %>% 
  select(ZCTA, Pop) %>% 
  rename(Zip=ZCTA) %>% 
  left_join(Zip_Calc, ., by="Zip") %>% 
  mutate(Cases_percap=signif(1000*Cases/Pop,3), 
         new_cases_percap=signif(1000*new_cases/Pop,3), 
         active_cases_percap=signif(1000*active_cases/Pop,3))
            
tail(Zip_Calc)
#################################################################
#                     Save files
#################################################################

path <- "/home/ajackson/Dropbox/Rprojects/Covid/Today_Data/"

saveRDS(District_Calc, paste0(path,"Today_Harris_schools.rds"))
saveRDS(Zip_Calc, paste0(path,"Today_Harris_zip.rds"))


path <- "/home/ajackson/Dropbox/mirrors/ajackson/Covid/"

saveRDS(District_Calc, paste0(path,"Today_Harris_schools.rds"))
saveRDS(Zip_Calc, paste0(path,"Today_Harris_zip.rds"))

cat("\n\n=============== Build Harris Files finished =========\n\n")
print(lubridate::now())
cat("\n=============== Build Harris Files finished =========\n\n")


