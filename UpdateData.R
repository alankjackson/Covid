#   Read state Health Dept site for Covid update

print(paste("Starting run ---------------------", lubridate::now()))
library(tidyverse)
library(stringr)
library(lubridate)
library(RCurl)
library(rvest)
library(httr)
library(xml2)
library(RSelenium)
library(xfun)
library(readxl)

options(stringsAsFactors = FALSE)

cat("\n\n=============== UpdateData updates started =========\n\n")
print(lubridate::now())
cat("\n=============== UpdateData updates started =========\n\n")

#url <- "https://txdshs.maps.arcgis.com/apps/opsdashboard/index.html#/ed483ecd702b4298ab01e8b9cafc8b83"

#---------------------------------------------------------------------
#   retrieve the webpage with a headless browser
#---------------------------------------------------------------------

# start the server and browser in headless mode
#rD <- rsDriver(browser="firefox",
#               extraCapabilities = list("moz:firefoxOptions" = list(
#                 args = list('--headless')))
#)
#
#driver <- rD$client
#
## navigate to an URL
#driver$navigate(url)
#Sys.sleep(9)
#
## get parsed page source
#parsed_pagesource <- driver$getPageSource()[[1]]
#
##close the driver
#driver$close()
#
##close the server
#rD$server$stop()
#
#---------------------------------------------------------------------
#   Extract cases per county
#---------------------------------------------------------------------

#---------------------------------------------------------------------
#   Read in excel files from state and save
#---------------------------------------------------------------------


this_day <- lubridate::today()

#this_day <- lubridate::ymd("2022-01-22")

url <- "https://dshs.texas.gov/coronavirus/TexasCOVID19CaseCountData.xlsx"
casecounts <- getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

path <- "/home/ajackson/Dropbox/Rprojects/CovidTempData/TexasDataXcel/"
writeBin(casecounts, paste0(path, "Cases_by_County_", this_day,".xlsx"))
casecounts_path <- paste0(path, "Cases_by_County_", this_day,".xlsx")

url <- "https://dshs.texas.gov/coronavirus/TexasCOVID-19CumulativeTestsOverTimebyCounty.xlsx"
#url <- "https://www.dshs.texas.gov/chs/data/COVID-19CumulativeTestTotalsbyCounty.xlsx"
Tests <- getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

path <- "/home/ajackson/Dropbox/Rprojects/CovidTempData/TexasDataXcel/"
writeBin(Tests, paste0(path, "Tests_by_County_", this_day,".xlsx"))
Tests_path <- paste0(path, "Tests_by_County_", this_day,".xlsx")

url <- "https://dshs.texas.gov/coronavirus/TexasCOVID19DailyCountyFatalityCountData.xlsx"
Deaths <- getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

path <- "/home/ajackson/Dropbox/Rprojects/CovidTempData/TexasDataXcel/"
writeBin(Deaths, paste0(path, "Deaths_by_County_", this_day,".xlsx"))

url <- "https://dshs.texas.gov/coronavirus/TexasCOVID19DailyCountyCaseCountData.xlsx"
DailyCounts <- getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

path <- "/home/ajackson/Dropbox/Rprojects/CovidTempData/TexasDataXcel/"
writeBin(DailyCounts, paste0(path, "County_Case_Data_", this_day,".xlsx"))

#   Cases and Deaths

foo <- read_excel(casecounts_path,
                  sheet="Case and Fatalities_ALL",
                  range=cell_rows(2:256)) %>% 
  rename(County=1, Cases=2, Probable_Cases=3, Deaths=4) %>% 
  select(County, Cases, Deaths) %>% 
  filter(County!="County", County!="Total") %>% 
  filter(!str_detect(County, "DSHS")) %>% 
  mutate(Date=this_day) %>% 
  mutate(County=str_replace_all(County, "[\r\n]" , "")) %>% 
  mutate(County=str_replace(County, "SanAugustine", "San Augustine")) %>% 
  filter(County != "Probable cases are not included in the total case numbers") %>% 
  filter(!str_detect(County, "Texas is reporting"))
  #mutate(LastUpdate=as.character(lubridate::today()))
foo$Cases <- as.numeric(foo$Cases)
foo$Deaths <- as.numeric(foo$Deaths)

head(foo)
tail(foo)

#  emergency backup file
saveRDS(foo,paste0("/home/ajackson/Dropbox/Rprojects/CovidTempData/DailyBackups/",this_day,"_mytable.rds"))
# Read in the old data
CovidData <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Covid.rds")
# append the new data
CovidData <- bind_rows(CovidData, foo)
# Save an accumulated file in case of a failure
saveRDS(CovidData,paste0("/home/ajackson/Dropbox/Rprojects/CovidTempData/DailyBackups/",this_day,"_Covid.rds"))
# Save the real file for later use
saveRDS(CovidData,"/home/ajackson/Dropbox/Rprojects/Covid/Covid.rds")
# Also save to mirror site
saveRDS(CovidData,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Covid.rds")

#---------------------------------------------------------------------
#   Extract Testing status
#---------------------------------------------------------------------

#total_tests <- 
#  read_html(parsed_pagesource) %>%
#  html_nodes(xpath='//*[@id="ember16"]') %>%
#  html_text() %>%  
#  str_replace_all("\n"," ") %>% 
#  str_remove_all(",") %>% 
#  str_split("  +") 
#total_tests <- total_tests[[1]][2]
#
#public_tests <- 
#  read_html(parsed_pagesource) %>%
#  html_nodes(xpath='//*[@id="ember23"]') %>%
#  html_text() %>%  
#  str_replace_all("\n"," ") %>% 
#  str_remove_all(",") %>% 
#  str_split("  +")
#public_tests <- public_tests[[1]][2]
#
#private_tests <- 
#  read_html(parsed_pagesource) %>%
#  html_nodes(xpath='//*[@id="ember30"]') %>%
#  html_text() %>% 
#  str_replace_all("\n"," ") %>% 
#  str_remove_all(",") %>% 
#  str_split("  +")
#private_tests <- private_tests[[1]][2]
#
#
#print(paste("Total:", total_tests))
#print(paste("Public:", public_tests))
#print(paste("Private:", private_tests))
#
#
#print("--0--")
#
#testing_status <- tribble(
#  ~Total,               ~Public,            ~Private,
#  total_tests, public_tests, private_tests
#)   
#
#print("--test--")

#Tests_path <- "/home/ajackson/Dropbox/Rprojects/Covid/TexasDataXcel/Tests_by_County_2020-05-16.xlsx"
#foo <- read_excel(Tests_path) #%>% 

# No data for May 5

#my_colnames <- c(
#  ymd("2020-04-21")+0:13,  
#  ymd("2020-05-06")+0:(ncol(foo)-17))
#
#my_colnames <- my_colnames %>% 
#  format('%Y-%m-%d')
#my_colnames <- c("County", my_colnames)
#
#my_columns <- c(1:15, 17:ncol(foo))
#
#foo <- foo %>% 
#  rename_at(my_columns,~ my_colnames)
#
#foo <- foo[2:258,] # delete bad rows
#foo <- foo[-16] # delete bad column
#
#foo <- foo %>% pivot_longer(-County, names_to="Date", values_to="Tests")
#
#foo <- foo %>% mutate(Tests=as.numeric(Tests))
#
#total_tests <- last(foo$Tests[foo$County=="TOTAL"])
#
#testing_status <- tribble(
#  ~Total, ~Public, ~Private,
#  total_tests, NA, NA
#)   
#
#tail(foo)
#
#testing_status <- testing_status %>% 
#  mutate(Date=lubridate::today()-1) %>% 
#  mutate(Total=as.character(Total))
#################   Testing data
## Read in the old data
#TestingData <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Testing.rds")
## append the new data
#TestingData <- bind_rows(TestingData, testing_status)
## Save an accumulated file in case of a failure
#saveRDS(TestingData,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_Testing.rds"))
## Save the real file for later use
#saveRDS(TestingData,"/home/ajackson/Dropbox/Rprojects/Covid/Testing.rds")
## Also save to mirror site
#saveRDS(TestingData,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Testing.rds")
#
## Save county level file as well
#saveRDS(foo,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_County_Testing.rds"))
## Save the real file for later use
#saveRDS(foo,"/home/ajackson/Dropbox/Rprojects/Covid/County_Testing.rds")
#
#
#
#print("--2--")
#---------------------------------------------------------------------
#   Extract Total Deaths
#---------------------------------------------------------------------

#deaths <- 
#  read_html(parsed_pagesource) %>%
#  html_nodes(xpath='//*[@id="ember45"]') %>%
#  html_text() %>% 
#  str_replace_all("\n"," ") %>% 
#  str_remove_all(",") %>% 
#  str_split("  +")
#print("--3--")
#deaths=as.numeric(deaths[[1]][2])
#
#print("--4--")
#deaths_today <- tribble(
#  ~Date, ~Cum_Deaths,
#  lubridate::today()-1, deaths
#)
#################   Death data
## Read in the old data
#deaths <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Deaths.rds")
## append the new data
#deaths <- bind_rows(deaths, deaths_today)
## Save an accumulated file in case of a failure
#saveRDS(deaths ,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_deaths.rds"))
## Save the real file for later use
#saveRDS(deaths,"/home/ajackson/Dropbox/Rprojects/Covid/Deaths.rds")
## Also save to mirror site
#saveRDS(deaths,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Deaths.rds")

print("--5--")

#---------------------------------------------------------------------
#   Extract Prison information
#---------------------------------------------------------------------

#url="https://www.tdcj.texas.gov/covid-19/offender_mac.html"

#webpage <- 
#  read_html(url) 

#tbls_ls <- read_html(url) %>%
#  html_nodes("table") %>%
#  html_table(fill = TRUE)
#
# Unpack all ten tables and combine where needed

#col.names <- c("Pending_Tests",
#               "Negative_Tests", 
#               "Positive_Tests", 
#               "Medical_Restriction",
#               "Medical_Isolation")

#df <- tbls_ls[[1]] %>% 
#  rename(Unit=X1, Pending_Tests=X2) %>% 
#  mutate(Unit=str_squish(Unit))

#for (i in 2:5){
#  tmp <- tbls_ls[[i]] %>% 
#    rename(Unit=X1, !!col.names[i] := X2) %>% 
#    mutate(Unit=str_squish(Unit))
#  df <- left_join(df, tmp, by="Unit")
#}

#staff <- tbls_ls[[6]] %>% 
#  rename(Unit=X1, Staff_Positive_Tests=X2) %>% 
#  mutate(Unit=str_squish(Unit))

#for (i in 7:10) {
#  tmp <- tbls_ls[[i]] %>% 
#    rename(Unit=X1, Staff_Positive_Tests=X2) %>% 
#    mutate(Unit=str_squish(Unit))
#  staff <- bind_rows(staff, tmp)
#}
#new_prisons <- left_join(df, staff, by="Unit")
#new_prisons <- new_prisons %>% mutate(Date=lubridate::today()-1)



#---------------------------------------------------------------------
#   Read in old data, append new data, and save
#---------------------------------------------------------------------



################   Prison data
# Read in the old data
#prisons <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Prisons.rds")
# append the new data
#prisons <- bind_rows(prisons, new_prisons)
# Save an accumulated file in case of a failure
#saveRDS(prisons ,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_Prisons.rds"))
# Save the real file for later use
#saveRDS(prisons,"/home/ajackson/Dropbox/Rprojects/Covid/Prisons.rds")
# Also save to mirror site
#saveRDS(prisons,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Prisons.rds")



print(paste("Ending run ---------------------", lubridate::now()))
