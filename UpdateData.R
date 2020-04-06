#   Read state Health Dept site for Covid update

print(paste("Starting run ---------------------", lubridate::now()))
library(tidyverse)
library(stringr)
library(RCurl)
library(rvest)
library(httr)
library(xml2)
library(RSelenium)
library(xfun)

options(stringsAsFactors = FALSE)

url <- "https://txdshs.maps.arcgis.com/apps/opsdashboard/index.html#/ed483ecd702b4298ab01e8b9cafc8b83"

#---------------------------------------------------------------------
#   retrieve the webpage with a headless browser
#---------------------------------------------------------------------

# start the server and browser in headless mode
rD <- rsDriver(browser="firefox",
               extraCapabilities = list("moz:firefoxOptions" = list(
                 args = list('--headless')))
)

driver <- rD$client

# navigate to an URL
driver$navigate(url)
Sys.sleep(9)

# get parsed page source
parsed_pagesource <- driver$getPageSource()[[1]]

#close the driver
driver$close()

#close the server
rD$server$stop()

#---------------------------------------------------------------------
#   Extract cases per county
#---------------------------------------------------------------------

#result <- read_html(parsed_pagesource) %>%
#  html_nodes(xpath='/html/body/div/div/div/div/div/div/margin-container/full-container/div[11]/margin-container/full-container/div/div/nav') %>%
#  html_text() %>% 
#  str_replace_all("\n"," ") %>% 
#  str_split("  +")
#
#result <- result[[1]][2:(length(result[[1]])-1)]
#res <- cbind.data.frame(split(result, 
#                              rep(1:2, times=length(result)/2)), 
#                        stringsAsFactors=F)
#names(res) <- c("County", "Cases") 
#res$County <- str_remove(res$County, " County") 
#res$Cases <- as.numeric(res$Cases)

#   What to do. Deaths per county appeared and then disappeared.
#   I'll just add the column in for now.

#mytable <- res %>% mutate(Deaths="-")
#mytable <- mytable %>% mutate(Date=lubridate::today())
#
#mytable

####   Use arcgis site instead, easier and they still post deaths

url <- "https://opendata.arcgis.com/datasets/bc83058386d2434ca8cf90b26dc6b580_0.csv"

mytable <- read_csv(url) %>% 
  replace_na(list(Count_ = 0, Deaths=0)) %>% 
  select(County, Cases=Count_, Deaths, LastUpdate) %>% 
  mutate(Date=lubridate::today()-1) %>% # Add today's date
  select(County, Cases, Date, Deaths, LastUpdate) # reorder columns

mytable$LastUpdate <- as.character(mytable$LastUpdate)

mytable

#---------------------------------------------------------------------
#   Extract Testing status
#---------------------------------------------------------------------

total_tests <- 
  read_html(parsed_pagesource) %>%
  html_nodes(xpath='//*[@id="ember16"]') %>%
  html_text() %>%  
  str_replace_all("\n"," ") %>% 
  str_remove_all(",") %>% 
  str_split("  +") 
total_tests <- total_tests[[1]][2]

public_tests <- 
  read_html(parsed_pagesource) %>%
  html_nodes(xpath='//*[@id="ember23"]') %>%
  html_text() %>%  
  str_replace_all("\n"," ") %>% 
  str_remove_all(",") %>% 
  str_split("  +")
public_tests <- public_tests[[1]][2]

private_tests <- 
  read_html(parsed_pagesource) %>%
  html_nodes(xpath='//*[@id="ember30"]') %>%
  html_text() %>% 
  str_replace_all("\n"," ") %>% 
  str_remove_all(",") %>% 
  str_split("  +")
private_tests <- private_tests[[1]][2]


print(paste("Total:", total_tests))
print(paste("Public:", public_tests))
print(paste("Private:", private_tests))


print("--0--")

testing_status <- tribble(
  ~Total,               ~Public,            ~Private,
  total_tests, public_tests, private_tests
)   

print("--1--")

testing_status <- testing_status %>% mutate(Date=lubridate::today()-1)

print("--2--")
#---------------------------------------------------------------------
#   Extract Total Deaths
#---------------------------------------------------------------------

deaths <- 
  read_html(parsed_pagesource) %>%
  html_nodes(xpath='//*[@id="ember45"]') %>%
  html_text() %>% 
  str_replace_all("\n"," ") %>% 
  str_remove_all(",") %>% 
  str_split("  +")
print("--3--")
deaths=as.numeric(deaths[[1]][2])

print("--4--")
deaths_today <- tribble(
  ~Date, ~Cum_Deaths,
  lubridate::today()-1, deaths
)

print("--5--")

#---------------------------------------------------------------------
#   Read in excel file from state
#---------------------------------------------------------------------

url <- "https://dshs.texas.gov/coronavirus/TexasCOVID19CaseCountData.xlsx"
myfile <- getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

path <- "/home/ajackson/Dropbox/Rprojects/Covid/TexasDataXcel/"
writeBin(myfile, paste0(path, "Cases_by_County_", lubridate::today(),".xlsx"))



#---------------------------------------------------------------------
#   Read in old data, append new data, and save
#---------------------------------------------------------------------

########################################################

# Read in the old data
CovidData <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Covid.rds")
# append the new data
CovidData <- bind_rows(CovidData, mytable)
# Save an accumulated file in case of a failure
saveRDS(CovidData,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_Covid.rds"))
# Save the real file for later use
saveRDS(CovidData,"/home/ajackson/Dropbox/Rprojects/Covid/Covid.rds")
# Also save to mirror site
saveRDS(CovidData,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Covid.rds")

################   Testing data
# Read in the old data
TestingData <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Testing.rds")
# append the new data
TestingData <- bind_rows(TestingData, testing_status)
# Save an accumulated file in case of a failure
saveRDS(TestingData,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_Testing.rds"))
# Save the real file for later use
saveRDS(TestingData,"/home/ajackson/Dropbox/Rprojects/Covid/Testing.rds")
# Also save to mirror site
saveRDS(TestingData,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Testing.rds")


################   Death data
# Read in the old data
deaths <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Deaths.rds")
# append the new data
deaths <- bind_rows(deaths, deaths_today)
# Save an accumulated file in case of a failure
saveRDS(deaths ,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_deaths.rds"))
# Save the real file for later use
saveRDS(deaths,"/home/ajackson/Dropbox/Rprojects/Covid/Deaths.rds")
# Also save to mirror site
saveRDS(deaths,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Deaths.rds")



print(paste("Ending run ---------------------", lubridate::now()))