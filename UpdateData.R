#   Read state Health Dept site for Covid update

print(paste("Starting run ---------------------", lubridate::now()))
library(tidyverse)
library(stringr)
library(rvest)
library(httr)
library(xml2)

options(stringsAsFactors = FALSE)

url <- "https://www.dshs.state.tx.us/news/updates.shtm#coronavirus"
#   Bad idea, disabling certificate check
page <- RCurl::getURL("https://www.dshs.state.tx.us/news/updates.shtm#coronavirus", ssl.verifyhost = 0L, ssl.verifypeer = 0L)

mytable <- read_html(page) %>% 
  html_nodes(xpath='//*[@id="ctl00_ContentPlaceHolder1_uxContent"]/div[2]/table') %>% 
  html_table()

mytable <- mytable[[1]]
names(mytable) <- c("County", "Cases")
mytable <- mytable %>% mutate(Date=lubridate::today())

# Get rid of footnotes
mytable$County <- str_replace(mytable$County, "\\d", "")

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

print(paste("Ending run ---------------------", lubridate::now()))
