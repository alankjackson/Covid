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
  html_nodes(xpath='/html/body/form/div[4]/div/div[3]/div[2]/div/div/div[2]/div/table') %>% 
  html_table()

mytable <- mytable[[1]]
names(mytable) <- c("County", "Cases")
mytable <- mytable %>% mutate(Date=lubridate::today())

# Get rid of footnotes
mytable$County <- str_replace(mytable$County, "\\d", "")


testing_status <- read_html(page) %>% 
  html_nodes(xpath='/html/body/form/div[4]/div/div[3]/div[2]/div/div/div[2]/table[1]') %>% 
  html_table()

testing_status <-   testing_status[[1]][["X2"]] %>% 
  str_remove_all("\\D") %>% 
  tibble(x=.) %>% 
  mutate(y=c("Total", "Public", "Private")) %>% 
  spread(key=y, value=x)

testing_status <- testing_status %>% mutate(Date=lubridate::today())

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

print(paste("Ending run ---------------------", lubridate::now()))
