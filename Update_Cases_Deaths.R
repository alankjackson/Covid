#   Read state Health Dept site for Covid update

print(paste("Starting run ---------------------", lubridate::now()))
library(tidyverse)
library(stringr)
library(lubridate)
#library(RCurl)
#library(rvest)
#library(httr)
#library(xml2)
#library(RSelenium)
#library(xfun)
#library(readxl)

options(stringsAsFactors = FALSE)

cat("\n\n=============== Cases and Deaths updates started =========\n\n")
print(lubridate::now())
cat("\n=============== Cases and Deaths updates started =========\n\n")

check_url <- function(url, mail_message, action=c("stop", "warn")) {
  #  is file still there?
  
  if(!RCurl::url.exists(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)) {
    system(paste('echo ',url,' | mail -s \"',mail_message,'\" alankjackson@gmail.com'))
    if(action=="stop") {
      stop(">>>>>>>>>>>>>>>>>>>> url no longer works")
    } else {
      print(mail_message)
      return(FALSE)
    }
  }
  return(TRUE)
}
#
#---------------------------------------------------------------------
#   Extract cases per county
#---------------------------------------------------------------------

#---------------------------------------------------------------------
#   Read in excel files from state and save
#---------------------------------------------------------------------

#--------------------  County Case and Fatality data for today
url <- "https://dshs.texas.gov/coronavirus/TexasCOVID19CaseCountData.xlsx"
fail_check <- check_url(url, "URL failure for Case Data", "stop")
casecounts <- RCurl::getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

path <- "/home/ajackson/Dropbox/Rprojects/Covid/TexasDataXcel/"
casecounts_path <- paste0(path, "Cases_by_County_", lubridate::today(),".xlsx")
writeBin(casecounts, casecounts_path)

#--------------------  County Death data
url <- "https://dshs.texas.gov/coronavirus/TexasCOVID19DailyCountyFatalityCountData.xlsx"
fail_check <- check_url(url, "URL failure for Death Data", "warn")

if (fail_check) {
  Deaths <- RCurl::getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  path <- "/home/ajackson/Dropbox/Rprojects/Covid/TexasDataXcel/"
  writeBin(Deaths, paste0(path, "Deaths_by_County_", lubridate::today(),".xlsx"))
}

#--------------------  County Cumulative Case data
url <- "https://dshs.texas.gov/coronavirus/TexasCOVID19DailyCountyCaseCountData.xlsx"
fail_check <- check_url(url, "URL failure for Cumulative County Data", "warn")
if (fail_check) {
  DailyCounts <- RCurl::getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  path <- "/home/ajackson/Dropbox/Rprojects/Covid/TexasDataXcel/"
  writeBin(DailyCounts, paste0(path, "County_Case_Data_", lubridate::today(),".xlsx"))
}

#   Cases and Deaths

foo <- readxl::read_excel(casecounts_path) %>% 
  rename(County=1, Cases=2, Deaths=3) %>% 
  filter(County!="County", County!="Total") %>% 
  mutate(Date=lubridate::today()) %>% 
  mutate(County=str_replace_all(County, "[\r\n]" , "")) %>% 
  mutate(County=str_replace(County, "SanAugustine", "San Augustine"))
foo$Cases <- as.numeric(foo$Cases)
foo$Deaths <- as.numeric(foo$Deaths)

head(foo)
tail(foo)

# Has anything changed?

# Read in the old data
CovidData <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Covid.rds")

# Sum up cases for the last date in file

case_sum <- CovidData %>% 
  group_by(Date) %>% 
    summarise(casestotal=sum(Cases)) %>% 
  ungroup() %>% 
  tail(1)
  

if(case_sum$casestotal==sum(foo$Cases)) { # no change. Abort
  system(paste('echo ',testing_status$Total,' | mail -s "cases unchanged" alankjackson@gmail.com'))
  stop(">>>>>>>>>>>>>>>>>>> No change seen in file")
}


#  emergency backup file
saveRDS(foo,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_mytable.rds"))
# Read in the old data
CovidData <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Covid.rds")

# append the new data
CovidData <- bind_rows(CovidData, foo)

# Save an accumulated file in case of a failure
saveRDS(CovidData,paste0("/home/ajackson/Dropbox/Rprojects/Covid/DailyBackups/",lubridate::today(),"_Covid.rds"))
# Save the real file for later use
saveRDS(CovidData,"/home/ajackson/Dropbox/Rprojects/Covid/Covid.rds")
# Also save to mirror site
saveRDS(CovidData,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Covid.rds")


cat("\n\n=============== Cases and Deaths updates finished =========\n\n")
print(lubridate::now())
cat("\n=============== Cases and Deaths updates finished =========\n\n")


print(paste("Ending run ---------------------", lubridate::now()))
