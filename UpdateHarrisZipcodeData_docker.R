#   Read state Health Dept site for Covid update

# This version uses Docker
## https://docs.docker.com/engine/install/ubuntu/

print(paste("Starting run ---------------------", lubridate::now()))
library(tidyverse)
library(stringr)
library(rvest)
library(httr)
library(xml2)
library(RSelenium)
library(xfun)

options(stringsAsFactors = FALSE)

cat("\n\n=============== Harris Zipcode updates started =========\n\n")
print(lubridate::now())
cat("\n=============== Harris Zipcode updates started =========\n\n")

url <- "https://harriscounty.maps.arcgis.com/apps/opsdashboard/index.html#/31370c72d3844e6b962fcf8490718821"

this_day <- lubridate::today()
#---------------------------------------------------------------------
#   retrieve the webpage with a headless browser
#---------------------------------------------------------------------

system('docker run -d -p 4445:4444 selenium/standalone-firefox')

# start the server and browser in headless mode
               
rD <- remoteDriver(browserName="firefox",
                   port = 4445,
               extraCapabilities = list("moz:firefoxOptions" = list(
                 args = list('--headless')))
)

rD$open()

# navigate to an URL
rD$navigate(url)
Sys.sleep(9)

# get parsed page source
parsed_pagesource <- rD$getPageSource()[[1]]

#close the driver
rD$close()

#close the server
####  docker stop $(sudo docker ps -q)

#   Save in case the rest of the code crashes, like when they update the page on you
saveRDS(parsed_pagesource,paste0("/home/ajackson/Dropbox/Rprojects/Covid/DailyBackups/",this_day,"_ParsedPagesource.rds"))
#---------------------------------------------------------------------
#   Extract cases per zipcode
#---------------------------------------------------------------------

result <- read_html(parsed_pagesource) %>%
  html_nodes(xpath='//*[@id="ember56"]') %>%
  html_text() %>% 
  str_replace_all("\n"," ") %>% 
  str_split("  +")

result <- result[[1]][3:(length(result[[1]])-1)]

res <- cbind.data.frame(split(result, 
                              rep(1:2, times=length(result)/2)), 
                        stringsAsFactors=F)
names(res) <- c("Zip", "Cases") 
res$Cases <- str_remove(res$Cases, "Total Confirmed Cases:\\s*") 
res$Cases <- str_remove(res$Cases, ",") 
res$Cases <- as.numeric(res$Cases)

res <- res %>% mutate(Date=this_day-1) 

res

###HarrisZip <- res

#---------------------------------------------------------------------
#   Read in old data, append new data, and save
#---------------------------------------------------------------------

########################################################

# Read in the old data
HarrisZip <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/HarrisZip.rds")
# append the new data
HarrisZip <- bind_rows(HarrisZip, res)
# Save an accumulated file in case of a failure
saveRDS(HarrisZip,paste0("/home/ajackson/Dropbox/Rprojects/Covid/DailyBackups/",this_day,"_HarrisZip.rds"))
# Save the real file for later use
saveRDS(HarrisZip,"/home/ajackson/Dropbox/Rprojects/Covid/HarrisZip.rds")
# Also save to mirror site
saveRDS(HarrisZip,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/HarrisZip.rds")


cat("\n\n=============== Harris Zipcode updates finished =========\n\n")
print(lubridate::now())
cat("\n=============== Harris Zipcode updates finished =========\n\n")



print(paste("Ending run ---------------------", lubridate::now()))