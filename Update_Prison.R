#
#    Update prison covid data
#

library(tidyverse)
library(stringr)
#library(rvest)
#library(httr)
#library(xml2)
#library(RSelenium)
library(xfun)

#---------------------------------------------------------------------
#   Extract Prison information
#---------------------------------------------------------------------

cat("\n\n=============== Prison updates started =========\n\n")
print(lubridate::now())
cat("\n=============== Prison updates started =========\n\n")

# Read in the old data
prisons <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Prisons.rds")

# What is newest date?

newest <- max(prisons$Date)

# If that is today, then bye-bye

if (newest==(lubridate::today())) { # no change. Abort
  system(paste('echo ',newest,' | mail -s "file already current" alankjackson@gmail.com'))
  stop(">>>>>>>>>>>>>>>>>>> file already current")
}

#url="https://www.tdcj.texas.gov/covid-19/offender_mac.html"
url <- "https://txdps.maps.arcgis.com/apps/opsdashboard/index.html#/dce4d7da662945178ad5fbf3981fa35c"


#  is file still there?

#if(!RCurl::url.exists(url)) {
#  system(paste('echo ',url,' | mail -s "prison URL failure" alankjackson@gmail.com'))
#  stop(">>>>>>>>>>>>>>>>>>>> url no longer works")
#}

#tbls_ls <- xml2::read_html(url) %>%
#  rvest::html_nodes("table") %>%
#  rvest::html_table(fill = TRUE)

#   Save this in case of catastrophic failure

#saveRDS(tbls_ls ,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_Prisons_rawtable.rds"))

# Unpack all ten tables and combine where needed
#print("--1--")
#
#col.names <- c("Recovered",
#               "Negative_Tests", 
#               "Positive_Tests", 
#               "Medical_Restriction",
#               "Medical_Isolation")
#
#df <- tbls_ls[[1]] %>% 
#  rename(Unit=X1, Recovered=X2) %>% 
#  mutate(Unit=str_squish(Unit))
#
#print("--2--")
#for (i in 3:6){
#  tmp <- tbls_ls[[i]] %>% 
#    rename(Unit=X1, !!col.names[i-1] := X2) %>% 
#    mutate(Unit=str_squish(Unit))
#  df <- left_join(df, tmp, by="Unit")
#}
#print("--3--")
#
#staff <- tbls_ls[[7]] %>% 
#  rename(Unit=X1, Staff_Positive_Tests=X2) %>% 
#  mutate(Unit=str_squish(Unit))
#
#print("--4--")
#for (i in 8:11) {
#  tmp <- tbls_ls[[i]] %>% 
#    rename(Unit=X1, Staff_Positive_Tests=X2) %>% 
#    mutate(Unit=str_squish(Unit))
#  staff <- bind_rows(staff, tmp)
#}
#print("--5--")
#new_prisons <- left_join(df, staff, by="Unit")
#new_prisons <- new_prisons %>% mutate(Date=lubridate::today()) %>% 
#  mutate(Pending_Tests=NA) %>% 
#  replace_na(list("Recovered"=0, "Positive_Tests"=0))
#
#
#print("--6--")
## Has anything changed?
#
#new_totals <- new_prisons %>%
#  summarise_if(is.numeric, sum, na.rm = TRUE)
#print("--7--")
#
#totals <- prisons %>% 
#  filter(Date==max(Date)) %>% 
#  select(-Pending_Tests) %>% 
#  summarise_if(is.numeric, sum, na.rm = TRUE)
#  
#print("--8--")
#if(sum(near(new_totals, totals))==6) { # no change. Abort
#  system(paste('echo ',new_totals,' | mail -s "prison unchanged" alankjackson@gmail.com'))
#  stop(">>>>>>>>>>>>>>>>>>> No change seen in file")
#}

# start the server and browser in headless mode
rD <- RSelenium::rsDriver(browser="firefox",
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

#   Save in case the rest of the code crashes, like when they update the page on you
saveRDS(parsed_pagesource,paste0("/home/ajackson/Dropbox/Rprojects/Covid/DailyBackups/",lubridate::today(),"_ParsedPagePrisons.rds"))

#---------------------------------------------------------------------
#   Extract prison info
#---------------------------------------------------------------------

result <- xml2::read_html(parsed_pagesource) %>%
  # select out the part of the page you want to capture
  rvest::html_nodes(xpath='//*[@id="ember194"]') %>%
  # convert it to a really long string, getting rid of html
  rvest::html_text() %>% 
  # there are a lot of carriage returns in there, let's clean them out
  str_replace_all("\n"," ") %>% 
  # Split string on long strings of spaces, returning a list
  str_split("  +")

 
# get rid of title and extra line at end
result <- result[[1]][3:(length(result[[1]])-1)]

# every other element of list is a Unit, so let's combine the Unit name
# with the table it used to head, to get the first iteration of a data frame
res <- cbind.data.frame(split(result, 
                              rep(1:2, times=length(result)/2)), 
                        stringsAsFactors=F)
#assign some better names
names(res) <- c("Unit", "foo") 

res <- res %>% 
  # add dash after numbers for later splitting
  mutate(foo=str_replace_all(foo, "(\\d) ", "\\1 -")) %>% 
  # remove all whitespace, some are tabs
  mutate(foo=str_remove_all(foo, "\\s*")) %>% 
  # remove commas from numbers
  mutate(foo=str_remove_all(foo, ",")) %>% 
  # split the field into 12 pieces
  separate(foo, letters[1:12], sep="-") %>% 
  # select out the numeric fields
  select(Unit, b,d,f,h,j,l) %>% 
  # make them numeric
  mutate_at(c("b","d","f","h","j","l"), as.numeric)

# give every field a bright, shiny new name
names(res) <- c("Unit", 
                "Offender Active Cases",
                "Offender Recovered",
                "Employee Active Cases",
                "Employee Recovered",
                "Medical Restriction",
                "Medical Isolation")


# add a field with today's date
res <- res %>% mutate(Date=lubridate::today()) 

# let's see what it looks like - this is for QC
res

#---------------------------------------------------------------------
#   Read in old data, append new data, and save
#---------------------------------------------------------------------

################   Prison data

# append the new data
##prisons <- bind_rows(prisons, new_prisons)

# Save an accumulated file in case of a failure
saveRDS(res ,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_Prisons_2.rds"))
# Save the real file for later use
#saveRDS(prisons,"/home/ajackson/Dropbox/Rprojects/Covid/Prisons_2.rds")
# Also save to mirror site
#saveRDS(prisons,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Prisons.rds")


cat("\n\n=============== Prison updates finished =========\n\n")
print(lubridate::now())
cat("\n=============== Prison updates finished =========\n\n")
