#
#    Update prison covid data
#

library(tidyverse)

#---------------------------------------------------------------------
#   Extract Prison information
#---------------------------------------------------------------------

print("=============== Prison updates ==================")

url="https://www.tdcj.texas.gov/covid-19/offender_mac.html"

#  is file still there?

if(!RCurl::url.exists(url)) {
  system(paste('echo ',url,' | mail -s "prison URL failure" alankjackson@gmail.com'))
  stop(">>>>>>>>>>>>>>>>>>>> url no longer works")
}

tbls_ls <- xml2::read_html(url) %>%
  rvest::html_nodes("table") %>%
  rvest::html_table(fill = TRUE)

# Unpack all ten tables and combine where needed

col.names <- c("Pending_Tests",
               "Negative_Tests", 
               "Positive_Tests", 
               "Medical_Restriction",
               "Medical_Isolation")

df <- tbls_ls[[1]] %>% 
  rename(Unit=X1, Pending_Tests=X2) %>% 
  mutate(Unit=str_squish(Unit))

for (i in 2:5){
  tmp <- tbls_ls[[i]] %>% 
    rename(Unit=X1, !!col.names[i] := X2) %>% 
    mutate(Unit=str_squish(Unit))
  df <- left_join(df, tmp, by="Unit")
}

staff <- tbls_ls[[6]] %>% 
  rename(Unit=X1, Staff_Positive_Tests=X2) %>% 
  mutate(Unit=str_squish(Unit))

for (i in 7:10) {
  tmp <- tbls_ls[[i]] %>% 
    rename(Unit=X1, Staff_Positive_Tests=X2) %>% 
    mutate(Unit=str_squish(Unit))
  staff <- bind_rows(staff, tmp)
}
new_prisons <- left_join(df, staff, by="Unit")
new_prisons <- new_prisons %>% mutate(Date=lubridate::today()-1)

# Read in the old data
prisons <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Prisons.rds")

# Has anything changed?

new_totals <- new_prisons %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

totals <- prisons %>% 
  filter(Date==max(Date)) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)
  
if(sum(near(new_totals, totals))==6) { # no change. Abort
  system(paste('echo ',new_totals,' | mail -s "prison unchanged" alankjackson@gmail.com'))
  stop(">>>>>>>>>>>>>>>>>>> No change seen in file")
}

new_prisons

#---------------------------------------------------------------------
#   Read in old data, append new data, and save
#---------------------------------------------------------------------

################   Prison data

# append the new data
prisons <- bind_rows(prisons, new_prisons)

# Save an accumulated file in case of a failure
saveRDS(prisons ,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_Prisons.rds"))
# Save the real file for later use
saveRDS(prisons,"/home/ajackson/Dropbox/Rprojects/Covid/Prisons.rds")
# Also save to mirror site
saveRDS(prisons,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Prisons.rds")

print("=============== Prison updates finished =========")
