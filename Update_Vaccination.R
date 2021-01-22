#
#    Update vaccination covid data
#

library(tidyverse)

#---------------------------------------------------------------------
#   Extract Vaccine information
#---------------------------------------------------------------------
cat("\n\n=============== Vaccine updates started =========\n\n")
print(lubridate::now())
cat("\n=============== Vaccine updates started =========\n\n")

this_day <- lubridate::today()

this_day <- lubridate::ymd("2021-01-06")

path <- "/home/ajackson/Dropbox/Rprojects/Covid/TexasDataXcel/"

Vaccine_path <- paste0(path, "Vaccine", this_day,".xlsx")

url <- "https://www.dshs.texas.gov/immunize/covid19/COVID-19-Vaccine-Data-by-County.xls"
#  is file still there?
if(!RCurl::url.exists(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)) {
  system(paste('echo ',url,' | mail -s "vaccine URL failure" alankjackson@gmail.com'))
  stop(">>>>>>>>>>>>>>>>>>>> url no longer works")
}

# Read in excel file and write back out again
Vaccine <- RCurl::getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
writeBin(Vaccine, Vaccine_path)

#  Read file into tibble

namekey <- c(County="County Name", 
             Region="Public Health Region (PHR)", 
             Doses_Alloc="Total Doses Allocated", 
             Doses_Admin="Vaccine Doses Administered", 
             People_one_dose="People Vaccinated with at least One Dose", 
             People_fully="People Fully Vaccinated", 
             Pop_Adult="Population, 16+",
             Pop_old="Population, 65+", 
             Pop_Healthcare="Population, Phase 1A Healthcare Workers",
             Pop_LongTerm="Population, Phase 1A Long-term Care Residents",
             Pop_1B_Medical="Population, Phase 1B Any Medical Condition")

foo <- readxl::read_excel(Vaccine_path, sheet=2)  

names(foo) <- namekey[names(foo)]

#  rename(County="County Name", 
#         Region="Public Health Region (PHR)", 
#         Doses_Alloc="Total Doses Allocated", 
#         Doses_Admin="Vaccine Doses Administered", 
#         People_one_dose="People Vaccinated with at least One Dose", 
#         People_fully="People Fully Vaccinated", 
#         Pop_Adult="Population, 16+",
#         Pop_old="", 
#         Pop_Healthcare=9) #%>% 
#
  #rename(County=1, Region=2, Doses_Distrib=3, Doses_Admin=4, People_one_dose=5, People_fully=6, Pop_Adult=7,
  #          Pop_old=8, Pop_Healthcare=9) %>% 
#   filter(County!="County", County!="Total") %>% 
  mutate(Date=this_day) %>% 
  mutate(County=str_replace_all(County, "[\r\n]" , "")) #%>% 
#   mutate(County=str_replace(County, "SanAugustine", "San Augustine"))
# foo$Cases <- as.numeric(foo$Cases)
# foo$Deaths <- as.numeric(foo$Deaths)
# 
head(foo)
tail(foo)

# Save the tibble file
saveRDS(foo,paste0("/home/ajackson/Dropbox/Rprojects/Covid/DailyBackups/",this_day,"_Vaccinate.rds"))

cat("\n\n=============== Vaccine updates finished =========\n\n")
print(lubridate::now())
cat("\n=============== Vaccine updates finished =========\n\n")


