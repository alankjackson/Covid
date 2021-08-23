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

#############
# this_day <- lubridate::ymd("2021-05-15")
#############

path <- "/home/ajackson/Dropbox/Rprojects/CovidTempData/TexasDataXcel/"

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

namekey <- c("County Name"="County", 
             "Public Health Region (PHR)"="Region", 
             "Total Doses Allocated"="Doses_alloc", 
             "Vaccine Doses Distributed"="Doses_alloc", 
             "Vaccine Doses Administered"="Doses_admin", 
             "People Vaccinated with at least One Dose"="People_one_dose", 
             "People Fully Vaccinated"="People_fully", 
             "Population\r\n12+"="Pop_teen",
             "Population, 16+"="Pop_adult",
             "Population, 65+"="Pop_old", 
             "Population, Phase 1A Healthcare Workers"="Pop_healthcare",
             "Population, Phase 1A Long-term Care Residents"="Pop_longterm",
             "Population, 16-64 Any Medical Condition"="Pop_to_64",
             "Population, 16-64\r\n Any Medical Condition"="Pop_to_64",
             "Population, Phase 1B Any Medical Condition"="Pop_1B_medical",
             "Population, 16-64"="Pop_16_64",
             "Any Medical Condition"="Any_Med_Cond",
             "Population, Education and Child Care Personnel"="Pop_Ed_ChildCare")

foo <- readxl::read_excel(Vaccine_path, sheet=2, na=c("", "--"))  
head(foo)

names(foo) <- namekey[names(foo)]

head(foo)
tail(foo)

# Save the tibble file
saveRDS(foo,paste0("/home/ajackson/Dropbox/Rprojects/CovidTempData/DailyBackups/",this_day,"_Vaccinate.rds"))

cat("\n\n=============== Vaccine updates finished =========\n\n")
print(lubridate::now())
cat("\n=============== Vaccine updates finished =========\n\n")


