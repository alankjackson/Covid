#
#    Update Schools covid data
#

library(tidyverse)

#---------------------------------------------------------------------
#   Extract Tests information
#---------------------------------------------------------------------
cat("\n\n=============== Schools updates started =========\n\n")
print(lubridate::now())
cat("\n=============== Schools updates started =========\n\n")

path <- "/home/ajackson/Dropbox/Rprojects/Covid/TexasDataXcel/"

filename <- paste0("state-level-data-file_", 
                   paste0(str_split(lubridate::today(), "-")[[1]][c(2,3,1)], collapse=""))

#url <- "https://dshs.texas.gov/chs/data/tea/statewide-school-covid-19-case-data/state-level-data-file_09232020/"
url <- paste0("https://dshs.texas.gov/chs/data/tea/statewide-school-covid-19-case-data/",filename,"/")

#  is file still there?
if(!RCurl::url.exists(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)) {
  system(paste('echo ',url,' | mail -s "Schools URL failure" alankjackson@gmail.com'))
  stop(">>>>>>>>>>>>>>>>>>>> url no longer works")
}

# Read in excel file and write back out again
Schools <- RCurl::getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
Schools_path <- paste0(path, "Schools", lubridate::today(),".xlsx")
writeBin(Schools, Schools_path)


cat("\n\n=============== Schools updates finished =========\n\n")
print(lubridate::now())
cat("\n=============== Schools updates finished =========\n\n")


