#
#    Update nursing home covid data
#

library(tidyverse)

#---------------------------------------------------------------------
#   Extract Tests information
#---------------------------------------------------------------------
cat("\n\n=============== Nursing Home updates started =========\n\n")
print(lubridate::now())
cat("\n=============== Nursing Home updates started =========\n\n")

path <- "/home/ajackson/Dropbox/Rprojects/Covid/TexasDataXcel/"

url <- "https://dshs.texas.gov/coronavirus/COVID-19OutbreaksinLong-termCareFacilities.xlsx"
#  is file still there?

if(!RCurl::url.exists(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)) {
  system(paste('echo ',url,' | mail -s "nursing home URL failure" alankjackson@gmail.com'))
  stop(">>>>>>>>>>>>>>>>>>>> url no longer works")
}

# Read in excel file and write back out again
NursingHome <- RCurl::getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
NursingHome_path <- paste0(path, "NursingHome", lubridate::today(),".xlsx")
writeBin(NursingHome, NursingHome_path)

#  Read file into tibble

foo <- readxl::read_excel(NursingHome_path)
##################################   stopped here waiting for correct file
# No data for May 5

my_colnames <- c(
  lubridate::ymd("2020-04-21")+0:13,  
  lubridate::ymd("2020-05-06")+0:(ncol(foo)-17))

my_colnames <- my_colnames %>% 
  format('%Y-%m-%d')
my_colnames <- c("County", my_colnames)

my_columns <- c(1:15, 17:ncol(foo))

foo <- foo %>% 
  rename_at(my_columns,~ my_colnames)

foo <- foo[2:258,] # delete bad rows
foo <- foo[-16] # delete bad column

foo <- foo %>% pivot_longer(-County, names_to="Date", values_to="Tests")

foo <- foo %>% mutate(Tests=as.numeric(Tests))

total_tests <- last(foo$Tests[foo$County=="TOTAL"])

testing_status <- tribble(
  ~Total, ~Public, ~Private,
  total_tests, NA, NA
)   

tail(foo)

testing_status <- testing_status %>% 
  mutate(Date=lubridate::today()-1) %>% 
  mutate(Total=as.character(Total))

# Has anything changed?

# Read in the old data
TestingData <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/NursingHome.rds")

if(last(TestingData$Total)==testing_status$Total) { # no change. Abort
  system(paste('echo ',testing_status$Total,' | mail -s "testing unchanged" alankjackson@gmail.com'))
  stop(">>>>>>>>>>>>>>>>>>> No change seen in file")
}



################   Testing data
# append the new data
TestingData <- bind_rows(TestingData, testing_status)
# Save an accumulated file in case of a failure
saveRDS(TestingData,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_Testing.rds"))
# Save the real file for later use
saveRDS(TestingData,"/home/ajackson/Dropbox/Rprojects/Covid/Testing.rds")
# Also save to mirror site
saveRDS(TestingData,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Testing.rds")

# Save county level file as well
saveRDS(foo,paste0("/home/ajackson/Dropbox/Rprojects/Covid/",lubridate::today(),"_County_Testing.rds"))
# Save the real file for later use
saveRDS(foo,"/home/ajackson/Dropbox/Rprojects/Covid/County_Testing.rds")

cat("\n\n=============== Testing updates finished =========\n\n")
print(lubridate::now())
cat("\n=============== Testing updates finished =========\n\n")


