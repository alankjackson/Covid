#
#    Update testing covid data
#

library(tidyverse)

#---------------------------------------------------------------------
#   Extract Tests information
#---------------------------------------------------------------------
cat("\n\n=============== Testing updates started =========\n\n")
print(lubridate::now())
cat("\n=============== Testing updates started =========\n\n")

this_day <- lubridate::today()

#this_day <- lubridate::ymd("2021-01-21")

path <- "/home/ajackson/Dropbox/Rprojects/CovidTempData/TexasDataXcel/"
#               legacy file pre 9/13
#url <- "https://dshs.texas.gov/coronavirus/TexasCOVID-19CumulativeTestsOverTimebyCounty.xlsx"
url <- "https://dshs.texas.gov/coronavirus/TexasCOVID-19CumulativeTestsbyCounty.xlsx"
#  is file still there?

if(!RCurl::url.exists(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)) {
  system(paste('echo ',url,' | mail -s "tests URL failure" alankjackson@gmail.com'))
  stop(">>>>>>>>>>>>>>>>>>>> url no longer works")
}

# Read in excel file and write back out again
Tests <- RCurl::getBinaryURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
Tests_path <- paste0(path, "Tests_by_County_", this_day,".xlsx")
writeBin(Tests, Tests_path)

#  Read file into tibble

foo <- readxl::read_excel(Tests_path)

# No data for May 5

#my_colnames <- c(
#  lubridate::ymd("2020-04-21")+0:13,  
#  lubridate::ymd("2020-05-06")+0:(ncol(foo)-17))
#
#my_colnames <- my_colnames %>% 
#  format('%Y-%m-%d')
#my_colnames <- c("County", my_colnames)
#
#my_columns <- c(1:15, 17:ncol(foo))

my_colnames <- lubridate::ymd("2020-09-13")+0:(ncol(foo)-2)
my_colnames <- my_colnames %>% 
  format('%Y-%m-%d')
my_colnames <- c("County", my_colnames)
my_columns <- c(1:ncol(foo))

foo <- foo %>% 
  rename_at(my_columns,~ my_colnames)

foo <- foo[2:257,] # delete bad rows
#foo <- foo[-16] # delete bad column

foo <- foo %>% pivot_longer(-County, names_to="Date", values_to="Tests")

foo <- foo %>% mutate(Tests=as.numeric(Tests))
foo <- foo %>% mutate(Date=lubridate::as_date(Date))

total_tests <- last(foo$Tests[foo$County=="Total"])

testing_status <- tribble(
  ~Total, ~Public, ~Private,
  total_tests, NA, NA
)   

tail(foo)

testing_status <- testing_status %>% 
  mutate(Date=lubridate::today()) %>% 
  mutate(Total=as.character(Total))

# Has anything changed?

# Read in the old data
TestingData <- readRDS("/home/ajackson/Dropbox/Rprojects/Covid/Testing.rds")

if(last(TestingData$Total)==testing_status$Total) { # no change. Abort
  system(paste('echo ',testing_status$Total,' | mail -s "testing unchanged" alankjackson@gmail.com'))
  stop(">>>>>>>>>>>>>>>>>>> No change seen in file")
}

Legacy_data <- readRDS("/home/ajackson/Dropbox/Rprojects/CovidTempData/DailyBackups/2020-09-13_County_Testing.rds")
Legacy_data <- Legacy_data %>% mutate(Date=lubridate::as_date(Date))

foo <- rbind(Legacy_data, foo)


################   Testing data
# append the new data
TestingData <- bind_rows(TestingData, testing_status)
# Save an accumulated file in case of a failure
saveRDS(TestingData,paste0("/home/ajackson/Dropbox/Rprojects/CovidTempData/DailyBackups/",this_day,"_Testing.rds"))
# Save the real file for later use
saveRDS(TestingData,"/home/ajackson/Dropbox/Rprojects/Covid/Testing.rds")
# Also save to mirror site
saveRDS(TestingData,"/home/ajackson/Dropbox/mirrors/ajackson/Covid/Testing.rds")

# Save county level file as well
saveRDS(foo,paste0("/home/ajackson/Dropbox/Rprojects/CovidTempData/DailyBackups/",this_day,"_County_Testing.rds"))
saveRDS(foo,paste0("/home/ajackson/Dropbox/Rprojects/Covid/Today_Data/Today_County_Testing.rds"))
saveRDS(foo,paste0("/home/ajackson/Dropbox/mirrors/ajackson/Covid/Today_County_Testing.rds"))
# Save the real file for later use
saveRDS(foo,"/home/ajackson/Dropbox/Rprojects/Covid/County_Testing.rds")

cat("\n\n=============== Testing updates finished =========\n\n")
print(lubridate::now())
cat("\n=============== Testing updates finished =========\n\n")


