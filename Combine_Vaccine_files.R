#   
#           Combine daily vaccine files
#

cat("\n\n=============== Vaccine combine started =========\n\n")
print(lubridate::now())
cat("\n=============== Vaccine combine started =========\n\n")

library(tidyverse)

this_day <- lubridate::today()

# Get a list of files

in_path <- "/home/ajackson/Dropbox/Rprojects/CovidTempData/DailyBackups/"
out_path <-  "/home/ajackson/Dropbox/Rprojects/Covid/"

# df <-
#   list.files(path = in_path, pattern = "*Vaccinate*", full.names = T) %>% 
#   map(read_rds) %>% 
#   #map(select,c("County", "Region", "Doses_distrib", "Doses_admin", "People_one_dose", 
#   #"People_fully", "Pop_adult", "Pop_old", "Pop_healthcare", "Date")) %>% 
#   reduce(bind_rows)
#   #reduce(rbind)

df <- NULL

for (filename in list.files(path = in_path, pattern = "*Vaccinate*", full.names = T)){
  foo <- read_rds(filename)
  foo$Date <- str_extract(filename, "202[0-9-]+")
  print(paste("----------", filename))
  print(head(foo))
  df <- bind_rows(df, foo)
  print(names(df))
}

df$Date <- lubridate::as_date(df$Date)



# Create some new quantities and clean up data

df <- df %>% 
  select(County, Date, Doses_alloc, 
         Doses_admin, People_one_dose, 
         People_fully, contains("Pop")) %>% 
  # Calculate daily numbers
  group_by(County) %>% 
    arrange(Date) %>% 
    mutate(Daily_adm=(Doses_admin-lag(Doses_admin, default=Doses_admin[1]))) %>%
    mutate(Daily_dist=(Doses_alloc-lag(Doses_alloc, default=Doses_alloc[1]))) %>%
  ###mutate(new_cases=pmax(new_cases, 0)) %>% # truncate negative numbers
  #  Calculate date of 100% vaccinated based on current rate, per county
    mutate(Date_twodose=(Date + (2*Pop_adult-Doses_admin)/Daily_adm)) %>% 
    mutate(Pct_given=signif(100*Doses_admin/Doses_alloc,3)) %>% 
    mutate(Pct_given=na_if(Pct_given, Inf)) %>% 
  ungroup()


df$Pct_given[is.nan(df$Pct_given)] <- NA

head(df)
tail(df)

df %>% 
  filter(County=="Harris") %>% 
  tail()

saveRDS(df, paste0(out_path, "Vaccinations.rds"))

cat("\n\n=============== Vaccine combine ended =========\n\n")
print(lubridate::now())
cat("\n=============== Vaccine combine ended =========\n\n")
