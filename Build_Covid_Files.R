
#   Build the time-consuming files required by the Covid app

#   outputs are:
#      Counties - County level data with all calculations
#    County_pop - Counties and Population, largest first
#   TestingData - Total tests by date
#          MSAs - Metro areas with all calculations
#       MSA_raw - Metro areas, counties, and Population
#   Prison_data - All data by prison
#  Prison_county - All prison data by county
#    MappingData - Data for drawing map
#      MapLabels - Labels for each county on map

library(tidyverse)
library(stringr)
library(lubridate)
library(broom)
library(purrr)


cat("\n\n=============== Build Covid Files started =========\n\n")
print(lubridate::now())
cat("\n=============== Build Covid Files started =========\n\n")


###################################
#   get and set up the basic data
###################################

#   Directory where data is stored
DataLocation <- "/home/ajackson/Dropbox/Rprojects/Covid/"
DataArchive <- "/home/ajackson/Dropbox/mirrors/ajackson/SharedData/"

#   Case data
DF <- readRDS(paste0(DataLocation, "Covid.rds"))

#   Testing data
TestingData <- readRDS(paste0(DataLocation, "Testing.rds"))
TestingData$Total <- as.numeric(TestingData$Total)

#   County Population data
County_pop <- readRDS(paste0(DataArchive, "Census_July1_2019_TexasCountyPop.rds"))

#   Prison Population data
Prison_pop <- readRDS(paste0(DataArchive, "Prison_Pop2020.rds"))

#   Prison Location data
Prison_loc <- readRDS(paste0(DataArchive, "Prison_Locations.rds"))

#   Current Prison epidemic data
Prison_covid <- readRDS(paste0(DataLocation, "Prisons.rds"))

#   MSA data
MSA_raw <- readRDS(paste0(DataArchive, "Texas_MSA_Pop_Counties.rds")) 
#  Put tiny, small, and moderate at bottom
bottom <- MSA_raw %>% filter(MSA %in% c("tiny", "small", "moderate")) %>% 
  arrange(desc(MSA))
top <-  MSA_raw %>% filter(!(MSA %in% c("tiny", "small", "moderate"))) %>% 
  arrange(MSA)
MSA_raw <- bind_rows(top, bottom)
#    Add Texas to the top
MSA_raw <- MSA_raw %>% 
  add_row(MSA="Texas", Population=27864555, Counties=list("Texas"),
          .before=1)

#   County polygons
Texas <- readRDS(paste0(DataArchive, "Texas_County_Outlines_lowres.rds"))

#global_slope <- 0.13
# https://dartthrowingchimp.shinyapps.io/covid19-app/

print("--1--")

# Clean up footnotes

DF$County <- str_replace(DF$County, "\\d", "")

# Clean up bad county name

DF <- DF %>% 
  mutate(County=str_replace_all(County, "[\r\n]" , "")) %>%  
  mutate(County=str_replace(County, "SanAugustine" , "San Augustine"))

# drop rows with zero or NA cases

DF <- DF %>% filter(Cases>0, !is.na(Cases))

Prison_counties <- c("Jones", "Anderson", "Walker", "Medina", "Rusk",
                     "Grimes", "Coryell", "Houston", "Pecos", "Angelina",
                     "Bowie", "Jefferson", "Brazoria")

#   De-step

DF <- DF %>% 
  mutate(Raw_cases=Cases) %>% 
  arrange(County) %>% 
  group_by(County) %>% 
    mutate(delta=Cases-lag(Cases)) %>% 
    replace_na(list(delta=0)) %>% 
    mutate(Threshold=as.numeric((abs(delta)>25)&(abs(delta)/Cases>0.10))*delta) %>% 
    mutate(Threshold=cumsum(Threshold)) %>%  
  ungroup() %>% 
  mutate(Cases=ifelse(County %in% Prison_counties, 
                      Raw_cases-Threshold, 
                      Raw_cases)) %>% 
  select(-Threshold, -delta)

# Add Statewide Totals per day

DF <- DF %>% bind_rows(
  DF %>%
    group_by(Date) %>% 
    summarise(Cases = sum(Cases), Deaths=sum(Deaths)) %>% 
    mutate(County="Total")
) %>% 
  arrange(Date)

# Calc days since March 10, the starting point for everything

DF <- DF %>% 
  mutate(Days=as.integer(Date-ymd("2020-03-10")))

# Fix Deaths field

DF$Deaths <- str_replace(DF$Deaths,"-", "na")

DF <- DF %>% 
  mutate(Deaths=as.numeric(Deaths)) %>% 
  replace_na(list(Deaths=0))

##########################################
# Keep prison and county data separate until more is known
##########################################

print("--2--")
# Calculate new cases

DF <- DF %>% 
  group_by(County) %>% 
  arrange(Date) %>% 
  mutate(new_cases=(Cases-lag(Cases, default=Cases[1]))) %>%
  mutate(new_cases=pmax(new_cases, 0)) %>% # truncate negative numbers
  mutate(new_deaths=(Deaths-lag(Deaths, default=Deaths[1]))) %>%
  mutate(new_deaths=pmax(new_deaths, 0)) %>% # truncate negative numbers
  ungroup() %>% 
  left_join(County_pop, by="County") 

# Add cases and deaths to MSA

MSA <- MSA_raw %>% 
  unnest(Counties) %>% 
  rename(County=Counties) %>% 
  left_join(DF, ., by="County") %>% 
  select(-Population.x) %>% 
  rename(Population=Population.y) %>% 
  group_by(MSA, Date) %>% 
  summarise(Cases=sum(Cases, na.rm = TRUE),
            Deaths=sum(Deaths, na.rm = TRUE),
            new_cases=sum(new_cases, na.rm = TRUE),
            new_deaths=sum(new_deaths, na.rm = TRUE),
            Population=unique(Population)) %>% 
  ungroup() %>% 
  replace_na(list(MSA="Texas"))

MSA$Population[MSA$MSA=="Texas"] <- 28995881 

print("--3--")

#   Sort counties with 20 largest first, then alphabetical

ByPop <- arrange(County_pop, -Population)
ByAlpha <- arrange(ByPop[21:nrow(ByPop),], County)
County_pop <- bind_rows(ByPop[1:20,], ByAlpha)
ByPop <- ByAlpha <- NULL


# https://docs.google.com/document/d/1ETeXAfYOvArfLvlxExE0_xrO5M4ITC0_Am38CRusCko/edit#
Disease <- tibble::tribble(
  ~Demographics, "% Hosp", "% Hosp ICU", "% CFR",
  "12%", "0-9", "0.1%", "5.0%", "0.002%",
  "13%", "10-19", "0.3%", "5.0%", "0.006%",
  "14%", "20-29", "1.2%", "5.0%", "0.03%",
  "13%", "30-39", "3.2%", "5.0%", "0.08%",
  "12%", "40-49", "4.9%", "6.3%", "0.15%",
  "13%", "50-59", "10.2%", "12.2%", "0.60%",
  "11%", "60-69", "16.6%", "27.4%", "2.20%",
  "7%", "70-79", "24.3%", "43.2%", "5.10%",
  "4%", "80+", "27.3%", "70.9%", "9.30%"
)
meat_packing <- tribble(
  ~City,          ~County,     ~Company, ~Employees,
  "Cactus",       "Moore",       "JBS",    3000,
  "Dalhart",      "Dallam",      "JBS",    2200,
  "Lufkin",       "Angelina",    "JBS",    1500,
  "Mt. Pleasant", "Titus",       "JBS",    3200,
  "Waco",         "McLennan",    "JBS",     450,
  "Nacogdoches",  "Nacogdoches", "JBS",    1500,
  "Amarillo",     "Potter",      "Tyson",  3500,
  "Carthage",     "Panola",      "Tyson",   575,
  "Center",       "Shelby",      "Tyson",  2000,     
  "Dallas",       "Dallas",      "Tyson",    NA,   
  "Fort Worth",   "Tarrant",     "Tyson",    NA,        
  "Haltom City",  "Tarrant",     "Tyson",    NA, 
  "Houston",      "Harris",      "Tyson",    NA, 
  "N Richland H", "Tarrant",     "Tyson",   500,          
  "Seguin",       "Guadelupe",   "Tyson",   700,       
  "Sherman",      "Grayson",     "Tyson",  1600,
  "Vernon",       "Wilbarger",   "Tyson",   500,
  "Nixon",        "Gonzales",    "Holmes Foods",     300,
  "Waco",         "McLennan",    "Cargill",          650,
  "Waco",         "McLennan",    "Sanderson Farms", 1100,
  "Fort Worth",   "Tarrant",     "Cargill",          300,
  "Friona",       "Parmer",      "Cargill",         2000,
  "Palestine",    "Anderson",    "Sanderson Farms", 1100,
  "Bryan",        "Brazos",      "Sanderson Farms", 1400,
  "Tyler",        "Smith",       "Sanderson Farms", 1400)

######################################################################
#          Calculate doubling times along whole vector
######################################################################
doubling <- function(cases, window=5, grouper) {
  print("---->>>>")
  print(grouper[1])
  if (length(cases)<10){ # must have >= 10 points
    return(rep(NA,length(cases)))
  }
  halfwidth <- as.integer(window/2)
  rolling_lm <- tibbletime::rollify(.f = function(logcases, Days) {
    lm(logcases ~ Days)
  }, 
  window = window, 
  unlist = FALSE) 
  
  foo <- 
    tibble(Days = 1:length(cases), logcases = log10(cases)) %>%
    mutate(roll_lm = rolling_lm(logcases, Days)) %>% 
    filter(!is.na(roll_lm)) %>%
    mutate(tidied = purrr::map(roll_lm, broom::tidy)) %>%
    unnest(tidied) %>%
    filter(term=="Days") %>% 
    mutate(m=estimate) %>% 
    #   calculate doubling time
    mutate(m=signif(log10(2)/m,3)) %>% 
    mutate(m=replace(m, m>200, NA)) %>%  
    mutate(m=replace(m, m<=0, NA)) %>% 
    select(m)
  return(matlab::padarray(foo[[1]], c(0,halfwidth), "replicate"))
}  ######################  end doubling

######################################################################
#     Calculate a rolling average
######################################################################
attribute <- function(foo, attribute, grouper){
  #   group = grouping variable (County)
  #   attribute = column in data frame to rolling average
  foo %>% 
    group_by(!!grouper) %>% 
    arrange(Date) %>% 
    #mutate(avg=zoo::rollmean(!!attribute, window, 
    #                         fill=c(0, NA, last(!!attribute)))) %>% 
    mutate(avg=zoo::rollapply(!!attribute, window, 
                              FUN=function(x) mean(x, na.rm=TRUE),
                              partial=TRUE,
                              fill=c(0, NA, last(!!attribute)))) %>% 
    ungroup()
  
} ###################### end rolling average

######################################################################
#   trim outliers
######################################################################

isnt_out_z <- function(x, thres = 8, na.rm = TRUE) {
  good <- abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
  x[!good] <- NA # set outliers to na
  x[x<=0] <- NA # non-positive values set to NA
  x
}

#---------------------------------------------------    
#------------------- County Data -------------------
#---------------------------------------------------    

#---------------  Control matrix, available everywhere

calc_controls <<- tribble(
  ~base,             ~avg, ~percap, ~trim, ~positive,
  "Cases",           TRUE,  TRUE,  FALSE,  TRUE,
  "Deaths",          TRUE,  TRUE,  FALSE,  TRUE,
  "pct_chg",         TRUE, FALSE,  FALSE,  TRUE,
  "doubling",        TRUE, FALSE,   TRUE,  TRUE,
  "active_cases",    TRUE,  TRUE,  FALSE,  TRUE,
  "deaths_percase", FALSE, FALSE,   TRUE,  TRUE,
  "new_cases",       TRUE,  TRUE,  FALSE,  TRUE,
  "new_deaths",      TRUE,  TRUE,  FALSE,  TRUE
)

print("--4--")
######################################################################
#       Calculate everything by grouping variable (County, MSA)
######################################################################
prep_counties <- function(DF, Grouper) { 
  
  window <- 5
  Grouper_str <- Grouper
  Grouper <- rlang::sym(Grouper)
  
  #---------------  Clean up and calc base quantities
  foo <- DF %>%     
    group_by(!!Grouper) %>% 
      arrange(Date) %>% 
      mutate(day = row_number()) %>% 
      add_tally() %>% 
    ungroup() %>% 
    select(!!Grouper, Cases, Deaths, Date, new_cases, new_deaths, Population, n) %>% 
    filter(!!Grouper!="Total") %>% 
    filter(!!Grouper!="Pending County Assignment") %>% 
    replace_na(list(Cases=0, Deaths=0, new_cases=0, new_deaths=0)) %>% 
    group_by(!!Grouper) %>%
      arrange(Date) %>% 
      mutate(pct_chg=100*new_cases/lag(Cases, default=Cases[1])) %>%
      mutate(active_cases=Cases-lag(Cases, n=14, default=0)) %>%
      mutate(deaths_percase=Deaths/Cases) %>%
      mutate(doubling=doubling(Cases, window, !!Grouper)) %>% 
    ungroup() 
  
  #----------------- Trim outliers and force to be >0
  
  for (base in calc_controls$base[calc_controls$trim]){
    for (grp in unique(foo[[Grouper_str]])) {
      foo[foo[[Grouper_str]]==grp,][base] <- isnt_out_z((foo[foo[[Grouper_str]]==grp,][[base]]))
    }
  }
  for (base in calc_controls$base[calc_controls$positive]){
    foo[base] <- pmax(0, foo[[base]])
  }
  
  #----------------- Calc Rolling Average
  
  inputs <- calc_controls$base[calc_controls$avg==TRUE]
  
  foo <- foo %>% 
    group_by(!!Grouper) %>% 
    mutate_at(inputs, list(avg = ~ zoo::rollapply(., window, 
                                                  FUN=function(x) mean(x, na.rm=TRUE),
                                                  fill=c(first(.), NA, last(.))))) %>% 
    rename_at(vars(ends_with("_avg")), 
              list(~ paste("avg", gsub("_avg", "", .), sep = "_")))
  
  foo <- foo %>% 
    mutate(pct_chg=na_if(pct_chg, 0)) %>% 
    mutate(pct_chg=replace(pct_chg, pct_chg>30, NA)) %>% 
    mutate(pct_chg=replace(pct_chg, pct_chg<0.1, NA)) %>% 
    mutate(avg_pct_chg=na_if(avg_pct_chg, 0)) %>% 
    mutate(avg_pct_chg=replace(avg_pct_chg, avg_pct_chg>30, NA)) %>% 
    mutate(avg_pct_chg=replace(avg_pct_chg, avg_pct_chg<0.1, NA))
  
  #----------------- Calc per capitas
  
  inputs <- calc_controls$base[calc_controls$percap==TRUE]
  inputs <- c(paste0("avg_", inputs), inputs)
  
  foo <- foo %>% 
    mutate_at(inputs, list(percap = ~ . / Population * 1.e5)) 
  
  return(foo)
  
} ###############  end of prep_counties

############   Create Counties file
County_calc <- prep_counties(DF, "County")
############   Create MSAs file
MSAs <- prep_counties(MSA, "MSA")


span <- function(vector){
  foo <- range(vector, na.rm=TRUE)
  return(max(foo) - min(foo))
}
print("--7--")
#---------------------------------------------------    
#------------------- Prison Data -------------------
#---------------------------------------------------    

Prison <- left_join(Prison_loc, Prison_pop, by="Unit_Name")

Prison <- Prison %>% 
  select(Unit_Name, County, Population)

Prison_load <- Prison %>% # inmates per county
  group_by(County) %>% 
    summarise(Population=sum(Population)) %>% 
  ungroup

Prison_covid <- Prison_covid %>% 
  mutate(Unit=str_replace(Unit, "ETTF", "East Texas")) %>% 
  mutate(Unit=str_replace(Unit, "Fort Stockton", "Ft. Stockton")) %>% 
  mutate(Unit=str_replace(Unit, "Jester 1", "Jester I")) %>% 
  mutate(Unit=str_replace(Unit, "Jester 3", "Jester III")) %>% 
  mutate(Unit=str_replace(Unit, "Jester 4", "Jester IV")) %>% 
  mutate(Unit=str_replace(Unit, "Sansaba", "San Saba")) %>% 
  filter(Unit!="No Longer in Custody") %>% 
  filter(Unit!="Bambi") %>% 
  mutate(Cases=Positive_Tests+Recovered)

Prison_covid <- left_join(Prison_covid, Prison, by=c("Unit"="Unit_Name"))

Prison_covid <- Prison_covid %>% 
  group_by(Unit) %>% 
    arrange(Cases) %>% 
    mutate(new_cases=(Cases-lag(Cases, default=Cases[1]))) %>%
    mutate(new_cases=pmax(new_cases, 0)) %>% # truncate negative numbers
  ungroup()

#   What if update to website got delayed? Just delete last date

if (sum(Prison_covid$new_cases[Prison_covid$Date==max(Prison_covid$Date)])==0){
  Prison_covid <- Prison_covid %>% filter(!Date==max(Prison_covid$Date))
}

print("--9--")
prep_prisons <- function() { 
  
  window <- 5
  #---------------  Control matrix
  
  calc_controls <- tribble(
    ~base,       ~avg, ~percap, ~trim, ~positive,
    "Cases",      TRUE, TRUE,  FALSE, TRUE,
    "pct_chg",    TRUE, FALSE, FALSE, TRUE,
    "doubling",   TRUE, FALSE, TRUE, TRUE,
    "new_cases",  TRUE, TRUE,  FALSE, TRUE
  )
  
  #---------------  Clean up and calc base quantities
  foo <- Prison_covid %>%     
    # Start each county at 10 cases
    filter(Cases>10) %>%  
    group_by(Unit) %>% 
    arrange(Date) %>% 
    mutate(day = row_number()) %>% 
    add_tally() %>% 
    ungroup() %>% 
    filter(n>5) %>% # must have at least 5 datapoints
    select(Unit, Cases, Date, new_cases, Population, County) %>% 
    group_by(Unit) %>%
    arrange(Date) %>% 
    mutate(pct_chg=100*new_cases/lag(Cases, default=Cases[1])) %>% 
    mutate(doubling=doubling(Cases, window, Unit)) %>% 
    ungroup()
  
  #----------------- Trim outliers and force to be >0
  
  for (base in calc_controls$base[calc_controls$trim]){
    for (unit in unique(foo$Unit)) {
      foo[foo$Unit==unit,][base] <- isnt_out_z((foo[foo$Unit==unit,][[base]]))
    }
  }
  for (base in calc_controls$base[calc_controls$positive]){
    foo[base] <- na_if(foo[base], 0)
  }
  
  #----------------- Calc Rolling Average
  
  inputs <- calc_controls$base[calc_controls$avg==TRUE]
  
  foo <- foo %>% 
    group_by(Unit) %>% 
    mutate_at(inputs, list(avg = ~ zoo::rollmean(., window, 
                                                 fill=c(first(.), NA, last(.))))) %>% 
    rename_at(vars(ends_with("_avg")), 
              list(~ paste("avg", gsub("_avg", "", .), sep = "_")))
  
  foo <- foo %>% 
    mutate(pct_chg=na_if(pct_chg, 0)) %>% 
    mutate(pct_chg=replace(pct_chg, pct_chg>30, NA)) %>% 
    mutate(pct_chg=replace(pct_chg, pct_chg<0.1, NA)) %>% 
    mutate(avg_pct_chg=na_if(avg_pct_chg, 0)) %>% 
    mutate(avg_pct_chg=replace(avg_pct_chg, avg_pct_chg>30, NA)) %>% 
    mutate(avg_pct_chg=replace(avg_pct_chg, avg_pct_chg<0.1, NA))
  
  #----------------- Calc per capitas
  
  inputs <- calc_controls$base[calc_controls$percap==TRUE]
  inputs <- c(paste0("avg_", inputs), inputs)
  
  foo <- foo %>% 
    mutate_at(inputs, list(percap = ~ . / Population * 100)) 
  
  Prison_data <<- foo
  
}

prep_prisons()

print("--10--")
Prison_county <- Prison_data %>% 
  group_by(Date, County) %>% 
    summarise(Population=sum(Population),
              Cases=sum(Cases, na.rm = TRUE),
              new_cases=sum(new_cases, na.rm = TRUE)
              ) %>% 
  ungroup() %>% 
  select(County, Date, Cases, new_cases, Inmates=Population)

print("--11--")

#---------------------------------------------------    
#------------------- Mapping Data -------------------
#---------------------------------------------------    

#   Select off latest values from County_calc
TodayData <- County_calc %>% 
  group_by(County) %>% 
  mutate_at(vars(matches("avg_")), nth, -3) %>% 
  filter(row_number()==n()) %>% 
  mutate_if(is.numeric, signif, 3) %>% 
  ungroup()

print("--5--")
# Add current cases to county for labeling selector

County_pop <- left_join(County_pop, TodayData, by="County") %>% 
  select(County, Population=Population.x, Cases) %>% 
  replace_na(list(Cases=0))

foo <- left_join(TodayData, Prison_load, by="County") %>% 
  select(County, Population=Population.x,
         inmates=Population.y) %>% 
  mutate(inmate_pct=signif(100*inmates/Population,3)) %>%  
  select(County, inmate_pct ) %>% 
  mutate(prison_size=ifelse(inmate_pct>1, 
                            "Large Inmate Pop",
                            "Small Inmate Pop")) %>% 
  replace_na(list(prison_size="Small Inmate Pop")) %>% 
  mutate(prison=factor(prison_size, levels=c("Small Inmate Pop", "Large Inmate Pop"))) %>% 
  select(County, prison_size)

print("--8--")
TodayData$prison_size <- foo$prison_size
TodayData <- TodayData %>% 
  mutate(prison=factor(prison_size, levels=c("Small Inmate Pop", "Large Inmate Pop")))

#     Meat packing in the county?
meat <- meat_packing %>% 
  group_by(County) %>% 
  summarise(Employees=sum(Employees, na.rm=TRUE)) %>% 
  ungroup
foo <- left_join(TodayData, meat, by="County") %>% 
  mutate(meaty=ifelse(is.na(Employees), 
                      FALSE,
                      TRUE))  
TodayData$meat <- foo$meaty

#  add county polygons
MappingData <-  merge(Texas, TodayData,
                      by.x = c("County"), by.y = c("County"),
                      all.x = TRUE) 

# Build labels for map

MapLabels <- lapply(seq(nrow(MappingData)), function(i) {
  htmltools::HTML(
    str_replace_all(
      paste( MappingData[i,]$County, 'County<br>', 
             MappingData[i,]$Cases,'Cases Total<br>', 
             MappingData[i,]$Cases_percap, "per 100,000<br>",
             MappingData[i,]$Deaths, "Deaths<br>",
             MappingData[i,]$deaths_percase, "Deaths per Case<br>",
             MappingData[i,]$Deaths_percap, "Deaths per 100,000<br>",
             MappingData[i,]$doubling, "Doubling Time<br>",
             MappingData[i,]$avg_pct, "Avg Pct Chg"
      ),
      "NA", "Zero"))
})

print("--6--")
#     Trim County_calc now that mappingdata is made
################    why do I do this?

County_calc <<- County_calc %>% filter(n>5)


######################################################################
#                               save files
######################################################################

#   County_calc - County level data with all calculations
#    County_pop - Counties and Population, largest first
#   TestingData - Total tests by date
#          MSAs - Metro areas with all calculations
#       MSA_raw - Metro areas, counties, and Population
#   Prison_data - All data by prison
#  Prison_county - All prison data by county
#    MappingData - Data for drawing map
#      MapLabels - Labels for each county on map

path <- "/home/ajackson/Dropbox/Rprojects/Covid/Today_Data/"

saveRDS(County_calc, paste0(path,"Today_County_calc.rds"))
saveRDS(County_pop, paste0(path,"Today_County_pop.rds"))
saveRDS(TestingData, paste0(path,"Today_TestingData.rds"))
saveRDS(MSAs, paste0(path,"Today_MSAs.rds"))
saveRDS(MSA_raw, paste0(path,"Today_MSA_raw.rds"))
saveRDS(Prison_data, paste0(path,"Today_Prison_data.rds"))
saveRDS(Prison_county, paste0(path,"Today_Prison_county.rds"))
saveRDS(MappingData, paste0(path,"Today_MappingData.rds"))
saveRDS(MapLabels, paste0(path,"Today_MapLabels.rds"))

path <- "/home/ajackson/Dropbox/mirrors/ajackson/Covid/"

saveRDS(County_calc, paste0(path,"Today_County_calc.rds"))
saveRDS(County_pop, paste0(path,"Today_County_pop.rds"))
saveRDS(TestingData, paste0(path,"Today_TestingData.rds"))
saveRDS(MSAs, paste0(path,"Today_MSAs.rds"))
saveRDS(MSA_raw, paste0(path,"Today_MSA_raw.rds"))
saveRDS(Prison_data, paste0(path,"Today_Prison_data.rds"))
saveRDS(Prison_county, paste0(path,"Today_Prison_county.rds"))
saveRDS(MappingData, paste0(path,"Today_MappingData.rds"))
saveRDS(MapLabels, paste0(path,"Today_MapLabels.rds"))

cat("\n\n=============== Build Covid Files ended =========\n\n")
print(lubridate::now())
cat("\n=============== Build Covid Files ended =========\n\n")
