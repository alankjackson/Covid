#
# View and model Texas county-level Covid-19 data
# 
#                Alan Jackson March 2020
#
#

library(shiny)
library(tidyverse)
library(leaflet)
library(leafpop) # for popup on map
library(ggplot2)
library(stringr)
library(lubridate)
library(rsample)
library(broom)
library(purrr)
library(slider)


###################################
#   get and set up the basic data
###################################

#   Directory where data is stored

DataLocation <- "https://www.ajackson.org/Covid/"
DataArchive <- "https://www.ajackson.org/SharedData/"

#   Tibble database

#   Case data
z <- gzcon(url(paste0(DataLocation, "Covid.rds")))
DF <- readRDS(z)
close(z)
#   Testing data
z <- gzcon(url(paste0(DataLocation, "Testing.rds")))
TestingData <- readRDS(z)
close(z)
TestingData$Total <- as.numeric(TestingData$Total)
#   Death data
z <- gzcon(url(paste0(DataLocation, "Deaths.rds")))
DeathData <- readRDS(z)
close(z)
#   County Population data
z <- gzcon(url(paste0(DataArchive, "Census_July1_2019_TexasCountyPop.rds")))
Counties <- readRDS(z)
close(z)




#   County polygons
Texas <- readRDS(gzcon(url(paste0(DataArchive, "Texas_County_Outlines_lowres.rds"))))

init_zoom <- 6
MapCenter <- c(-99.9018, 31.9686) # center of state

global_slope <- 0.13
# https://dartthrowingchimp.shinyapps.io/covid19-app/

# Clean up footnotes

DF$County <- str_replace(DF$County, "\\d", "")

# drop rows with zero or NA cases

DF <- DF %>% filter(Cases>0, !is.na(Cases))

# Add Statewide Totals per day

DF <- DF %>% select(-LastUpdate) %>% bind_rows(
                  DF %>%
                  group_by(Date) %>% 
                  summarise(Cases = sum(Cases), Deaths=sum(Deaths)) %>% 
                  mutate(County="Total")
                 ) %>% 
    arrange(Date)

# Calc days since March 10

DF <- DF %>% 
    mutate(Days=as.integer(Date-ymd("2020-03-10")))

DeathData <- DeathData %>% 
    mutate(Days=as.integer(Date-ymd("2020-03-10")))

# Fix Deaths field

DF$Deaths <- str_replace(DF$Deaths,"-", "na")

DF <- DF %>% 
  mutate(Deaths=as.numeric(Deaths)) %>% 
  mutate(Deaths=na_if(Deaths, 0))

# Calculate new cases

DF <- DF %>% 
  group_by(County) %>% 
    arrange(Cases) %>% 
    mutate(new_cases=(Cases-lag(Cases, default=Cases[1]))) %>%
    mutate(new_cases=pmax(new_cases, 0)) %>% # truncate negative numbers
    mutate(new_deaths=(Deaths-lag(Deaths, default=Deaths[1]))) %>%
    mutate(new_deaths=pmax(new_deaths, 0)) %>% # truncate negative numbers
  ungroup()

#   Last date in dataset formatted for plotting

sf <- stamp_date("Sunday, Jan 17, 1999")
lastdate <- sf(DF$Date[nrow(DF)])

LastDate <- DF[nrow(DF),]$Date

#   Sort counties with 20 largest first, then alphabetical

ByPop <- arrange(Counties, -Population)
ByAlpha <- arrange(ByPop[21:nrow(ByPop),], County)
Counties <- bind_rows(ByPop[1:20,], ByAlpha)
ByPop <- ByAlpha <- NULL

Regions <- tribble(
            ~Region, ~Population, ~Label,
            "Texas", 27864555, "Texas",
            "Houston-Galv", 6779104, "Houston/Galveston Metro Region",
            "Dallas-Fort Worth", 4938225, "Dallas/Fort Worth Metro Region",
            "San Antonio", 2426204, "San Antonio Metro Region",
            "Austin", 2058351, "Austin Metro Region",
            "Lubbock", 290805, "Lubbock Metro Region",
            "Corpus Christi", 429024, "Corpus Christi Region", 
            "Killeen-Temple", 460303, "Killeen-Temple Region", 
            "Beaumont-Port Arthur", 392563, "Beaumont-Port Arthur Region", 
            "Amarillo", 249881, "Amarillo Metro Region")

DefineRegions <- tribble(
    ~Region, ~List,
    "Texas", c("Total"),
    "Houston-Galv", c("Harris", "Fort Bend", "Galveston", "Waller", "Montgomery", "Liberty", "Brazoria", "Chambers", "Austin"),
    "Dallas-Fort Worth", c("Collin", "Dallas", "Denton", "Ellis", "Hood", "Hunt", "Johnson", "Kaufman", "Parker", "Rockwall", "Somervell", "Tarrant", "Wise"),
    "San Antonio", c("Atascosa", "Bandera", "Bexar", "Comal", "Guadalupe", "Kendall", "Medina", "Wilson"), 
    "Austin", c("Bastrop", "Caldwell", "Hays", "Travis", "Williamson"),
    "Lubbock", c("Crosby", "Lubbock", "Lynn"),
    "Corpus Christi", c("Aransas", "Nueces", "San Patricio"),
    "Killeen-Temple", c("Bell", "Coryell", "Lampasas"),
    "Beaumont-Port Arthur", c("Hardin", "Jefferson", "Orange"),
    "Amarillo", c("Armstrong", "Carson", "Potter", "Randall", "Oldham")
)

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

# prep mapping polygons

TodayData <- DF %>% filter(Date==LastDate) %>% 
  filter(County!="Pending County Assignment") %>% 
  left_join(., Counties, by="County") %>% 
  mutate(percapita=Cases/Population*100000)

# Calc doubling and slope for last 5 days
GetSlope <- function(x, y){
  if(length(y)<5) {return(NA)}
  if(y[5]<5) {return(NA)}
  model <- lm(y~x )
  model[["coefficients"]][["x"]]
}
GetDouble <- function(x, y){
  if(length(y)<5) {return(NA)}
  if(y[5]<5) {return(NA)}
  model <- lm(log10(y)~x )
  m <- model[["coefficients"]][["x"]]
  if (m<.0001) {return(NA)}
  signif(log10(2)/m,2) # doubling time
}
GetPercent <- function(x, y){
  if(length(y)<5) {return(NA)}
  if(y[5]<5) {return(NA)}
  m <- (100*(y-lag(y))/lag(y))
  m <- mean(m, na.rm = TRUE)
  signif(m,2) # average percent change
}

Slopes <- DF %>% 
  filter(County!="Pending County Assignment") %>% 
  group_by(County) %>% 
  slice(tail(row_number(), 5)) %>% 
  mutate(m=GetSlope(Days, Cases)) %>% 
  mutate(double=GetDouble(Days, Cases)) %>% 
  mutate(avgpct=GetPercent(Days, Cases)) %>% 
  slice(1) %>% 
  ungroup %>% 
  select(County, m, double, avgpct)

# Add current cases to county for labeling selector

Counties <- left_join(Counties, TodayData, by="County") %>% 
  select(County, Population=Population.x, Cases) %>% 
  replace_na(list(Cases=0))

MappingData <-  merge(Texas, TodayData,
                      by.x = c("County"), by.y = c("County"),
                      all.x = TRUE) 

MappingData <- left_join(MappingData, Slopes, by="County")

# Build labels for map


MappingData <- MappingData %>%
  mutate(m=signif(100000*m/Population,3)) %>% 
  mutate(percapita=signif(percapita,3)) %>% 
  mutate(Deaths=na_if(Deaths, 0)) %>% 
  mutate(DPerC=na_if(signif(Deaths/Cases,2),0)) %>% 
  mutate(DPerCap=na_if(100000*signif(Deaths/Population,2),0))  

MapLabels <- lapply(seq(nrow(MappingData)), function(i) {
  htmltools::HTML(
    str_replace_all(
      paste0( MappingData[i,]$County, ' County<br>', 
              MappingData[i,]$Cases,' Cases Total<br>', 
              MappingData[i,]$percapita, " per 100,000<br>",
              MappingData[i,]$Deaths, " Deaths<br>",
              MappingData[i,]$DPerC, " Deaths per Case<br>",
              MappingData[i,]$DPerCap, " Deaths per 100,000<br>",
              #MappingData[i,]$m, " Cum Slope per capita<br>",
              MappingData[i,]$double, " Doubling Time<br>",
              MappingData[i,]$avgpct, " Avg Pct Chg"
              ),
      "NA", "Zero"))
})

span <- function(vector){
  foo <- range(vector, na.rm=TRUE)
  return(max(foo) - min(foo))
}

#          Calculate doubling times along whole vector
doubling <- function(cases, window, County) {
  print("---->>>>")
  print(County[1])
  if (length(cases)<window){
    return(rep(NA,length(cases)))
  }
  halfwidth <- as.integer(window/2)
  rolling_lm <- tibbletime::rollify(.f = function(logcases, Days) {
    lm(logcases ~ Days)
  }, 
  window = 5, 
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
}

#     Calculate a rolling average
attribute <- function(foo, attribute, grouper){
  #   group = grouping variable (County)
  #   attribute = column in data frame to rolling average
  foo %>% 
    group_by(!!grouper) %>% 
    arrange(Date) %>% 
    mutate(avg=zoo::rollmean(!!attribute, window, 
                             fill=c(0, NA, last(!!attribute)))) %>% 
    ungroup()
  
}

#   trim outliers

isnt_out_z <- function(x, thres = 8, na.rm = TRUE) {
  good <- abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
  x[!good] <- NA # set outliers to na
  x[x<=0] <- NA # non-positive values set to NA
  x
}

  #---------------------------------------------------    
  #------------------- County Data -------------------
  #---------------------------------------------------    
  
 prep_counties <- function( ) { 
  
  window <- 5
  #---------------  Control matrix
  
  calc_controls <- tribble(
    ~base,       ~avg, ~percap, ~trim, ~positive,
    "Cases",      TRUE, TRUE,  FALSE, TRUE,
    "Deaths",     TRUE, TRUE,  FALSE, TRUE,
    "pct_chg",    TRUE, FALSE, FALSE, TRUE,
    "doubling",   TRUE, FALSE, TRUE, TRUE,
    "new_cases",  TRUE, TRUE,  FALSE, TRUE,
    "new_deaths", TRUE, TRUE,  FALSE, TRUE
  )
  
  #---------------  Clean up and calc base quantities
  foo <- DF %>%     
    # Start each county at 10 cases
    filter(Cases>10) %>%  
    group_by(County) %>% 
      arrange(Date) %>% 
      mutate(day = row_number()) %>% 
      add_tally() %>% 
    ungroup() %>% 
    filter(n>5) %>% # must have at least 5 datapoints
    select(County, Cases, Deaths, Date, new_cases, new_deaths) %>% 
    filter(County!="Total") %>% 
    filter(County!="Pending County Assignment") %>% 
    left_join(Counties, by="County") %>% 
    rename(Cases=Cases.x) %>% 
    select(-Cases.y) %>% 
    group_by(County) %>%
      arrange(Date) %>% 
      mutate(pct_chg=100*new_cases/lag(Cases, default=Cases[1])) %>% 
      mutate(doubling=doubling(Cases, window, County)) %>% 
    ungroup()
  
  #----------------- Trim outliers and force to be >0
  
  for (base in calc_controls$base[calc_controls$trim]){
    for (county in unique(foo$County)) {
      foo[foo$County==county,][base] <- isnt_out_z((foo[foo$County==county,][[base]]))
    }
  }
  for (base in calc_controls$base[calc_controls$positive]){
    foo[base] <- na_if(foo[base], 0)
  }
  
  #----------------- Calc Rolling Average
  
  inputs <- calc_controls$base[calc_controls$avg==TRUE]
  
  foo <- foo %>% 
    group_by(County) %>% 
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
    mutate_at(inputs, list(percap = ~ . / Population * 1.e5)) 
  
  counties <<- foo
  
 }

prep_counties()


#' log scale
#'
#' Creates a function which returns ticks for a given data range. It uses some
#' code from scales::log_breaks, but in contrast to that function it not only
#' the exponentials of the base b, but log minor ticks (f*b^i, where f and i are 
#' integers), too.
#'
#' @param n Approximate number of ticks to produce
#' @param base Logarithm base
#'
#' @return
#'
#' A function which expects one parameter:
#'
#' * **x**: (numeric vector) The data for which to create a set of ticks.
#'
#' @export
logTicks <- function(n = 5, base = 10){
  # Divisors of the logarithm base. E.g. for base 10: 1, 2, 5, 10.
  divisors <- which((base / seq_len(base)) %% 1 == 0)
  mkTcks <- function(min, max, base, divisor){
    f <- seq(divisor, base, by = divisor)
    return(unique(c(base^min, as.vector(outer(f, base^(min:max), `*`)))))
  }
  
  function(x) {
    rng <- range(x, na.rm = TRUE)
    lrng <- log(rng, base = base)
    min <- floor(lrng[1])
    max <- ceiling(lrng[2])
    
    tck <- function(divisor){
      t <- mkTcks(min, max, base, divisor)
      t[t >= rng[1] & t <= rng[2]]
    }
    # For all possible divisors, produce a set of ticks and count how many ticks
    # result
    tcks <- lapply(divisors, function(d) tck(d))
    l <- vapply(tcks, length, numeric(1))
    
    # Take the set of ticks which is nearest to the desired number of ticks
    i <- which.min(abs(n - l))
    if(l[i] < 2){
      # The data range is too small to show more than 1 logarithm tick, fall
      # back to linear interpolation
      ticks <- pretty(x, n = n, min.n = 2)
    }else{
      ticks <- tcks[[i]]
    }
    return(ticks)
  }
}
##################################################
# Define UI for displaying data for Texas
##################################################
ui <- basicPage(
  tags$head(
    tags$style(type="text/css", ".inline label{ display: table-cell; text-align: left; vertical-align: middle; } 
                 .inline .form-group{display: table-row;}")
  ),
    #    Cases, Map, Documentation
  tabsetPanel(id = "tabs",
    ##########   Analysis Tab
    tabPanel( "Analysis", fluid = TRUE, value = "AnalysisTab",
        HTML("<hr>"),
        fluidPage(#-------------------- Analysis Tabs
          tags$head(
            tags$style(
              HTML(".shiny-notification {
                   position:fixed;
                   top: calc(20%);
                   left: calc(80%);
                   } "
              ) 
            ) 
          ),
    column( 9, # Tabs
      tabsetPanel(id = "An_tabs",          
         tabPanel( ##########   Cases Tab
           "Cases",
           fluid = TRUE,
           value = "Cases",
           HTML("<hr>"),
           plotOutput("plot_cases",
                      height = "700px"),
           h4("Details on displayed data"),
           htmlOutput("data_details")
         ), # end tab panel Deaths
         tabPanel( ##########   Deaths Tab
                   "Deaths",
                   fluid = TRUE,
                   value = "Deaths",
                   HTML("<hr>"),
                   plotOutput("plot_deaths",
                              height = "700px"),
                   h4("Details on displayed data"),
                   htmlOutput("death_details")
         ), # end tab panel Deaths
         tabPanel( ##########   Indicators
                   "Indicators",
                   fluid = TRUE,
                   value = "Indicators",
                   plotOutput("plot_slopes",
                              height = "700px")#,
                  # h4("Details on displayed data"),
                  # htmlOutput("death_details")
         ), # end tab panel Indicators
         tabPanel( ##########   Missed Tests
                   "Missed Tests",
                   fluid = TRUE,
                   value = "Tests",
                   HTML("<hr>")
         ), # end tab panel Missed Tests
         tabPanel(
                   "Something",
                   fluid = TRUE,
                   value = "Something",
                   HTML("<hr>")
          ) # end tab panel Something
        ) # end TabSet panel An_tabs
      ), # end column 
            #-------------------- Data Selection
         column(3, # Controls
              wellPanel( # Data Select
                 h4("Choose the data"),
                 radioButtons(
                   "dataset",
                   label = strong("Which Data?"),
                   choices = list("Region" = "Region",
                                  "County" = "County"),
                   selected = "Region",
                   width = '90%'
                 ),
                 conditionalPanel(
                   #    Select Region
                   condition = "input.dataset == 'Region'",
                   selectInput("region", "Choose a Region:",
                               Regions$Region,
                               selected = "Texas")
                 ), # end conditional panel
                 conditionalPanel(
                   #    Select County
                   condition = "input.dataset == 'County'",
                   selectInput(
                     "county",
                     label = "Choose a County:",
                     paste0(Counties$County, ": ", Counties$Cases),
                     selected = "Harris"
                   )
                 ), # end select county 
                 dateRangeInput('dateRange',
                      label = 'Date range mm/dd/yy',
                      start = ymd("2020-03-10"), end = today(),
                      min   = ymd("2020-03-10"), max = today(),
                      separator = " - ", format = "mm/dd/yy"
                 )
               ), # end Data select

              #-------------------- Modeling parameters
                  conditionalPanel( # Cases Plot controls
                    #    Cases Tab
                    condition = "input.An_tabs == 'Cases'",               
                    #-------------------- Plot controls
              wellPanel(
                # Control plot options
                h4("Plotting options"),
                checkboxInput(
                  inputId = "avoid",
                  label = strong("Crowd sizes to avoid"),
                  value = FALSE
                ),
                checkboxInput(
                  inputId = "zoom",
                  label = strong("Expand scale"),
                  value = FALSE
                ),
                #checkboxInput(
                #    inputId = "recovery",
                #    label = strong("Est missed cases"),
                #    value = FALSE
                #),
                checkboxInput(
                  inputId = "logscale",
                  label = strong("Log Scaling"),
                  value = TRUE
                ),
                numericInput(
                  "recover_days",
                  label = strong("Days to Recover"),
                  step = 1,
                  value = 14
                )
              ),
              # end wellPanel Control plot options
                  wellPanel(
                    # Modeling parameters
                    #h4("Data Fits"),
                    radioButtons(
                      "modeling",
                      label = h4("Fitting"),
                      choices = list(
                        "Exponential" = "do fit",
                        "Logistic" = "logistic",
                        "Worldwide (0.13)" = "standard",
                        "User entry" = "user"
                      ),
                      selected = "do fit"
                    ),
                    splitLayout(
                      numericInput(
                        "slope",
                        label = h5("Slope"),
                        step = 0.005,
                        value = global_slope
                      ),
                      numericInput(
                        "intercept",
                        label = h5("Intercept"),
                        step = 0.10,
                        value = 1.00
                      )
                    )

                    ) 
                  ),
                  # end wellPanel Modeling parameters
                
                  conditionalPanel( # Deaths Plot controls
                    #    Deaths Tab
                    condition = "input.An_tabs == 'Deaths'",
                      wellPanel(
                        checkboxInput(
                          inputId = "Deaths_logscale",
                          label = strong("Log Scaling"),
                          value = TRUE
                        ),
                        checkboxInput(
                          inputId = "Deaths_zoom",
                          label = strong("Expand Scale"),
                          value = FALSE
                        ),
                    radioButtons(
                      "death_modeling",
                      label = h4("Fitting"),
                      choices = list(
                        "Exponential" = "death_exp",
                        "Logistic" = "death_logistic"
                      ),
                      selected = "death_exp"
                    ),
                        HTML("<hr>"),
                        checkboxInput(
                          inputId = "Deaths_back_est",
                          label = strong("Est Cases from Deaths"),
                          value = FALSE
                        ),
                        splitLayout(
                          cellWidths = c("35%", "65%"),
                          numericInput(
                              "An_CFR",
                              label = h5("CFR (%)"),
                              step = 0.1,
                              value = 1.0
                          ),
                          numericInput(
                              "An_DeathLag",
                              label = h5("Days to Death"),
                              step = 1.0,
                              value = 13.00
                          )
                        ),
                    HTML("<hr>")
                    
                  ), # End Deaths Plot controls
                  # end conditional panel
                  ),# end wellPanel
                  # end conditional panel
                  conditionalPanel( 
                    #    Indicators Tab
                    condition = "input.An_tabs == 'Indicators'",
                      wellPanel(
                        tags$div(class = "inline", 
                                 numericInput(inputId = "window", 
                                              step = 2,
                                              value = 3,
                                              min=3,
                                              max=15,
                                           label = "Fit Length:"),
                                 numericInput(inputId = "smthlength", 
                                            step = 2,
                                            value = 3,
                                            min=3,
                                            max=15,
                                           label = "Median:")),
                        checkboxInput(
                          inputId = "smooth",
                          label = strong("Smooth?"),
                          value = FALSE
                        ),
                      ),
                      wellPanel(
                        radioButtons(
                          "slopetype",
                          label = h4("Y-Axis"),
                          choices = list(
                            "New Cases" = "newcase",
                            "Avg New Cases" = "avgnewcase",
                            "New Cases per 100k" = "newcasepercap",
                            "Avg New Cases per 100k" = "avgnewcasepercap",
                            "Percent change" = "percent",
                            "Avg Percent change" = "avgpercent",
                            "Cum Cases" = "cases",
                            "Doubling Time" = "doubling"
                          ),
                      selected = "newcase"
                        ) 
                      )
                  )
                 ) # end column Controls
         ) # end fluid page
                
     ), # end tabPanel Analysis
    ##########   Counties Tab
    tabPanel( "Counties", fluid = TRUE, value = "CountiesTab",
        HTML("<hr>"),
        fluidPage(#-------------------- Counties
            column( 9, # Plot           
              plotOutput("plot_counties",
                    height = "700px"),
              #h4("Details on displayed data"),
              #htmlOutput("counties_details")
              gt::gt_output("counties_details")
            ), # end of column Plot
            #-------------------- Controls
            column(3, # Controls
                  wellPanel( 
                    h4("Choose the Y-Axis"),
                    checkboxInput(
                      "counties_avg",
                      label = "Running Average",
                      value = TRUE
                    ),
                    #HTML("<hr>"),
                    checkboxInput(
                      inputId = "counties_percap",
                      label = "per 100,000",
                      value = TRUE
                    ),
                    HTML("<hr>"),
                    radioButtons(
                      "counties_y_axis",
                      label = NULL,
                      choices = list(
                        "Cases" = "Cases",
                        "New Cases" = "new_cases",
                        #"Avg New Cases" = "avg_new_cases",
                        #"New Cases per 100k" = "new_case_percap",
                        #"Avg New Cases per 100k" = "avg_new_case_percap",
                        #"Avg Percent change" = "avg_pct_chg",
                        #"Cases per 100k" = "percapita",
                        "Deaths" = "Deaths",
                        "New Deaths" = "new_deaths",
                        "Percent change" = "pct_chg",
                        #"Deaths per 100k" = "DPerC"
                        "Doubling Time" = "doubling"
                      ),
                      selected = "Cases"
                    ) 
                  ), # end y-axis panel
                  wellPanel( 
                    h4("Highlight Based On:"),
                    checkboxInput(
                      "counties_select_avg",
                      label = "Running Average",
                      value = TRUE
                    ),
                    #HTML("<hr>"),
                    checkboxInput(
                      inputId = "counties_select_percap",
                      label = "per 100,000",
                      value = TRUE
                    ),
                    HTML("<hr>"),
                    radioButtons(
                      "counties_selector",
                      label = NULL,
                      choices = list(
                        "Cases" = "Cases",
                        "New Cases" = "new_cases",
                        #"Avg New Cases" = "avg_new_cases",
                        #"New Cases per 100k" = "new_case_percap",
                        #"Avg New Cases per 100k" = "avg_new_case_percap",
                        #"Avg Percent change" = "avg_pct_chg",
                        #"Cases per 100k" = "percapita",
                        "Deaths" = "Deaths",
                        "New Deaths" = "new_deaths",
                        "Percent change" = "pct_chg",
                        #"Deaths per 100k" = "DPerC"
                        "Doubling Time" = "doubling"
                      ),
                      selected = "new_cases"
                    ) 
                  ), # end highlight panel
                  wellPanel( # Misc controls
                    tags$div(class = "inline", 
                             numericInput(inputId = "case_start", 
                                          step = 1,
                                          value = 30,
                                          min=10,
                                          label = "Start:")
                    ),
                    checkboxInput(
                      inputId = "county_log",
                      label = strong("Log Scale"),
                      value = TRUE
                    )
                  ) # end Misc controls
               ) # end column control
         ) # end fluid page
     ), # end tabPanel Counties
    ##########   Map Tab
    tabPanel( "Map", fluid = TRUE, value = "MapTab",
        HTML("<hr>"),
        fluidPage(#-------------------- Map
            column( 9, # Map
                    h3(textOutput("MapTitle")),
                    leafletOutput("TexasMap",
                                  height = "800px"),
                    HTML("<hr>")
            ), # end of column Plot
            #-------------------- Controls
            column(3, # Controls
               #    Select quantity to color counties with
               radioButtons("county_color", 
                  label = strong("Display which variable?"),
                  choices = list( "Total Cases" = "casetotal", 
                                 "Cases per 100,000 population" = "percapita",
                                 "Deaths" = "deaths",
                                 "Deaths per 100,000" = "deathspercap",
                                 "Deaths/Cases" = "deathpercase",
                                 #"Slope per 100,000" = "case_slope",
                                 "Doubling Time" = "double",
                                 "Avg Recent Pct Change" = "avg_chg"
                                 ), 
                  selected = "casetotal",
                  width='90%',
                  inline=FALSE)
               ) # end column control
         ) # end fluid page
                   
     ), # end tabPanel Map
    ##########   Documentation Tab
         tabPanel("Documentation", fluid=TRUE, value="DocumentationTab",
                  withMathJax(includeMarkdown("Documentation.Rmd")),
                  HTML("<hr>")

          )  # end tabPanel Documentation
        )  # end tabset 
    ) # end basic page

# Define server logic 
server <- function(input, output) {
  #hideTab(inputId = "An_tabs", target="Indicators")   
  hideTab(inputId = "An_tabs", target="Tests")   
  hideTab(inputId = "An_tabs", target="Something")   
#   Global variables are
#   DF = original data
#   PopLabel = list(Region, Population, Label)
#   subdata = tibble of data subsetted 
#   death_fit, case_fit, case_est_fit = Day, Date, 'value',
  #                                     upper_conf, lower_conf
#   death_params, case_params, case_est_params =
  #                     named arrays of m, b, Rsq or
  #                                     r, K, xmid
       
  #---------------------------------------------------    
  #------------------- Prep Data ---------------------
  #---------------------------------------------------    
  prep_data <- function(in_dataset="Region", 
                        in_area="Texas",
                        in_dateRange
                        ) { 
    print(":::::::  prep_data")
      print("-----------  dateRange")
      print(in_dateRange)
      print("-----------  dateRange")
    if (in_dataset=="Region") { # work with regions
      PopLabel <<- Regions %>% filter(Region==in_area)
      target <- unlist(DefineRegions$List[DefineRegions$Region==in_area])
      subdata <<- DF %>% 
          filter(County %in% target) %>% 
          group_by(Date) %>% 
          summarise(Cases=sum(Cases), 
                    new_cases=sum(new_cases), 
                    Days=mean(Days), 
                    Deaths=sum(Deaths, na.rm=TRUE)) %>% 
          mutate(actual_deaths=Deaths-lag(Deaths, 1, 0)) %>%  
          mutate(Deaths=na_if(Deaths, 0)) %>% 
          filter(between(Date, 
                         ymd(in_dateRange[1]), 
                         ymd(in_dateRange[2])))
      return()
      
    } else { # select a county
      #   Is there any data?
      county <- str_extract(in_area, "[A-Za-z ]+")
      if (! county %in% DF$County) {
        showNotification(paste("No reported cases in", county),
                         duration=2)
        return()
      }
      
      PopLabel <<- Counties %>% filter(County==county) %>% 
                     mutate(Label=paste(county, "County"))
      subdata <<- DF %>% filter(County==county) %>% 
                         mutate(actual_deaths=Deaths-lag(Deaths, 1, 0)) %>% 
                         filter(between(Date, 
                                        ymd(in_dateRange[1]), 
                                        ymd(in_dateRange[2])))
      return()
    }
  } # end prep_data
  #---------------------------------------------------    
  #-----------Fit an exponential model ---------------
  #---------------------------------------------------    
  
 fit_exponential <- function(indep="Cases", # independent variable
                             fit_type=c('all', 'none', "b_only", "m_only"),
                             m=1.3,
                             b=1,
                             cutoff=1,
                             projection=10,
                             calc_conf=TRUE) {
   
    print(paste(":::::::  fit_exponential", indep))
   #  Drop rows that are zero
   data <- subdata %>% filter((!!sym(indep))>0) 
   #  Drop rows that are equal to previous row
   data <- subdata %>% 
     filter((!!sym(indep))>0) %>% 
     filter(!is.na((!!sym(indep)))) %>% 
     mutate(actual=!!as.name(indep)-lag(!!as.name(indep), 1, 0)) %>% 
     filter(actual>0) %>% 
     mutate(!!indep:=cumsum(actual))
   
   ##############################
   #browser()
   ##############################
   #    Too few cases to do a fit
  if ((sum(!is.na(data[,indep][[1]]))<2) ||
      (nrow(unique(data[,indep]))<2)) {
      print(paste(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", data$County[1]))
     if (indep=="Cases") {
          case_fit <<- tibble( Days=NA, Date=NA,!!indep:=NA,
                               upper_conf=NA, lower_conf=NA) 
          case_params <<- list(m=NA, b=NA, Rsqr=NA)
          return()
      } else {
          death_fit <<- tibble( Days=NA, Date=NA,!!indep:=NA,
                               upper_conf=NA, lower_conf=NA) 
          death_params <<- list(m=NA, b=NA, Rsqr=NA)
          return()
      }
   }
   #   Go projection days into future
   begin <- data$Date[1] # date of first reported case
   LastDate <- data[nrow(data),]$Date
   lastday <- as.integer(LastDate - begin) + 1 # last day of real data
   dayseq <- data$Days
   dayseq <- c(dayseq,(dayseq[length(dayseq)]+1):
                 (dayseq[length(dayseq)]+projection))
   dateseq <- data$Date
   dateseq <- as_date(c(dateseq,(dateseq[length(dateseq)]+1): 
                          (dateseq[length(dateseq)]+projection)))
   x <- data$Days
   y <- data[,indep][[1]] 
   my_data <- tibble(x=x, y=y)
   
   if (fit_type=="all") { 
     model <- lm(log10(y)~x, data=my_data)
     m <- model[["coefficients"]][["x"]]
     b <- model[["coefficients"]][["(Intercept)"]]
     Rsqr <- summary(model)$adj.r.squared
     std_dev <- sigma(model)
   } else if (fit_type=="none") {
     m <- m
     b <- b
     Rsqr <- 1
     std_dev <- 0
   } else if (fit_type=="b_only") {
     model <- lm(log10(y) - m*x ~ 1, data=my_data)
     b <- model[["coefficients"]][["(Intercept)"]]
     Rsqr <- summary(model)$adj.r.squared
     std_dev <- sigma(model)
     print(paste("----b only ----", m, b, lastday, data$Cases[lastday]))
   } else if (fit_type=="m_only") {
     model <- lm(I(x - b) ~ 0 + log10(y), data=my_data)
     m <- model[["coefficients"]][["x"]]
     b <- model[["coefficients"]][["(Intercept)"]]
     Rsqr <- summary(model)$adj.r.squared
     std_dev <- sigma(model)
   } else {print("serious error in fit_exponential")}
   
   #  Estimate confidence bands 
   if(calc_conf & (fit_type=="all" || fit_type=="m_only")) {
     DayFrame <- data.frame(x=dayseq)
     pred.int <- cbind(DayFrame, 
                       predict(model, 
                               newdata = DayFrame, 
                               interval = "confidence", 
                               level = 0.975))
     fits <- tibble(Days=dayseq, 
                         Date=dateseq,
                         !!indep:=10**pred.int$fit,
                         lower_conf=10**pred.int$lwr,
                         upper_conf=10**pred.int$upr)
     params <- list(m=m, b=b, Rsqr=Rsqr)
        
     if (indep=="Cases") {
       case_fit <<- fits
       case_params <<- params
     } else if (indep=="Deaths") {
       death_fit <<- fits
       death_params <<- params
     }
   } else {
     Cases <- 10**(m*dayseq+b)
     if (indep=="Cases") {
       case_fit <<- tibble( Days=dayseq, Date=dateseq,!!indep:=Cases,
                          upper_conf=NA, lower_conf=NA) 
       case_params <<- list(m=m, b=b, Rsqr=Rsqr)
     } else if (indep=="Deaths") {
       death_fit <<- tibble( Days=dayseq, Date=dateseq,!!indep:=Cases,
                            upper_conf=NA, lower_conf=NA) 
       death_params <<- list(m=m, b=b, Rsqr=Rsqr)
     }
   }
 } 
  
  
  #---------------------------------------------------    
  #------------------- Fit Logistic function ---------
  #---------------------------------------------------    
  fit_logistic <- function(indep="Cases", # independent variable
                           r=0.24,
                           projection=10){
    
    print(":::::::  logistic")
    df <- subdata

    Asym <- max(df$Cases)*5
    xmid <- max(df$Days)*2
    scal <- 1/r
    my_formula <- as.formula(paste0(indep, " ~ SSlogis(Days, Asym, xmid, scal)"))
    
    print("----1----")
    
    ## using a selfStart model
      
      logistic_model <- NULL
      #try(logistic_model <- nls(indep ~ SSlogis(Days, Asym, xmid, scal), 
      try(logistic_model <- nls(my_formula, 
                                data=df)); # does not stop in the case of error
      
      if(is.null(logistic_model)) {
         case_params <<- list(K=NA, 
                              r=NA, 
                              xmid=NA,
                              xmid_se=NA)
        return()
      }
      
    print("----2----")
    print(logistic_model)
    coeffs <- coef(logistic_model)
    xmid_sigma <- 2*summary(logistic_model)$parameters[2,2] # 2 sigma
    print("----3----")
    #print(coeffs)
    
    dayseq <- df$Days
    dayseq <- c(dayseq,(dayseq[length(dayseq)]+1):
                       (dayseq[length(dayseq)]+projection))
    dateseq <- df$Date
    dateseq <- as_date(c(dateseq,(dateseq[length(dateseq)]+1): 
                                 (dateseq[length(dateseq)]+projection)))
    
    Cases <- predict(logistic_model, data.frame(Days=dayseq))
    foo <- tibble(Date=dateseq, Days=dayseq, Cases=Cases )
    
    ###############   tidy bootstrap start
    
    # Make 100 datasets for bootstrap
    boots <- bootstraps(df, times = 100)
    
    fit_nls_on_bootstrap <- function(split) {
      nls(my_formula, analysis(split))
    }
     f_safe <- purrr::safely(fit_nls_on_bootstrap)
    
    # Fit 100 models
    boot_models <- boots %>% 
      mutate(model = map(splits, f_safe)) %>% 
      mutate(no_error = model %>% purrr::map_lgl(.f = ~ is.null(.x$error))) %>% 
      filter(no_error) %>% 
      mutate(model = model %>% purrr::map("result")) %>% 
      mutate(coef_info = map(model, tidy))
    
    print("---------  boot models -----------")
    
    pred2 <- function(model, foo){
      list(predict(model, foo)[0:nrow(foo)])
    }
    
    # Create predictions from each model and extract confidence
    # limits at each day
    df2 <- boot_models %>% 
      rowwise() %>% 
      transmute(predicted = pred2(model, foo)) %>% 
      as_data_frame() %>%  transpose(.names="1":nrow(boot_models)) %>% 
      lapply(FUN = unlist) %>%
      as_tibble() %>% 
      as.matrix() %>% # convert to matrix for rapid quantile calc
      apply(., 1, quantile, c(0.025, 0.975)) %>% 
      as_tibble() %>% 
      rownames_to_column %>% # turn into 2 columns with many rows
      gather(var, value, -rowname) %>% 
      pivot_wider(names_from=rowname, values_from=value) %>% 
      select(lower_conf=2, upper_conf=3) %>% 
      tibble(foo, .)
    
    ###############   tidy bootstrap end
    #Cases <- predict(logistic_model, foo)
    print(paste("Cases",length(Cases)))
    print(paste("dayseq",length(dayseq)))
    print(paste("dateseq",length(dateseq)))
     #####   set global
    if (indep=="Cases") {
     case_fit <<- tibble(Days=dayseq, 
                         Date=dateseq,
                        !!indep:=Cases,
                        lower_conf=df2$lower_conf,
                        upper_conf=df2$upper_conf)
     
     case_params <<- list(K=coeffs[["Asym"]], 
                          r=1/coeffs[["scal"]], 
                          xmid=coeffs[["xmid"]],
                          xmid_se=xmid_sigma)
    } else {
     death_fit <<- tibble(Days=dayseq, 
                         Date=dateseq,
                        !!indep:=Cases,
                        lower_conf=df2$lower_conf,
                        upper_conf=df2$upper_conf)
     
     death_params <<- list(K=coeffs[["Asym"]], 
                          r=1/coeffs[["scal"]], 
                          xmid=coeffs[["xmid"]],
                          xmid_se=xmid_sigma)
      
    print(death_params)
    }
  }
  
  #---------------------------------------------------    
  #------------------- Build Basic Plot --------------
  #---------------------------------------------------    
  
  build_basic_plot <- function(in_modeling=c("do fit", 
                                             "standard", 
                                             "user",
                                             "logistic"), 
                               in_fit,
                               in_intercept,
                               in_logscale,
                               in_zoom,
                               in_estmiss,
                               in_avoid,
                               in_recover
    ){
      # Build exponential line for plot
    print(":::::::  build_basic_plot")

    begin <- subdata$Date[1] # date of first reported case
    
    if (in_modeling == "logistic") { 
      if (is.null(case_params[["r"]]) || is.na(case_params[["r"]])){# if nonlinear fit failed
        showNotification("Failure to fit data")
        return(NULL)
      }
      EqText <- paste0("Fit is Cases = ",
                       signif(case_params[["K"]],2), "/( 1 + e^(",
                       signif(case_params[["r"]]*case_params[["xmid"]],2)," + ", 
                       signif(case_params[["r"]],2),"*Days))")
    } else {
      if ((is.null(case_params[["m"]])) || (is.na(case_params[["m"]]))){# if fit failed
        showNotification("Failure to fit data")
        return(NULL)
      }
      EqText <- paste0("Fit is log(Cases) = ",
                       signif(case_params[["m"]],3),"*Days + ",
                       signif(case_params[["b"]],3))
    }
    
    print("case_fit")
    print(head(case_fit))
    print(tail(case_fit,10))
    xform <- 2*signif(max(TestingData$Total)/(8*max(subdata$Cases)),2)
 
    #  Basic canvas     
    p <- subdata %>% 
          ggplot(aes(x=Date, y=Cases))
    
    #------------------
    #  Error bars
    #------------------
      if (in_modeling=="do fit" || in_modeling=="logistic") {
          if (!is.nan(case_fit$upper_conf[1])){
            print("------- ribbon 1 -------")
            p <- p + geom_ribbon(data=case_fit,
                                 aes(x=Date,ymin=lower_conf,ymax=upper_conf),
                                 fill="pink")
            print("------- ribbon 2 -------")
        }
      }
    #------------------
    #  Plot fit line, Extension of line, and testing data
    #------------------
    testend <- tibble(end_date=last(TestingData$Date), 
                      end_case=last(TestingData$Total),
                      end_y=last(TestingData$Total)/xform)
    
    # truncate tests where cases start
    testbeg <- max(1, subdata$Days[1]+1-8)
    teststop <- last(subdata$Days)
    
    p <-  p +
          expand_limits(x = LastDate+10) +
          geom_line(data=case_fit,
                    aes(x=Date, y=Cases,
                        color="fit"),
                    size=1,
                    linetype="dashed") +
          geom_line(data=case_fit[1:(nrow(case_fit)-10),],
                    aes(x=Date, y=Cases,
                        color="fit" ),
                    size=1,
                    linetype="solid",
                    fill="blue") +
          geom_point(data=case_fit[(nrow(case_fit)-9):nrow(case_fit),],
                        aes(x=Date, y=Cases),
                     shape=20, size=2, fill="blue") +
          geom_point(data=TestingData[testbeg:teststop,],
                        aes(x=Date, y=Total/xform, color="tests", fill="white"),
                     size=3, shape=21, 
                     fill="white"
                     ) +
          theme(text = element_text(size=20)) +
          labs(title=paste0("COVID-19 Cases in ",PopLabel$Label), 
               subtitle=paste0(" as of ", lastdate)) +
          geom_text(data=testend,
                 aes(y=end_y,x=end_date,label=format(end_case, big.mark = ",")),
                 size=5.0,
                 vjust="bottom", hjust="left") 
    #------------------
    #  Plot recovered estimate
    #------------------
    if (nrow(case_fit)-in_recover>1) {
      p <- p +
        geom_line(data=case_fit[1:(nrow(case_fit)-in_recover),],
                  aes(x=Date+in_recover, y=Cases*0.98,
                      color="recovered"),
                  size=1,
                  linetype="dotted")
    
    #------------------
    #  Plot active cases estimate
    #------------------
      active <- tibble(Date=case_fit$Date[in_recover+1:(nrow(case_fit)-in_recover)],
                       Cases=case_fit$Cases[in_recover+1:(nrow(case_fit)-in_recover)] -
                             case_fit$Cases[1:(nrow(case_fit)-in_recover)]*0.98)
      print(active)
      p <- p +
        geom_line(data=active,
                  aes(x=Date, y=Cases,
                      color="active"),
                  size=1,
                  linetype="solid")
      
    }
    
    #------------------
    #  if logistic fit, show inflection and uncertainty
    #------------------
    if (in_modeling=="logistic") {
      x_infl <- case_params[["xmid"]] 
      x_se <- case_params[["xmid_se"]] 
      y <- case_fit$Cases
      x <- case_fit$Days
      i <- as.integer(x_infl + 2) 
      y_infl <- y[i-1] + (y[i]-y[i-1])*(x[i]-x_infl)/(x[i]-x[i-1])
      print("--------------  arrow")
      print(paste(x_infl, i, y[i-1], y[i], x[i],x[i-1], y_infl))
      x_infl <- as_date(case_params[["xmid"]] + begin)
      p <- p +
           geom_segment(data=tibble(x1=x_infl-x_se, x2=x_infl+x_se, 
                                  y1=y_infl, y2=y_infl),
                                  color="red",
                        aes(x=x1, y=y1, xend=x2, yend=y2),
                      arrow=arrow(ends="both", length = unit(0.15, "inches"))
                      ) +
           geom_segment(data=tibble(x1=x_infl, x2=x_infl,
                                   y1=y_infl*1.9, y2=y_infl*0.4),
                        color="red",
                        arrow=NULL,
                        aes(x=x1, y=y1, xend=x2, yend=y2)) +
          geom_text(data=tibble(x1=x_infl,
                                y1=y_infl*0.4),
                    color="red",
                    aes(x=x1, y=y1, label="Inflection point"),
                    nudge_x=3.50, nudge_y=-0.05) 
    }   
          
    #------------------
    #  Bars or points?
    #------------------
    if (!in_logscale) {
        p <- p + geom_point(aes(color="data"), size=2) 
        #p <- p + geom_col(alpha = 2/3)  +
        #     geom_label(aes(label=Cases), 
        #                    stat='identity',
        #                    size = 3) 
     } else {
        
        p <- p + geom_point(aes(color="data"), size=2) 
                # geom_text(aes(label=Cases),
                #           nudge_x=-1.50, nudge_y=0.0)
        if (max(subdata$Cases)<100) {
          p <- p + geom_text(aes(label=Cases),
                           nudge_x=-1.50, nudge_y=0.0)
          
        }
     }       

      
    #------------------
    #  Log scaling
    #------------------
      if (in_logscale) {
        min_limit <- min(subdata$Cases[1], 10)
        p <- p + scale_y_continuous(sec.axis = sec_axis(~.*xform, 
                                    name = "Statewide Test Total"),
                                    trans="log10",
                                    breaks = logTicks(n = 4), 
                                    minor_breaks = logTicks(n = 40)) 
      } else {
        min_limit <- 0
        p <- p + scale_y_continuous(sec.axis = sec_axis(~.*xform, 
                                    name = "Statewide Test Total"),
                                    trans="identity" ) 
      }
    #------------------
    #  Zoom
    #------------------
    zoom_factor <- 6
    if (in_modeling=="logistic") {zoom_factor <- 4}
      if (in_zoom) { # bigger scale
          # limit height of modeled fit
         p <- p + coord_cartesian(ylim=c(min_limit, 
                                 zoom_factor*max(subdata$Cases)))
      } else { # normal scale

        p <- p + coord_cartesian(ylim=c(min_limit, 
                                 1.5*max(subdata$Cases)))
      }
    #------------------
    #  Legend
    #------------------
      leg_labs <- c("Data", "Fit", "Tests", "Recovered", "Active Cases") # Labels for legend
      leg_vals <- c("black", "blue", "black", "red", "green") # Color values
      leg_fill <- c("black", "blue", "white", "red", "green") # Color fill values
      leg_brks <- c("data", "fit", "tests", "recovered", "active") # Breaks (named lists)

    #------------------
    #  Crowdsize
    #------------------
      if (in_avoid) {
          p <- add_crowdsize(p, in_zoom, in_recover)
      }
      
      p <-  build_legend(p, "Cases",
                             leg_labs, # Labels for legend
                             leg_vals, # Color values
                             leg_brks, # Breaks (named lists)
                             leg_fill, # fill values
                         in_logscale
      )
      
      if (in_modeling=="logistic") {
        output$data_details <- data_details_l(subdata, 
                                              "Cases", 
                                              EqText, 
                                              case_params[["K"]], 
                                              case_params[["xmid"]], 
                                              case_params[["r"]])
      } else {
        output$data_details <- data_details(subdata,
                                             "Cases",
                                             EqText,
                                             case_params[["m"]],
                                             case_params[["b"]],
                                             case_params[["Rsqr"]])
      }
      
    return(p)
  }
  
  #---------------------------------------------------    
  #------------------- Add Estimate of missed cases --
  #---------------------------------------------------    
  # use worldwide slope, derive b from last number of cases
  add_estmiss <- function(p, subdata) {
    print(":::::::  add_estmiss")      
      begin <- data$Date[1] # date of first reported case
      LastDate <- data[nrow(data),]$Date
      lastday <- as.integer(LastDate - begin) + 1  # last day of real data
      dayseq <- 0:(lastday - 1)
      dateseq <- as_date(begin:(LastDate))
      m <- global_slope
      b <- log10(subdata$Cases[lastday]) - m*(lastday-1)
      temp <- tibble( Days=dayseq, 
                      Date=dateseq, 
                      Cases=10**(m*dayseq + b)
                        )
      
      
    Est_layer <-   geom_point(data=temp,
                             aes(x=Date, y=Cases,
                                 color="est_miss")
                             )
    p <- p + Est_layer 
    
    return(p)
  }
  
  #---------------------------------------------------    
  #------------------- Build Legend ------------------
  #---------------------------------------------------    
  
  build_legend <- function(p, title, plabs, pvals, pbrks, fvals, in_log){
    print(":::::::  build_legend")
    
    #   Create list of named characters
    names(pvals) <- pbrks
    
    if (in_log) {
      legend <- theme(legend.position=c( 0.82, 0.20 ))
    } else {
      legend <- theme(legend.position=c( 0.18, 0.80 ))
    }
      
    print(pvals)
    print(plabs)
    print(pbrks)
    Legend_layer <- scale_color_manual(name = title, 
                                         values = pvals,
                                         labels = plabs,
                                         breaks = pbrks
                                         ) 
    p <- p + legend + Legend_layer +
                    scale_fill_manual(name = title, 
                                      values = fvals,
                                      guide="legend",
                                      breaks = pbrks)
    
    return(p)
  }
  
  #---------------------------------------------------    
  #------------------- Add crowd size ----------------
  #---------------------------------------------------    
  # When is probability of 1% contact reached?
  add_crowdsize <- function(p, in_zoom, in_recover) {
    print(":::::::  add_crowdsize")
      begin <- subdata$Date[1] # date of first reported case
      LastDate <- subdata[nrow(subdata),]$Date
      lastday <- as.integer(LastDate - begin) + 1  # last day of real data
      Population <- PopLabel[2][[1]]
      dayseq <- 0:(as.integer(LastDate - begin) + 10)
      dateseq <- as_date(begin:(LastDate + 10))
      active <- tibble(Date=case_fit$Date[in_recover+1:(nrow(case_fit)-in_recover)],
                     Cases=case_fit$Cases[in_recover+1:(nrow(case_fit)-in_recover)] -
                           case_fit$Cases[1:(nrow(case_fit)-in_recover)]*0.98)
      Cases <- active$Cases

      TestDays <- as.integer(LastDate 
                             - begin) + c(0,5,10) + 1 - in_recover
      TestDates <- LastDate + c(0,5,10)
      Crowdsize <- signif((0.01*Population)/(Cases[TestDays]), 2)
      
      # Build label tibble
      CrowdLabels <- tibble(Date=TestDates,
                            Crowd=Crowdsize,
                            Cases=Cases[TestDays])
      
      CrowdLayer1 <-  geom_point(data=CrowdLabels,
                                 aes(x=Date, y=Cases)) 
      CrowdLayer2 <- geom_label(data=CrowdLabels,
                                aes(x=Date, y=Cases, label=Crowd),
                                fill="lightcoral",
                                nudge_x=1,
                                nudge_y=0) 

#   Crowdsize text

CrowdText <- "Sizes of groups to avoid to keep\nchance of meeting a contagious\nperson below 1%"

      y_anno <- 6*last(Cases)*0.95
      if (in_zoom) {
        #y_anno <- Cases[length(Cases)]*0.9
        y_anno <- last(Cases)*0.95
      }
      
      p <- p + CrowdLayer1 + 
               CrowdLayer2 +
               annotate("label", label=CrowdText, 
                        x=begin+10, y=y_anno, 
                        fill="lightcoral", size=4)
      
          return(p)
  }
  

  ################################## Analysis
       
  #---------------------------------------------------    
  #------------------- Back Estimate Cases -----------
  #---------------------------------------------------    

backest_cases <- function(in_An_DeathLag, in_An_CFR, projection) {
  
    print(":::::::  backest_cases")

   dayseq <- death_fit$Days
   dayseq <- c(dayseq,(dayseq[length(dayseq)]+1):
                 (dayseq[length(dayseq)] + in_An_DeathLag))
   dateseq <- death_fit$Date
   dateseq <- as_date(c(dateseq,(dateseq[length(dateseq)]+1): 
                          (dateseq[length(dateseq)] + in_An_DeathLag)))
   dateseq <- dateseq - in_An_DeathLag
   
   print("---- backest 1 -----")
   if (is.null(death_params[["K"]])) { # did an exponential fit
   print("---- backest 3 -----")
     Cases <- 10**(death_params[["m"]]*dayseq+death_params[["b"]])
     Cases <- 100 * Cases / in_An_CFR
   } else { # logistic fit
   print("---- backest 2 -----")
     ################################
     #browser()
     ################################
     Co <- death_params[["r"]]*death_params[["xmid"]]
     Cases <- death_params[["K"]]/(1  + exp(Co - death_params[["r"]]*dayseq))
     Cases <- 100 * Cases / in_An_CFR
   }
  
   return( tibble(Date=dateseq,
                 Days=dayseq,
                 est_cases=Cases))
  
}
    
  #---------------------------------------------------    
  #------------------- Build Death Plot --------------
  #---------------------------------------------------    
  
  build_deaths_plot <- function(
                                in_Deaths_logscale,
                                in_Deaths_zoom,
                                in_An_CFR,
                                in_An_DeathLag,
                                in_Deaths_back_est,
                                in_death_modeling
                                ){
      # Build exponential line for plot
    print(":::::::  build_death_plot")
    # Get rid of duplicate cumulative entries.
    data <- subdata %>% 
      filter(Deaths>0) %>% 
      filter(!is.na(Deaths)) %>% 
      mutate(actual_deaths=Deaths-lag(Deaths, 1, 0)) %>% 
      filter(actual_deaths>0) %>% 
      mutate(Deaths=cumsum(actual_deaths))

    print(data)
    print(in_death_modeling)
    if (in_death_modeling == "death_logistic") { 
    if (!is.null(death_params[["r"]])){print(death_params)}
      if (is.null(death_params[["r"]]) || is.na(death_params[["r"]])){# if nonlinear fit failed
        showNotification("Failure to fit data")
        return(NULL)
      } 
      EqText <- paste0("Fit is Deaths = ",
                       signif(death_params[["K"]],2), "/( 1 + e^(",
                       signif(death_params[["r"]]*death_params[["xmid"]],2)," + ", 
                       signif(death_params[["r"]],2),"*Days))")
    } else {
      if (is.null(death_params[["m"]]) || is.na(death_params[["m"]])){# if nonlinear fit failed
        showNotification("Failure to fit data")
        return(NULL)
      } 
      EqText <- paste0("Fit is log(Cumulative Deaths) = ",
                       signif(death_params[["m"]],3),"*Days + ",
                       signif(death_params[["b"]],3))
    }  

    # Build Est Cases from Deaths
    
    print("---- build_death_plot 1 ------")
    p <- data %>% 
        ggplot(aes(x=Date, y=Deaths)) 
    
    print("---- build_death_plot 2 ------")
    p <- p + 
      expand_limits(x = LastDate+10) +
      geom_line(data=death_fit,
                aes(x=Date, y=Deaths,
                    color="fit"),
                size=1,
                linetype="dashed") +
      geom_line(data=death_fit[1:(nrow(death_fit)-10),],
                aes(x=Date, y=Deaths,
                    color="fit" ),
                size=1,
                linetype="solid") +
      geom_point(data=death_fit[(nrow(death_fit)-9):nrow(death_fit),],
                    aes(x=Date, y=Deaths),
                 shape=20, size=2, fill="blue") 
    print("---- build_death_plot 3 ------")
    if (!in_Deaths_logscale) {
        p <- p + geom_col(alpha = 2/3)  +
             geom_label(aes(label=Deaths), 
                            stat='identity',
                            size = 3) 
     } else {
        
        p <- p + geom_point(aes(color="data"), size=2) 
     } 
    print("---- build_death_plot 4 ------")
      p <- p + geom_label(data=death_fit[(nrow(death_fit)-9):nrow(death_fit),],
                  aes(label=as.integer(Deaths+.5)),
                  hjust=1, vjust=0) +

          theme(text = element_text(size=20)) +
          labs(title=paste0("COVID-19 Deaths in ",PopLabel$Label), 
               subtitle=paste0(" as of ", lastdate))
      
    #-------------------------------- back estimate?
    print("---- build_death_plot 5 ------")
    ymax <- max(death_fit$Deaths)
    if (in_Deaths_back_est) {#  Build a model for the number of cases from deaths
      ExpLine_est <- backest_cases(in_An_DeathLag, in_An_CFR, projection=10)

    print("---- build_death_plot 6 ------")
      ymax <- max(ExpLine_est$est_cases)
      p <- p + geom_line(data=ExpLine_est,
                    aes(x=Date, y=est_cases,
                        color="est"),
                    size=1,
                    linetype="dashed") 
    
      this_day <- data$Days[length(data$Days)] # day seq of today
      indx <- match(today()-1, ExpLine_est$Date)
      p <- p + geom_point(aes(x = ExpLine_est$Date[indx] , 
                              y = ExpLine_est$est_cases[indx]), 
                          color = "black", size = 2) +
        geom_label(data = data.frame(x = ExpLine_est$Date[indx], 
                                     y = ExpLine_est$est_cases[indx]), 
                                     nudge_x=1,
                                     nudge_y=0,
                                     aes(x, y, 
                                         label = signif(ExpLine_est$est_cases[indx], 3) ))
      #     Add actual cases
      p <- p + geom_point(data=subdata, aes(y=Cases, x=Date, color="data"), size=2)# +
               #geom_text(data=subdata, aes(y=Cases, x=Date, label=Cases),
               #          nudge_x=-1.50, nudge_y=0.0)
      
      p <-  build_legend(p, "Deaths",
                             c("Data", "Fit", "Est Cases"), # Labels for legend
                             c("black", "blue", "red"), # Color values
                             c("data", "fit", "est"), # Breaks (named lists)
                             c("black", "blue", "red"), # fill values
                         in_Deaths_logscale
                             )
    } else {
      p <-  build_legend(p, "Deaths",
                             c("Data", "Fit"), # Labels for legend
                             c("black", "blue"), # Color values
                             c("data", "fit"), # Breaks (named lists)
                             c("black", "blue"), # fill values
                         in_Deaths_logscale
                             )
    }      
    #-------------------------------- Log scaling?
    
    upper_limit <- ymax
    if (in_Deaths_zoom) { upper_limit <- ymax*6}
    
      if (in_Deaths_logscale) {
        min_limit <- min(death_fit$Deaths[1], 2)
        p <- p + scale_y_continuous(limits=c(min_limit, 6*ymax),
                                    trans="log10",
                                    breaks = logTicks(n = 4), 
                                    minor_breaks = logTicks(n = 40)) 
      } else {
        min_limit <- 0
        p <- p + scale_y_continuous(limits=c(min_limit, ymax),
                                    trans="identity")
      }
    
    print(":::::::  displayed_data")
    if (in_death_modeling=="death_logistic") {
      output$death_details <- data_details_l(subdata, 
                                            "Deaths", 
                                            EqText, 
                                            death_params[["K"]], 
                                            death_params[["xmid"]], 
                                            death_params[["r"]])
    } else {
      output$death_details <- data_details(subdata,
                                          "Deaths",
                                          EqText,
                                          death_params[["m"]],
                                          death_params[["b"]],
                                          death_params[["Rsqr"]])
    }
    
    return(p)
  }
  
  #---------------------------------------------------    
  #------------------- Build Data Details Panel ------
  #---------------------------------------------------    
  data_details <- function(data, variable, EqText, m, b, rsqr) {
    renderUI({
      str1 <- paste("Most recent value, on",data$Date[nrow(data)],
                    "was<b>", formatC(data[nrow(data), variable],
                                      format="d", big.mark=","),"</b>",variable)
      str3 <- paste("           Doubling Time =", signif(log10(2)/m,2), "days",
                    "<br> ", EqText)
      
      if (!is.nan(rsqr)){
        if (rsqr>.8) {
          str2 <- paste("R<sup>2</sup> value for fit =", signif(rsqr,4))
        } else {
          str2 <- paste("<font color=\"red\">R<sup>2</sup> value for fit =", 
                        signif(rsqr,4),
                        "<b>which is poor</b></font>")
        }
        HTML(paste(str1, str3, str2, sep = '<br/>'))
      } else {
        HTML(paste(str1, str3, sep = '<br/>'))
      }
    })
  }  
  
  #---------------------------------------------------    
  #------------------- Build Data Details Panel for Logistic
  #---------------------------------------------------    
  data_details_l <- function(data, variable, EqText, K, xmid, r) {
    renderUI({
      str1 <- paste("Most recent value, on",data$Date[nrow(data)],
                    "was<b>", data[nrow(data), variable],"</b>",variable)
      str3 <- paste("Asymptote for fit is",signif(K,3),"cases",
                    "<br> ", EqText)
      str2 <- paste("Growth rate =", signif(r,3), 
                    "&nbsp; &nbsp; &nbsp; &nbsp; Date of inflection is",
                    data$Date[1]+as.integer(xmid+0.5))
      HTML(paste(str1, str3, str2, sep = '<br/>'))
    })
  }
  
  #---------------------------------------------------    
  #------------------- Build Slope Plot --------------
  #---------------------------------------------------    
  
  build_slope_plot <- function(
                                in_window,
                                in_slopetype,
                                in_smooth,
                                in_smthlength=3
                                ){
    print(":::::::  build_slope_plot")
    halfwidth <- as.integer(in_window/2)
    Population <- PopLabel$Population
    print("-------- slope  1 --------------")
    foo <- subdata %>% 
      arrange(Cases) %>% 
      mutate(Pct_change=100*new_cases/lag(Cases, default=Cases[1])) %>% 
      mutate(avg_pct_chg=zoo::rollmean(Pct_change, in_window, 
                                  fill=c(0, NA, last(Pct_change)))) %>% 
      mutate(avg_chg=zoo::rollmean(new_cases, in_window, 
                              fill=c(0, NA, last(new_cases)))) %>% 
      mutate(avg_pct_chg=na_if(avg_pct_chg, 0)) %>% 
      mutate(Pct_change=na_if(Pct_change, 0)) %>% 
      mutate(new_cases=na_if(new_cases, 0)) %>% 
      mutate(chgpercapita=1.e5*new_cases/Population) %>% 
      mutate(avg_chgpercapita=1.e5*avg_chg/Population) #%>% 
      #mutate(avg_pct_chg=replace(avg_pct_chg, avg_pct_chg>30, NA)) %>% 
      #mutate(Pct_new_cases=replace(Pct_change, Pct_change>30, NA))
    print("-------- slope  2 --------------")
    #browser()
    foo <- foo %>% 
      mutate(m = case_when(
        in_slopetype=="percent" ~ Pct_change,
        in_slopetype=="avgpercent" ~ avg_pct_chg,
        in_slopetype=="newcase" ~ new_cases,
        in_slopetype=="avgnewcase" ~ avg_chg,
        in_slopetype=="newcasepercap" ~ chgpercapita,
        in_slopetype=="avgnewcasepercap" ~ avg_chgpercapita,
        TRUE ~ 0
      ))
    print("-------- slope  3 --------------")
    
    my_title <- list("cases"="Slope of Cum Case Count in ",
                  "percent"="Percent Change of Cases in ",
                  "doubling"="Doubling Time for ",
                  "avgpercent"="Avg Pct Change in Cases in ",
                  "newcase"="New Cases in ",
                  "avgnewcase"="Avg New Cases in ",
                  "newcasepercap"="New Cases per 100,000 in ",
                  "avgnewcasepercap"="Avg New Cases per 100,000 in ")
    my_ylab <- list("cases"="Slope: Change in cum num cases/ num days ",
                "percent"="Daily percent change",
                "doubling"="Doubling Time in Days",
                "avgpercent"="Avg Pct Change in Cases",
                "newcase"="New Cases",
                "avgnewcase"="Avg New Cases",
                "newcasepercap"="New Cases per 100,000",
                "avgnewcasepercap"="Avg New Cases per 100,000")
    
    print("-------- slope  4 --------------")
    if (in_slopetype=="cases") {
      foo <- foo %>% 
        mutate(log_cases=Cases)
    } else {
      foo <- foo %>% 
        mutate(log_cases=log10(Cases))
    }
    
    print("-------- slope  5 --------------")
    if (in_slopetype=="doubling" || in_slopetype=="cases") { # calc slope
    print("-------- slope  5.1 --------------")
      foo <- foo %>%
        mutate(
          model = slide(
            .x = tibble(Days = Days, log_cases = log_cases), 
            .f = ~lm(log_cases ~ Days, .x), 
            .before = halfwidth, 
            .after = halfwidth,
            .complete = TRUE
          ),
          tidied=map(model, tidy)
        ) %>% 
        unnest(tidied) %>% 
        filter(term=="Days") %>% 
        select(-log_cases, -model, -term) %>% 
        rename(sd=std.error) %>% 
        mutate(m=estimate)
    }
    
    print("-------- slope  5.5 --------------")
   # if (!grepl("percent", in_slopetype)) {
   #   foo <- foo %>% filter(m>0.0)
   # }
    #   calculate doubling time
    if (in_slopetype=="doubling") {
      foo <- foo %>% 
        mutate(m=signif(log10(2)/m,2),
               sd=signif(log10(2)/(m-sd),2)) %>% 
        mutate(m=replace(m, m>200, NA)) %>%  
        mutate(m=replace(m, m< -200, NA)) 
       # filter(m<200)
    }
    print("-------- slope  5.75 --------------")
    #---------------   smoothing
    if (in_smooth) {
      #foo$m <- fractal::medianFilter(foo$m,in_smthlength)
      foo$m <- zoo::rollmedian(foo$m, in_smthlength, 
                    fill=c(0, NA, last(foo$m)))
    }
    
    print("-------- slope  6 --------------")
    foo
    #-------------------------   plot
    
    p <- foo %>% 
      ggplot(aes(x=Date, y=m)) +
      geom_point() +
      theme(text = element_text(size=20)) +
      geom_smooth() +
      labs(title=paste0(my_title[[in_slopetype]]
                        ,PopLabel$Label),
           y=my_ylab[[in_slopetype]])
    
    if (in_slopetype=="cases") {
      p <- p +
      geom_errorbar(aes(ymax=m+sd, ymin=m-sd)) +
        labs(subtitle=paste0("Fit over ",in_window," days"))
    }
  return(p)
  }
   
  #---------------------------------------------------    
  #------------------- Build Counties Plot -----------
  #---------------------------------------------------    
  
  build_counties_plot <- function(
                                in_counties_y_axis,
                                in_counties_selector,
                                in_case_start,
                                in_county_log
                                ){
    print(":::::::  build_counties_plot")
    print(paste(in_counties_y_axis, in_counties_selector))
    
    window <- 5
    

    
    print("-------------  counties plot 1")
    
    #---------------  Control matrix
    
    calc_controls <- tribble(
      ~base,       ~avg, ~percap, ~trim, ~positive,
      "Cases",      TRUE, TRUE,  FALSE, TRUE,
      "Deaths",     TRUE, TRUE,  FALSE, TRUE,
      "pct_chg",    TRUE, FALSE, TRUE, TRUE,
      "doubling",   TRUE, FALSE, TRUE, TRUE,
      "new_cases",  TRUE, TRUE,  TRUE, TRUE,
      "new_deaths", TRUE, TRUE,  TRUE, TRUE
    )
    
    #--------------- Clean up unuseable choices
    
    if (grepl("percap", in_counties_y_axis)&
        (grepl("pct_chg",in_counties_y_axis)
         || grepl("doubling",in_counties_y_axis))) {
      in_counties_y_axis <- str_remove(in_counties_y_axis, "_percap")
    }
    if (grepl("percap", in_counties_selector)&
        (grepl("pct_chg",in_counties_selector)
         || grepl("doubling",in_counties_selector))) {
      in_counties_selector <- str_remove(in_counties_selector, "_percap")
    }
    
    print("-------------  counties plot 2")
    # Start each county at the minimum case spot and create an x-axis variable
    counties_case <- counties %>% 
      filter(Cases>in_case_start) %>%  
      group_by(County) %>% 
        arrange(Date) %>% 
        mutate(day = row_number()) %>% 
        add_tally() %>% 
      ungroup() %>% 
      filter(n>5) # must have at least 5 datapoints
    
    print("-------------  counties plot 3")
    y_labels <- list(
                     "Cases"="Number of Cases",
                     "Cases_percap"="Cases per 100,000",
                     "avg_Cases"="Avg Cases",
                     "avg_Cases_percap"="Avg Cases per 100,000",
                     "new_cases"="Number of New Cases",
                     "new_cases_percap"="New Cases per 100,000",
                     "avg_new_cases"="Avg New Cases",
                     "avg_new_cases_percap"="Avg New Cases per 100,000",
                     "Deaths"="Number of Deaths",
                     "Deaths_percap"="Deaths per 100,000",
                     "avg_Deaths"="Avg Deaths",
                     "avg_Deaths_percap"="Avg Deaths per 100,000",
                     "new_deaths"="Number of New Deaths",
                     "new_deaths_percap"="New Deaths per 100,000",
                     "avg_new_deaths"="Avg New Deaths",
                     "avg_new_deaths_percap"="Avg New Deaths per 100,000",
                     "pct_chg"="Percent Change",
                     "avg_pct_chg"="5-day Avg Percent Change",
                     "doubling"="Doubling Time in Days",
                     "avg_doubling"="Avg Doubling Time in Days"
                     )
    
    print("-------------  counties plot 4")
    #     Apply selector
    sorting <- grepl("doubling", in_counties_selector)
    print(paste("--->>> sorting = ", sorting))
    
    title_label <- "Greatest"
    if (sorting) {title_label <- "Smallest"}
    
    do_sort <- function(df, sorting) {
      if (sorting){
        print("a")
        dplyr::arrange(df, Mselect)
      } else {
        print("b")
        dplyr::arrange(df, desc(Mselect))
        }
    }
    do_filter <- function(df, sorting) {
      if (sorting){
        print("c")
        dplyr::filter(df, Mselect<(unique(Mselect)[7]))
      } else {
        print("d")
        dplyr::filter(df, Mselect>(unique(Mselect)[7]))
        }
    }
    
    counties_case %>% 
      arrange(Date) %>% 
      group_by(County) %>% 
        mutate(Mselect=last(!!as.name(in_counties_selector))) %>% 
        mutate(end_case=last(!!as.name(in_counties_y_axis)), end_day=max(day)) %>% 
        do_sort(sorting) %>% 
<<<<<<< HEAD
      ungroup() %>% 
      do_filter(sorting) %>% 
=======
        #arrange(desc(Mselect)) %>% 
      ungroup() %>% 
      do_filter(sorting) %>% 
      #filter(Mselect>(unique(Mselect)[7])) %>% 
>>>>>>> Counties
      select(-Mselect) -> counties_case_filt
    
    #   Stretch scale
    daylimit <- max(counties_case_filt$day)*1.1
    
    print("-------------  counties plot 5")
    #   Plot county data
    p <- 
    counties_case %>% 
      ggplot(aes(x=day, y=!!as.name(in_counties_y_axis))) + 
      #scale_y_log10(breaks = logTicks(n = 4), minor_breaks = logTicks(n = 40)) +
      theme(legend.position = "none", text = element_text(size=20)) +
      geom_line(aes(group=County),colour = alpha("grey", 0.7)) +
      geom_line(data=counties_case_filt,
                aes(color=County)) + 
      geom_label(data=counties_case_filt,
                 aes(y=end_case,x=end_day,label=County, color=County),
                 size=3.0,
                 label.size = 0.15,
                 vjust="top", hjust="left") +
      expand_limits(x=daylimit) + # make room for labels
      labs(title=paste("Counties With",title_label,y_labels[[in_counties_selector]]),
           x=paste0("Days after reaching ",in_case_start," Cases"),
           y=paste(y_labels[[in_counties_y_axis]])) 
    
    if (in_county_log){
      p <- p + scale_y_log10(breaks = logTicks(n = 4), minor_breaks = logTicks(n = 40))
    }
    
    print("-------------  counties plot 6")
    #-----------   Data details
    output$counties_details <- gt::render_gt({
      
      details <- counties_case_filt %>% 
        group_by(County) %>% 
          summarise(signif(last(!!as.name(in_counties_selector)),3),
                    last(Cases)) %>% 
        rename(!!sym(in_counties_selector):=2, Cases=3) %>% 
        mutate(label=y_labels[[in_counties_selector]]) %>% 
        arrange(desc(!!as.name(in_counties_selector))) %>% 
        mutate(text=paste0(County,": ", label, " = ", 
                           !!as.name(in_counties_selector),
                           " and Total Cases = ", Cases))
      
    details %>% select(-label, -text) %>%
      gt::gt() %>%
      gt::tab_header(title="Highlighted Details") %>% 
      gt::cols_label(County=gt::md("**County**"), 
                     !!sym(in_counties_selector):=gt::md(paste0("**",y_labels[[in_counties_selector]],"**")), 
                     Cases=gt::md("**Cases**")) %>% 
      gt::tab_style(style=gt::cell_fill(color="lightcyan"),
                    locations=gt::cells_title())
    
     # HTML(paste(details$text[1:6], collapse = '<br/>'))
      
    })
    
    
    return(p)
    
  }
  ######################  Map ########################
  #---------------------------------------------------    
  #------------------- Build Model -------------------
  #---------------------------------------------------    
  
  draw_map <- function() {
    
    print("Map --1--")
    Range <- range(MappingData$percapita, na.rm=TRUE)
    DPerCRange <- range(MappingData$DPerC, na.rm=TRUE)
    DPerCapRange <- range(MappingData$DPerCap, na.rm=TRUE)
    CaseRange <- range(MappingData$Cases, na.rm=TRUE)
    DeathRange <- range(MappingData$Deaths, na.rm=TRUE)
    SlopeRange <- range(MappingData$m, na.rm=TRUE)
    DoubleRange <- range(MappingData$double, na.rm=TRUE)
    avgpctRange <- range(MappingData$avgpct, na.rm=TRUE)
    print(as.character(seq(DPerCRange[1], DPerCRange[2], length.out = 5)))
    
    print("Map --2--")
    #   Calculate proper number of quantile cuts
    nCase <- as.integer(sum(MappingData$Cases>0, na.rm=TRUE)/
                          sum(MappingData$Cases==1, na.rm=TRUE))
    nDeath <- as.integer(sum(MappingData$Deaths>0, na.rm=TRUE)/
                           sum(MappingData$Deaths==1, na.rm=TRUE))
    
    palcap <-colorQuantile(palette = heat.colors(8), 
                           domain = MappingData$percapita, 
                           n = 8, 
                           na.color = "transparent", 
                           alpha = FALSE, 
                           reverse = TRUE,
                           right = FALSE) 
    
    print("Map --3--")
    palcase <- colorQuantile(
                            na.color = "transparent",
                            palette = heat.colors(nCase),
                            n = nCase, 
                            reverse=TRUE,
                            right = FALSE,
                            domain = MappingData$Cases)
    
    print("Map --4--")
    paldeath <- colorNumeric(
                            na.color = "transparent",
                            palette = heat.colors(8),
                            reverse=TRUE,
                            domain = MappingData$Deaths)
    
    palslope <- colorNumeric(
                            na.color = "transparent",
                            palette = heat.colors(8),
                            reverse=TRUE,
                            domain = MappingData$m)
    
    paldouble <- colorQuantile(
                            na.color = "transparent",
                            palette = heat.colors(8),
                            #reverse=TRUE,
                            n = 8, 
                            right = FALSE,
                            domain = MappingData$double)
    
    paldeathpercap <- colorQuantile(
                            na.color = "transparent",
                            palette = heat.colors(8),
                            n = 8, 
                            reverse=TRUE,
                            right = FALSE,
                            domain = MappingData$DPerCap)
    palavgpct <- colorNumeric(
                            na.color = "transparent",
                            palette = heat.colors(8),
                            #n = 5, 
                            reverse=TRUE,
                            #right = FALSE,
                            domain = MappingData$avgpct)
    
    paldeathpercase <- colorQuantile(
                            na.color = "transparent",
                            palette = heat.colors(5),
                            n = 5, 
                            reverse=TRUE,
                            right = FALSE,
                            domain = MappingData$DPerC)
    print("Map --5--")
    if (input$county_color=="casetotal") {
    print("Map --6--")
      output$TexasMap <- renderLeaflet({############   Total Cases
        #   Basemap
        leaflet(MappingData) %>% 
          setView(lng = MapCenter[1] , lat = MapCenter[2], zoom = init_zoom ) %>%   
          addTiles() %>%
          addPolygons(data = MappingData, 
                      group="cases",
                      stroke = TRUE,
                      weight = 1,
                      smoothFactor = 0.2, 
                      fillOpacity = 0.7,
                      label = MapLabels,
                      fillColor = ~palcase(MappingData$Cases)) %>% 
          addLegend("bottomleft", pal = palcase, values = ~Cases, 
                    title = "Total Cases",
                    labels= as.character(seq(CaseRange[1], CaseRange[2], length.out = 5)),
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0(signif(cuts[-n],2), " &ndash; ", signif(cuts[-1],2))
                    },
                    opacity = 1)
      }) 
    } else if (input$county_color=="percapita") { ######### Cases per 100k
      output$TexasMap <- renderLeaflet({
        #   Basemap
        leaflet(MappingData) %>% 
          setView(lng = MapCenter[1] , lat = MapCenter[2], zoom = init_zoom ) %>%   
          addTiles() %>%
          addPolygons(data = MappingData, 
                      group="percapita",
                      stroke = TRUE,
                      weight = 1,
                      smoothFactor = 0.2, 
                      fillOpacity = 0.7,
                      label = MapLabels,
                      fillColor = ~palcap(MappingData$percapita)) %>% 
          addLegend("bottomleft", pal = palcap, values = ~percapita, 
                    title = "Cases per 100,000",
                    labels= as.character(seq(Range[1], Range[2], length.out = 8)),
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0(signif(cuts[-n],2), " &ndash; ", signif(cuts[-1],2))
                    },
                    opacity = 1)
      }) 
    } else if (input$county_color=="deaths")  { #############  Deaths
      output$TexasMap <- renderLeaflet({
        #   Basemap
        leaflet(MappingData) %>% 
          setView(lng = MapCenter[1] , lat = MapCenter[2], zoom = init_zoom ) %>%   
          addTiles() %>%
          addPolygons(data = MappingData, 
                      group="deaths",
                      stroke = TRUE,
                      weight = 1,
                      smoothFactor = 0.2, 
                      fillOpacity = 0.7,
                      label = MapLabels,
                      fillColor = ~paldeath(MappingData$Deaths)) %>% 
          addLegend("bottomleft", pal = paldeath, values = ~Deaths, 
                    title = "Total Deaths",
                    opacity = 1)
      }) 
    } else if (input$county_color=="case_slope")  { #############  Slope
      output$TexasMap <- renderLeaflet({
        #   Basemap
        leaflet(MappingData) %>% 
          setView(lng = MapCenter[1] , lat = MapCenter[2], zoom = init_zoom ) %>%   
          addTiles() %>%
          addPolygons(data = MappingData, 
                      group="slope",
                      stroke = TRUE,
                      weight = 1,
                      smoothFactor = 0.2, 
                      fillOpacity = 0.7,
                      label = MapLabels,
                      fillColor = ~palslope(MappingData$m)) %>% 
          addLegend("bottomleft", pal = palslope, values = ~m, 
                    title = "Slope of cases per capita",
                    opacity = 1)
      }) 
    } else if (input$county_color=="double")  { #############  Doubling
      output$TexasMap <- renderLeaflet({
        #   Basemap
        leaflet(MappingData) %>% 
          setView(lng = MapCenter[1] , lat = MapCenter[2], zoom = init_zoom ) %>%   
          addTiles() %>%
          addPolygons(data = MappingData, 
                      group="double",
                      stroke = TRUE,
                      weight = 1,
                      smoothFactor = 0.2, 
                      fillOpacity = 0.7,
                      label = MapLabels,
                      fillColor = ~paldouble(MappingData$double)) %>% 
          addLegend("bottomleft", pal = paldouble, values = ~double, 
                    title = "Doubling Time",
                    labels= as.character(seq(DoubleRange[1], DoubleRange[2], length.out = 8)),
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0(signif(cuts[-n],2), " &ndash; ", signif(cuts[-1],2))
                    },
                    opacity = 1)
      }) 
    } else if (input$county_color=="deathspercap")  { #############  Deaths per 100k
      output$TexasMap <- renderLeaflet({
        #   Basemap
        leaflet(MappingData) %>% 
          setView(lng = MapCenter[1] , lat = MapCenter[2], zoom = init_zoom ) %>%   
          addTiles() %>%
          addPolygons(data = MappingData, 
                      group="deathspercap",
                      stroke = TRUE,
                      weight = 1,
                      smoothFactor = 0.2, 
                      fillOpacity = 0.7,
                      label = MapLabels,
                      fillColor = ~paldeathpercap(MappingData$DPerCap)) %>% 
          addLegend("bottomleft", pal = paldeathpercap, values = ~DPerCap, 
                    title = "Deaths per 100,000",
                    labels= as.character(seq(DPerCapRange[1], DPerCapRange[2], length.out = 8)),
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0(signif(cuts[-n],2), " &ndash; ", signif(cuts[-1],2))
                    },
                    opacity = 1)
      }) 
    } else if (input$county_color=="avg_chg")  { #############  Avg Pct Chg
      output$TexasMap <- renderLeaflet({
        #   Basemap
        leaflet(MappingData) %>% 
          setView(lng = MapCenter[1] , lat = MapCenter[2], zoom = init_zoom ) %>%   
          addTiles() %>%
          addPolygons(data = MappingData, 
                      group="avgpctchg",
                      stroke = TRUE,
                      weight = 1,
                      smoothFactor = 0.2, 
                      fillOpacity = 0.7,
                      label = MapLabels,
                      fillColor = ~palavgpct(MappingData$avgpct)) %>% 
          addLegend("bottomleft", pal = palavgpct, values = ~avgpct, 
                    title = "Recent Avg Pct Change",
                    #labels= as.character(seq(avgpctRange[1], avgpctRange[2], length.out = 8)),
                    #labFormat = function(type, cuts, p) {
                    #  n = length(cuts)
                    #  paste0(signif(cuts[-n],2), " &ndash; ", signif(cuts[-1],2))
                    #},
                    opacity = 1)
      }) 
    } else { # deathpercase
      output$TexasMap <- renderLeaflet({ ########## Deaths per case
        #   Basemap
        leaflet(MappingData) %>% 
          setView(lng = MapCenter[1] , lat = MapCenter[2], zoom = init_zoom ) %>%   
          addTiles() %>%
          addPolygons(data = MappingData, 
                      group="DPerC",
                      stroke = TRUE,
                      weight = 1,
                      smoothFactor = 0.2, 
                      fillOpacity = 0.7,
                      label = MapLabels,
                      fillColor = ~paldeathpercase(MappingData$DPerC)) %>% 
          addLegend("bottomleft", pal = paldeathpercase, values = ~DPerC, 
                    title = "Deaths per Case",
                    labels= as.character(seq(DPerCRange[1], DPerCRange[2], length.out = 5)),
                    labFormat = function(type, cuts, p) {
                      n = length(cuts)
                      paste0(signif(cuts[-n],2), " &ndash; ", signif(cuts[-1],2))
                    },
                    opacity = 1)
      }) 
      
    }
    print("Map --7--")
  } 
   
  #-------------------------------------------------------    
  #------------------- Reactive bits ---------------------
  #-------------------------------------------------------    

  observeEvent(input$tabs, { # do stuff when tab changes
    print(paste("tab:", input$tabs))  
    #if (input$tabs=="MapTab") { ##  Map Tab ##
    #  draw_map()
    #}
    if (input$tabs=="AnalysisTab") { ## Analysis Tab ##
      #prep_An_data(input$An_dataset, 
      #             input$An_area)
    }
  })
    
   
  #---------------------------------------------------    
  #------------------- Select Data -------------------
  #---------------------------------------------------    
  observeEvent({
                input$dataset
                input$region
                input$county
                input$dateRange
                1}, { # Change data selection
    print(":::::::  observe_event Analysis Data")
                  
#  Set up geographic area desired and create data selection
    in_area <- case_when(
      input$dataset == "Region" ~ input$region,
      input$dataset == "County" ~ input$county
    )
    # ===============================
    prep_data(input$dataset,
              in_area,
              input$dateRange
    )           
    # ===============================
    
    # fit a model to the data
    if (input$modeling=="logistic") {
      fit_logistic()
    } else {
      m <- input$slope
      b <- input$intercept
      if (input$modeling=="do fit") {
        fit_type <- "all"
      } else if (input$modeling == "standard") {
        fit_type <- "b_only"
        m <- global_slope
      } else if (input$modeling == "user") {
        fit_type <- "none"
      }
        
    #############   fit cases
      fit_exponential(indep="Cases",
                      fit_type, 
                      m, 
                      b,
                      cutoff=1,
                      projection=10)
    }
    #############   fit deaths      
    if (input$death_modeling=="death_exp") {
      fit_exponential(indep="Deaths",
                      fit_type="all", 
                      m, 
                      b,
                      cutoff=1,
                      projection=10)
     } else {
      fit_logistic(indep="Deaths")
     }
    
    #if (input$Deaths_back_est) { # optional
    #  backest_cases(input$An_CFR, input$An_DeathLag, projection=10)
    #}
      
  #---------------------------------
  #-------------Cases tab this is here for initial state
  #---------------------------------
      if (input$An_tabs == "Cases") {
        if ((sum(!is.na(subdata$Cases))>3))
        {
          p <- build_basic_plot(input$modeling, 
                                input$slope,
                                input$intercept,
                                input$logscale,
                                input$zoom,
                                input$estmiss,
                                input$avoid,
                                input$recover_days)
          
          if(!is.null(p)){
            output$plot_cases <- renderPlot({
              print(p)
            })
          }
        } else {
          showNotification("Too few Cases for fitting")
        }        
      }
  #---------------------------------
  #-------------Deaths tab
  #---------------------------------
      if (input$An_tabs == "Deaths") {
        if ((sum(!is.na(subdata$Deaths))>2) &
            (span(subdata$Deaths)>0)) {
          p <- build_deaths_plot(
                                 input$Deaths_logscale,
                                 input$Deaths_zoom,
                                 input$An_CFR,
                                 input$An_DeathLag,
                                 input$Deaths_back_est,
                                 input$death_modeling
                                 )
          output$plot_deaths <- renderPlot({print(p)})
        } else {
          showNotification("Too little death data")
        }
      }
  #---------------------------------
  #------------- Indicators tab
  #---------------------------------
      if (input$An_tabs == "Indicators") {
        if (sum(!is.na(subdata$Cases))>15) {
          p <- build_slope_plot(
                                 input$window,
                                 input$slopetype,
                                 input$smooth,
                                 input$smthlength
                                 )
          output$plot_slopes <- renderPlot({print(p)})
        } else {
          showNotification("Too little case data")
        }
      } 
      if (input$An_tabs == "Tests") {
         # p <- build_tests_plot()
      } 
      if (input$An_tabs == "Something") {
         # p <- build_something()
      }
      
      print("============== end select An data ==================")
  }) #   end of respond to data change
   
  #---------------------------------------------------    
  #------------------- Rerun fitting -----------------
  #---------------------------------------------------    
  
  observeEvent({ #  fit cases
                input$modeling #do fit,  standard,  user, logistic
                input$slope
                input$intercept
                1}, { # change display
                  
    # fit a model to the data
    if (input$modeling=="logistic") {
      fit_logistic()
    } else {
      m <- input$slope
      b <- input$intercept
      if (input$modeling=="do fit") {
        fit_type <- "all"
      } else if (input$modeling == "standard") {
        fit_type <- "b_only"
        m <- global_slope
      } else if (input$modeling == "user") {
        fit_type <- "none"
      }
        
      fit_exponential(indep="Cases",
                      fit_type, 
                      m, 
                      b,
                      cutoff=1,
                      projection=10)
    }
#    if (input$An_tabs == "Cases") {
      p <- build_basic_plot(input$modeling,
                            input$slope,
                            input$intercept,
                            input$logscale,
                            input$zoom,
                            input$estmiss,
                            input$avoid,
                            input$recover_days)
      
      if(!is.null(p)){
        output$plot_cases <- renderPlot({
            print(p)
            })
      }
#    }
  })
  
  #---------------------------------------------------    
  #------------------- Rerun Death fitting -----------
  #---------------------------------------------------    
  
  observeEvent({  #  fit deaths back estimate
                input$Deaths_back_est
                input$An_CFR
                input$An_DeathLag 
                input$death_modeling
                1},{
    if (input$death_modeling=="death_exp") {
      fit_exponential(indep="Deaths",
                      fit_type="all", 
                      m, 
                      b,
                      cutoff=1,
                      projection=10)
    } else {
      fit_logistic(indep="Deaths")
    }
    
    #if (input$Deaths_back_est) { # optional
    #  backest_cases(input$An_CFR, input$An_DeathLag, projection=10)
    #}
    if (input$An_tabs == "Deaths") {
      if ((sum(!is.na(subdata$Deaths))>2) &
          (span(subdata$Deaths)>0)) {
        p <- build_deaths_plot(
          input$Deaths_logscale,
          input$Deaths_zoom,
          input$An_CFR,
          input$An_DeathLag,
          input$Deaths_back_est,
          input$death_modeling
        )
        output$plot_deaths <- renderPlot({print(p)})
      } else {
        showNotification("Too little death data")
      }
    }              
  })
  
  #---------------------------------------------------    
  #------------------- Deaths changes ----------------
  #---------------------------------------------------    
  observeEvent({
                input$Deaths_logscale
                input$Deaths_zoom
                input$Deaths_back_est
                input$An_CFR
                input$An_DeathLag
                input$An_tabs
                1}, { # change display
    print(":::::::  observe_event Analysis Data")
                  
      if (input$An_tabs == "Deaths") {
        if ((sum(!is.na(subdata$Deaths))>2) &
            (span(subdata$Deaths)>0)) {
          p <- build_deaths_plot(
                                 input$Deaths_logscale,
                                 input$Deaths_zoom,
                                 input$An_CFR,
                                 input$An_DeathLag,
                                 input$Deaths_back_est,
                                 input$death_modeling
                                 )
          output$plot_deaths <- renderPlot({print(p)})
        } else {
          showNotification("Too little death data")
        }
      } 
                  #   move the following stuff when implementing
      if (input$An_tabs == "Indicators") {
         # p <- build_slope_plot()
      } 
      if (input$An_tabs == "Tests") {
         # p <- build_tests_plot()
      } 
      if (input$An_tabs == "Something") {
         # p <- build_something()
      }
  })

  #---------------------------------------------------    
  #------------------- Cases changes -----------------
  #---------------------------------------------------    
  observeEvent({#input$modeling
                #input$slope
                #input$intercept
                input$zoom
                input$avoid
                input$logscale
                input$An_tabs
                input$recover_days
                1} , { # 
                  
    print(":::::::  observe_event 2")
                  
    if (input$An_tabs == "Cases") {
      p <- build_basic_plot(input$modeling,
                            input$slope,
                            input$intercept,
                            input$logscale,
                            input$zoom,
                            input$estmiss,
                            input$avoid,
                            input$recover_days)
      
      if(!is.null(p)){
        output$plot_cases <- renderPlot({
            print(p)
            })
      }
    }
  })
    
  #---------------------------------------------------    
  #------------------- Slope changes -----------------
  #---------------------------------------------------    
  observeEvent({
                input$window
                input$slopetype
                input$smooth
                input$smthlength
                input$An_tabs
                1} , { # 
      if (input$An_tabs == "Indicators") {
          if (sum(!is.na(subdata$Cases))>15) {
            p <- build_slope_plot(
              input$window,
              input$slopetype,
              input$smooth,
              input$smthlength
            )
            output$plot_slopes <- renderPlot({print(p)})
          } else {
            showNotification("Too little case data")
          }
      }
  }) 
  #---------------------------------------------------    
  #------------------- Analysis Tab ------------------
  #---------------------------------------------------    
  observeEvent({input$An_tabs
                1} , { # 
    print(":::::::  observe_event 2")
  })
  #---------------------------------------------------    
  #------------------- Counties Tab ------------------
  #---------------------------------------------------    
  observeEvent({input$CountiesTab
                input$counties_y_axis
                input$counties_selector
                input$counties_avg
                input$counties_percap
                input$counties_select_avg
                input$counties_select_percap
                input$case_start
                input$county_log
                1} , { # 
    print(":::::::  observe_event CountiesTab")
                  
                  print(paste(input$counties_y_axis,
                              input$counties_selector,
                              input$case_start,
                              input$county_log))
                  
    y_axis <- input$counties_y_axis
    if (input$counties_avg) {y_axis <- paste0("avg_", y_axis)}
    if (input$counties_percap) {y_axis <- paste0(y_axis,"_percap")}
                  
    selector <- input$counties_selector
    if (input$counties_select_avg) {selector <- paste0("avg_", selector)}
    if (input$counties_select_percap) {selector <- paste0(selector,"_percap")}
    
    p <- build_counties_plot(y_axis,
                             selector,
                             input$case_start,
                             input$county_log
                            )
                  
    output$plot_counties <- renderPlot({print(p)})             
  })
    
    
  #---------------------------------------------------    
  #------------------- Mapping Controls --------------
  #---------------------------------------------------    
  observeEvent({
    input$county_color
    1} , { #  draw map
      draw_map()
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

