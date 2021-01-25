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
library(broom)
library(purrr)
library(rsample) # for bootstrap
library(plotly)
#library(slider)


###################################
#   get and set up the basic data
###################################

#   Directory where data is stored

DataLocation <- "https://www.ajackson.org/Covid/"
DataArchive <- "https://www.ajackson.org/SharedData/"


###%%%  Temporary for coding and debugging
##path <-  "/home/ajackson/Dropbox/Rprojects/Covid/Today_Data/"
##county_animate <- readRDS(paste0(path, "Today_County_Animate.rds"))
###%%%

#   Tibble database

#   County Map Animation data

z <- gzcon(url(paste0(DataLocation, "Today_County_Animate.rds")))
county_animate <- readRDS(z)
close(z)

#   Case data
z <- gzcon(url(paste0(DataLocation, "Today_County_calc.rds")))
County_calc <- readRDS(z)
close(z)
#   Testing data
z <- gzcon(url(paste0(DataLocation, "Today_TestingData.rds")))
TestingData <- readRDS(z)
close(z)
#    County Testing Data
#z <- gzcon(url(paste0(DataLocation, "Today_County_Testing.rds")))
#CountyTestingData <- readRDS(z)
#close(z)
#   County Population data
z <- gzcon(url(paste0(DataLocation, "Today_County_pop.rds")))
County_pop <- readRDS(z)
close(z)
#   Current Prison epidemic data
z <- gzcon(url(paste0(DataLocation, "Today_Prison_data.rds")))
Prison_data <- readRDS(z)
close(z)
#   MSA data
z <- gzcon(url(paste0(DataLocation, "Today_MSA_raw.rds")))
MSA_raw <- readRDS(z)
close(z)
z <- gzcon(url(paste0(DataLocation, "Today_MSAs.rds")))
MSAs <- readRDS(z)
close(z)
#   Mapping data
z <- gzcon(url(paste0(DataLocation, "Today_MappingData.rds")))
MappingData <- readRDS(z)
close(z)
z <- gzcon(url(paste0(DataLocation, "Today_MapLabels.rds")))
MapLabels <- readRDS(z)
close(z)
MappingData <- sf::st_as_sf(MappingData)

#   Harris County Data
z <- gzcon(url(paste0(DataLocation, "Today_Harris_zip.rds")))
Harris_zip <- readRDS(z)
close(z)
z <- gzcon(url(paste0(DataLocation, "Today_Harris_schools.rds")))
Harris_schools <- readRDS(z)
close(z)
z <- gzcon(url(paste0(DataLocation, "Harris_School_Polys.rds")))
Harris_school_polys <- readRDS(z)
close(z)
z <- gzcon(url(paste0(DataLocation, "HarrisCounty_CensusByZip_polys.rds")))
Harris_zip_polys <- readRDS(z)
close(z)

f <- function(a,b){100*a/b}
Harris_zip_polys <- Harris_zip_polys %>% 
  mutate_at(vars(matches("^Age")), funs(f(.,Pop))) %>% 
  rename(Zip=ZCTA)
Harris_school_polys <- Harris_school_polys %>% 
  mutate_at(vars(matches("^Age")), funs(f(.,Pop)))  

har_choices <- list(
  "Med Income"="Med_Income",
  "Median Age"="MedianAge",
  "White Pct"="WhitePct",
  "Black Pct"="BlackPct",
  "Hispanic Pct"="HispanicPct",
  "Multigen Pct"="MultigenPct",
  "Density"="Density",
  "blueness"="blueness",
  "Age 0to4"="Age0to4",
  "Age 5to9"="Age5to9",
  "Age 10to14"="Age10to14",
  "Age 15to19"="Age15to19",
  "Age 20to24"="Age20to24",
  "Age 25to34"="Age25to34",
  "Age 35to44"="Age35to44",
  "Age 45to54"="Age45to54",
  "Age 55to59"="Age55to59",
  "Age 60to64"="Age60to64",
  "Age 65to74"="Age65to74",
  "Age 75to84"="Age75to84",
  "Age 85andup"="Age85andup"
)

Harris_last <- last(Harris_schools$Date) # last date for schools
map_data <- tribble(
  ~Zip, ~week,
  "77000", 1
) # fake initialization

init_zoom <- 6
MapCenter <- c(-99.9018, 31.9686) # center of state

global_slope <- 0.13
# https://dartthrowingchimp.shinyapps.io/covid19-app/

#   Last date in dataset formatted for plotting

sf <- stamp_date("Sunday, Jan 17, 1999")
lastdate <- sf(County_calc$Date[nrow(County_calc)])

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
  
  #---------------  Control matrix, available everywhere
  
  calc_controls <<- tribble(
    ~base,       ~avg, ~percap, ~trim, ~positive,
    "Cases",      TRUE, TRUE,  FALSE, TRUE,
    "Deaths",     TRUE, TRUE,  FALSE, TRUE,
    "pct_chg",    TRUE, FALSE, FALSE, TRUE,
    "doubling",   TRUE, FALSE, TRUE, TRUE,
    "active_cases", TRUE, TRUE,  FALSE, TRUE,
    "deaths_percase", FALSE, FALSE,  TRUE, TRUE,
    "Pct_pos", FALSE, FALSE,  FALSE, TRUE,
    "new_cases",  TRUE, TRUE,  FALSE, TRUE,
    "new_deaths", TRUE, TRUE,  FALSE, TRUE
  )

#   Has animated basemap and legend been drawn?

draw_anim_map <<- FALSE

draw_anim_legend <<- "none"

#   Last date in animate file

Animate_last <- max(county_animate$Date)

#   Labels for map animation

print("--------1----------")

span <- function(vector){ # range spanned by a vector
  foo <- range(vector, na.rm=TRUE)
  return(max(foo) - min(foo))
}

###############     modules

#----------------------------------------------------------
#               create attribute selector
#----------------------------------------------------------

attribute_select_UI <- function(id, label="Choose the Y-Axis") {
  ns <- NS(id)
  tagList(
    h4(label),
    checkboxInput(
      ns("Running_average"),
      label = "Running Average",
      value = TRUE
    ),
    checkboxInput(
      inputId = ns("Percap"),
      label = "per 100,000",
      value = TRUE
    ),
    HTML("<hr>"),
    radioButtons(
      ns("Attribute"),
      label = NULL,
      choices = list(
        "Cases" = "Cases",
        "New Cases" = "new_cases",
        "Active Cases" = "active_cases",
        "Deaths" = "Deaths",
        "New Deaths" = "new_deaths",
        "Deaths per Case" = "deaths_percase",
        "Percent change" = "pct_chg",
        "Doubling Time" = "doubling"
      ),
      selected = "Cases"
    ) 
  )
}

attribute_select_server <- function(input, output, session, tab_name) {
  
  returnval <- reactiveVal(NA_character_)
                 
  observeEvent({input$Attribute
               input$Running_average
               input$Percap
               1},{ 
                 
    y_axis <- input$Attribute
    if (input$Running_average &
        calc_controls$avg[calc_controls$base==input$Attribute]) {
      y_axis <- paste0("avg_", y_axis)}
    if (input$Percap &
        calc_controls$percap[calc_controls$base==input$Attribute]) {
      y_axis <- paste0(y_axis,"_percap")}
    print(paste("--- select:", y_axis))
    
    returnval(list("y_axis"=y_axis))
    
  }, ignoreInit = FALSE)
  returnval
}


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
                              height = "700px"),
                   h4("Details on displayed data"),
                   htmlOutput("indicator_details")
         ), # end tab panel Indicators
         tabPanel( ##########   Tests
                   "Testing",
                   fluid = TRUE,
                   value = "Tests",
                   HTML("<hr>"),
                   plotOutput("plot_tests",
                              height = "700px")
         ) # end tab panel Tests
         #tabPanel(
         #          "Something",
         #          fluid = TRUE,
         #          value = "Something",
         #          HTML("<hr>")
         # ) # end tab panel Something
        ) # end TabSet panel An_tabs
      ), # end column 
            #-------------------- Data Selection
         column(3, # Controls
              wellPanel( # Data Select
                 h4("Choose the data"),
                 radioButtons(
                   "dataset",
                   label = NULL,
                   choices = list("Region" = "Region",
                                  "County" = "County"),
                   selected = "Region",
                   width = '90%'
                 ),
                 conditionalPanel(
                   #    Select Region
                   condition = "input.dataset == 'Region'",
                   selectInput("region", "Choose a Region:",
                               MSA_raw$MSA[1:(nrow(MSA_raw)-3)],
                               selected = "Texas")
                 ), # end conditional panel
                 conditionalPanel(
                   #    Select County
                   condition = "input.dataset == 'County'",
                   selectInput(
                     "county",
                     label = "Choose a County:",
                     paste0(County_pop$County, ": ", County_pop$Cases),
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
                checkboxInput(
                  inputId = "logscale",
                  label = strong("Log Scaling"),
                  value = FALSE
                ),
                numericInput(
                  "recover_days",
                  label = strong("Days to Recover"),
                  step = 1,
                  value = 9
                )
              ),
              # end wellPanel Control plot options
                  wellPanel(
                    # Modeling parameters
                    radioButtons(
                      "modeling",
                      label = h4("Fitting"),
                      choices = list(
                        "Exponential" = "do fit",
                        "Logistic" = "logistic",
                        "Piecewise" = "piecewise",
                        #"Worldwide (0.13)" = "standard",
                        "User entry" = "user"
                      ),
                      selected = "piecewise"
                    ),
      tabsetPanel(id = "Params_tabs",  type="pills",        
         tabPanel( ##########   Piecewise Tab
                   "Piece",
                   fluid = TRUE,
                   value = "Piecewise",
                    splitLayout(
                      cellWidths = c("34%", "38%", "28%"),
                      numericInput(
                        "min_length",
                        label = h5("MinLen"),
                        step = 1,
                        value = 8,
                        min = 3,
                        max = 100
                      ),
                      numericInput(
                        "min_rsqr",
                        label = h5("Rsqr"),
                        step = 0.01,
                        value = 0.99,
                        min=0.8,
                        max=0.999
                      ),
                      numericInput(
                        "pct_chg",
                        label = h5("%chg"),
                        step = 1,
                        value = 5,
                        min = 0,
                        max = 100
                      )
                    )
         ), # end tab panel Piecewise
         tabPanel( ##########   User entry
                   "User",
                   fluid = TRUE,
                   value = "User_entry",
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

         ) # end tab panel User Entry
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
                          value = FALSE
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
                        "Logistic" = "death_logistic",
                        "Piecewise" = "piecewise"
                      ),
                      selected = "piecewise"
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
                              value = 4.1
                          ),
                          numericInput(
                              "An_DeathLag",
                              label = h5("Days to Death"),
                              step = 1.0,
                              value = 18.00
                          )
                        ),
                    HTML("<hr>")
                    
                  ), # End Deaths Plot controls
                  # end conditional panel
                  ),# end wellPanel
                  conditionalPanel( # Testing Plot controls
                    #    Testing Tab
                    condition = "input.An_tabs == 'Tests'",
                      wellPanel(
                        checkboxInput(
                          inputId = "Tests_logscale",
                          label = strong("Log Scaling"),
                          value = FALSE
                        ),
                        checkboxInput(
                          inputId = "Tests_Percapita",
                          label = strong("Per 100,000"),
                          value = TRUE
                        ),
                        checkboxInput(
                          inputId = "Tests_New",
                          label = strong("Daily Numbers"),
                          value = FALSE
                        ),
              checkboxGroupInput("test_display", 
                                 label = h3("Display"), 
                                 choices = list("Cases" = "Cases", 
                                                "% Positive" = "Pct Pos"),
                                 selected = NULL),


                        ), # end well panel
                    
                  ), # End Tests Plot controls
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
                          label = strong("De-Spike?"),
                          value = FALSE
                        ),
                      ),
                      wellPanel(
                    h4("Choose the Y-Axis"),
                    checkboxInput(
                      "ind_avg",
                      label = "Running Average",
                      value = TRUE
                    ),
                    checkboxInput(
                      inputId = "ind_percap",
                      label = "per 100,000",
                      value = TRUE
                    ),
                    HTML("<hr>"),
                        radioButtons(
                          "slopetype",
                          label = NULL,
                          choices = list(
                            "New Cases" = "new_cases",
                            "Active Cases" = "active_cases",
                            "Percent change" = "pct_chg",
                            "Doubling Time" = "doubling"
                          ),
                      selected = "new_cases"
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
                        "Active Cases" = "active_cases",
                        "Deaths" = "Deaths",
                        "New Deaths" = "new_deaths",
                        "Deaths per Case" = "deaths_percase",
                        "Percent change" = "pct_chg",
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
                        "Active Cases" = "active_cases",
                        "Deaths" = "Deaths",
                        "New Deaths" = "new_deaths",
                        "Deaths per Case" = "deaths_percase",
                        "Percent change" = "pct_chg",
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
    ##########   MetroRegions Tab
    tabPanel( "Regions", fluid = TRUE, value = "RegionsTab",
        HTML("<hr>"),
        fluidPage(#-------------------- Regions
            column( 9, # Plot           
              plotOutput("plot_Regions",
                    height = "700px"),
              #h4("Details on displayed data"),
              #htmlOutput("counties_details")
              gt::gt_output("regions_details")
            ), # end of column Plot
            #-------------------- Controls
            column(3, # Controls
                  wellPanel( 
                    attribute_select_UI("Regions", "Choose the Y-Axis")
                  ), # end y-axis panel
                  wellPanel( 
                    h4("Select Regions:"),
                    uiOutput("RegionSelect")
#                    selectInput('Regions_selector', 
#                                label=NULL, 
#                                #choices=MSA_raw$MSA[2:nrow(MSA_raw)],
#                                choices=choices,
#                                multiple=TRUE, 
#                                selectize=TRUE)
                    
                  ), # end highlight panel
                  wellPanel( # Misc controls
                    checkboxInput(
                      inputId = "Regions_log",
                      label = strong("Log Scale"),
                      value = TRUE
                    )
                  ) # end Misc controls
               ) # end column control
         ) # end fluid page
     ), # end tabPanel Regions
    ##########   Prisons Tab
    tabPanel( "Prisons", fluid = TRUE, value = "PrisonsTab",
        HTML("<hr>"),
        fluidPage(#-------------------- Counties
            column( 9, # Plot           
              plotOutput("plot_prisons",
                    height = "700px"),
              gt::gt_output("prisons_details")
            ), # end of column Plot
            #-------------------- Controls
            column(3, # Controls
                  wellPanel( 
                    h4("Choose the Y-Axis"),
                    checkboxInput(
                      "prisons_avg",
                      label = "Running Average",
                      value = TRUE
                    ),
                    #HTML("<hr>"),
                    checkboxInput(
                      inputId = "prisons_percap",
                      label = "percent population",
                      value = TRUE
                    ),
                    HTML("<hr>"),
                    radioButtons(
                      "prisons_y_axis",
                      label = NULL,
                      choices = list(
                        "Cases" = "Cases",
                        "New Cases" = "new_cases",
                        "Percent change" = "pct_chg",
                        "Doubling Time" = "doubling"
                      ),
                      selected = "Cases"
                    ) 
                  ), # end y-axis panel
                  wellPanel( 
                    h4("Highlight Based On:"),
                    checkboxInput(
                      "prisons_select_avg",
                      label = "Running Average",
                      value = TRUE
                    ),
                    #HTML("<hr>"),
                    checkboxInput(
                      inputId = "prisons_select_percap",
                      label = "percent population",
                      value = TRUE
                    ),
                    HTML("<hr>"),
                    radioButtons(
                      "prisons_selector",
                      label = NULL,
                      choices = list(
                        "Cases" = "Cases",
                        "New Cases" = "new_cases",
                        "Percent change" = "pct_chg",
                        "Doubling Time" = "doubling"
                      ),
                      selected = "new_cases"
                    ) 
                  ), # end highlight panel
                  wellPanel( # Misc controls
                    checkboxInput(
                      inputId = "prison_log",
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
               
                    h4("Display Variable"),
                    checkboxInput(
                      "map_avg",
                      label = "Average last 5 days",
                      value = TRUE
                    ),
                    checkboxInput(
                      inputId = "map_percap",
                      label = "per 100,000",
                      value = TRUE
                    ),
                    HTML("<hr>"),
                    radioButtons(
                      "map_color",
                      label = NULL,
                      choices = list(
                        "Cases" = "Cases",
                        "New Cases" = "new_cases",
                        "Active Cases" = "active_cases",
                        "Deaths" = "Deaths",
                        "New Deaths" = "new_deaths",
                        "Deaths per Case" = "deaths_percase",
                        "Percent change" = "pct_chg",
                        "New Tests" = "new_tests",
                        "Test Pos Pct" = "Pct_pos",
                        "Doubling Time" = "doubling"
                      ),
                      selected = "Cases"
                    ), 
               
                    HTML("<hr>"),
                    checkboxInput(
                      "map_animate",
                      label = "Animate by week",
                      value = FALSE
                    ),
                    sliderInput("map_anim_date",
                                "Date",
                                min=as.Date("2020-03-11"),
                                max=Animate_last,
                                value=as.Date("2020-03-11"),
                                step=7,
                                animate=TRUE
                                ),
                    tags$div(class = "inline", 
                             numericInput(inputId = "min_case", 
                                          step = 10,
                                          value = 30,
                                          min=0,
                                          max=900,
                                          label = "Min Cases:")
                    ),
                    checkboxInput(
                      "map_prisons",
                      label = "Counties with >1% prison pop",
                      value = FALSE
                    ),
                    checkboxInput(
                      "map_meat_packers",
                      label = "Counties with meat packing",
                      value = FALSE
                    ),
                    checkboxInput(
                      "map_top5",
                      label = "Worst 5 Counties",
                      value = TRUE
                    ),
                   htmlOutput("map_details")
               ) # end column control
         ) # end fluid page
                   
     ), # end tabPanel Map
    ##########   Harris County Tab
     tabPanel( "Harris Cty", fluid = TRUE, value = "Harris",
         HTML("<hr>"),
          fluidPage(
            sidebarLayout(
              sidebarPanel(
                width=3,            
                  h4("Domain"),
                  radioButtons(
                    inputId = "har_school",
                    label = NULL,
                    inline=TRUE,
                    choices = list(
                      "Zip"= "Zip",
                      "School"="District"
                    ),
                    selected = "Zip"
                  ),
                  h4("Map Variable"),
                  checkboxInput(
                    inputId = "har_percap",
                    label = "per 1,000",
                    value = TRUE
                  ),
                  HTML("<hr>"),
                  radioButtons(
                    "har_color",
                    label = NULL,
                    choices = list(
                      "Cases" = "Cases",
                      "New Cases" = "new_cases",
                      "Active Cases" = "active_cases",
                      "Percent change" = "pct_chg",
                      "Doubling Time" = "Doubling"
                    ),
                    selected = "Cases"
                  ) ,
                   tags$div(class = "inline",
                            numericInput(inputId = "har_extreme_value",
                                         step = 1,
                                         value = 5,
                                         min=1,
                                         max=100,
                                         label = "Extremes (%):")
                   ),
                  radioButtons(
                    inputId = "har_extremes",
                    label = NULL,
                    inline=TRUE,
                    choices = list(
                      "Extremes"= "Extremes",
                      "All"="All"
                    ),
                    selected = "All"
                  ),
                   HTML("<hr>"),
                   sliderInput("har_anim_date",
                               "Date",
                               min=as.Date("2020-05-24"),
                               max=Harris_last,
                               value=Harris_last,
                               step=7,
                               animate=
                                 animationOptions(interval=3000)
                               ),
                   tags$div(class = "inline",
                            numericInput(inputId = "min_har_case",
                                         step = 10,
                                         value = 30,
                                         min=0,
                                         max=900,
                                         label = "Min Cases:")
                   ),
                  # checkboxInput(
                  #   "har_top5",
                  #   label = "Worst 5",
                  #   value = TRUE
                  # ),
                   HTML("<hr>"),
              selectInput("har_cross", label = h4("Crossplot"),
                          # choices = list("Med Income" = 1,
                          #                "Median Age" = 2,
                          #                "Blueness" = 3),
                          choices = har_choices,
                          selected = "Med_Income"),
                  htmlOutput("har_details")
              ), # end sidebarPanel
              mainPanel(width=9, 
                        fluidRow(
                          column(12,
                            #HTML("<center><h4>HarrisTitle</h4></center>"),
                            htmlOutput("HarrisTitle")
                        )),
                        #fluidRow("har_title"),
                        fluidRow( # Row 1
                          column( # top left
                            6,
                            leafletOutput("HarrisMap",
                                          height = "400px")
                            
                          ), # end left column, top row
                          column( # top right
                            6,
                            plotlyOutput("har_history") 
                                     ###  click = "history_click"),
                            
                          ) # end right column, top row
                        ), # end Row 1
                        fluidRow( # Row 2
                          column( # bottom left
                            6,
                            plotlyOutput("har_xplot") 
                            #plotOutput("har_xplot") 
                            ), # end bottom left column
                          column( # bottom right
                            6,
                            plotlyOutput("har_histogram") 
                            ) # end bottom right
                        ) # end Row 2
                        ), # end mainPanel
              position="right",
              fluid=FALSE
            ) # end sidebarLayout
          ) # end fluid page
                    
      ), # end tabPanel Harris
    ##########   Documentation Tab
         tabPanel("Documentation", fluid=TRUE, value="DocumentationTab",
                  withMathJax(includeMarkdown("Documentation.Rmd")),
                  HTML("<hr>")

          )  # end tabPanel Documentation
        )  # end tabset 
    ) # end basic page

# Define server logic 
server <- function(input, output, session) {
  #hideTab(inputId = "An_tabs", target="Indicators")   
  #hideTab(inputId = "An_tabs", target="Tests")   
  #hideTab(inputId = "An_tabs", target="Something")   
  print("---- start server")
#   Global variables are
#   DF = original data
#   PopLabel = list(Region, Population, Label)
#   subdata = tibble of data subsetted 
#   death_fit, case_fit, case_est_fit = Day, Date, 'value',
  #                                     upper_conf, lower_conf
#   death_params, case_params, case_est_params =
  #                     named arrays of m, b, Rsq or
  #                                     r, K, xmid
  # County_tests - Test data by county
       
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
      PopLabel <<- tibble(Region=in_area, 
                          Population=MSA_raw$Population[MSA_raw$MSA==in_area], 
                          Label=in_area)
      target <- unlist(MSA_raw$Counties[MSA_raw$MSA==in_area])
      subdata <<- MSAs %>% ungroup() %>% 
        filter(MSA == in_area) %>% 
        mutate(Days=as.integer(Date-ymd("2020-03-10"))) %>% 
        mutate(actual_deaths=new_deaths) %>% #########   temporary fix
        filter(between(Date, 
                       ymd(in_dateRange[1]), 
                       ymd(in_dateRange[2])))
      return()
      
    } else { # select a county
      #   Is there any data?
      county <- str_extract(in_area, "[A-Za-z ]+")
      if (! county %in% County_calc$County) {
        showNotification(paste("No reported cases in", county),
                         duration=2)
        return()
      }
      
      PopLabel <<- County_pop %>% filter(County==county) %>% 
                     mutate(Label=paste(county, "County"))
      subdata <<- County_calc %>% filter(County==county) %>%  ungroup() %>% 
                         #mutate(actual_deaths=Deaths-lag(Deaths, 1, 0)) %>% 
                         mutate(Days=as.integer(Date-ymd("2020-03-10"))) %>% 
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
   #data <- subdata %>% 
   #  filter((!!sym(indep))>0) %>% 
   #  filter(!is.na((!!sym(indep)))) %>% 
   #  mutate(actual=!!as.name(indep)-lag(!!as.name(indep), 1, 0)) %>% 
   #  filter(actual>0) %>% 
   #  mutate(!!indep:=cumsum(actual))
   
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
  #-----------Fit a piecewise exponential model ------
  #---------------------------------------------------    
  
 fit_piecewise <- function(indep="Cases", # independent variable
                           projection=10,
                           min_length=8,
                           min_rsqr=0.99,
                           pct_chg=0.05,
                           calc_conf=TRUE) {
   print(paste(":::::::  fit_piecewise", indep))
   if (is.null(min_length)) {min_length <- 8}
   if (is.null(min_rsqr)) {min_rsqr <- 0.99}
   if (is.null(pct_chg)) {pct_chg <- 0.05}
   #    Add NA's for missing dates
   data <- subdata %>% 
     complete(nesting(Days=(first(Days):last(Days)),
                      Date=seq.Date(first(Date), last(Date), by="day") )) %>% 
     mutate(Deaths=na_if(Deaths, 0)) %>% 
     mutate(Cases=na_if(Cases, 0))
   
   #    Too few cases to do a fit
   if ((sum(!is.na(data[,indep][[1]]))<min_length) ||
       (nrow(unique(data[,indep]))<3) ||
       (nrow(unique(data[(nrow(data)-min_length):nrow(data),indep]))<3)) {
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

  fit_segment <- function(data, start, min_length, min_rsqr, indep) { # fit one segment and return when done
    
    oldRsqr <- 0
    for (i in ((start-min_length):0)){ # count backwards
      my_data <- data %>% select(x=Days, y=!!indep)
      #print(tail(my_data))
      my_data <- my_data[i:start,]
      
      model <- lm(log10(y) ~ x , data=my_data)
      
      m <- model[["coefficients"]][["x"]]
      b <- model[["coefficients"]][["(Intercept)"]]
      Rsqr <- summary(model)$adj.r.squared
      std_dev <- sigma(model)
      double <- signif(log10(2)/m,3)
      
      worsening <- ((Rsqr<min_rsqr) || 
                    (max(10**model[["fitted.values"]]-my_data$y, na.rm=TRUE)
                     > max(pct_chg*my_data$y, na.rm=TRUE))) & (start-i>min_length)
      if (worsening){ # Rsqr too small or jump > 5%
        return(list(stop=i,
                    Rsqr=oldRsqr,
                    m=old_m,
                    b=old_b,
                    double=old_double,
                    model=old_model))
      }
      oldRsqr <-  Rsqr
      old_m <- m
      old_b <- b
      old_double <- double
      old_model <- model
    }
    return(list(stop=i,
                Rsqr=Rsqr,
                m=m,
                b=b,
                double=double,
                model=model))
  } # end fit_segment function
  
    #     initialize results tibble
    results <- tibble(start=numeric(0),
                      stop=numeric(0), 
                      m=numeric(0), 
                      b=numeric(0), 
                      double=numeric(0), 
                      Rsqr=numeric(0),
                      model=list(),
                      tidiness=list())
      fits <- tibble(Days=numeric(0),
                     #Date=dateseq,
                     !!indep:=numeric(0),
                     lower_conf=numeric(0),
                     upper_conf=numeric(0))
      params <- list(m=numeric(0), 
                     b=numeric(0), 
                     Rsqr=numeric(0),
                     mid_day=numeric(0),
                     yvalue=numeric(0)
                     )
      ends <- tibble(Date=Date(), yvalue=numeric())
    
    start <- nrow(data)
    
    ########################################
     # browser()
    ########################################
    
    while ((start >=min_length) && 
           (nrow(unique(data[(start-min_length+1):start,indep]))>=3)) {
      
     #if ((sum(!is.na(my_data[,indep][[1]]))<min_length) ||
     #    (nrow(unique(my_data[,indep]))<min_length)) { # not enough data in segment
     #  
     #}
      print(paste("--1--",(start-min_length+1):start ))
      answers <- fit_segment(data, start, min_length, min_rsqr, indep)
      
      #  Estimate confidence bands 
      model <- answers[["model"]]
      stop <- answers[["stop"]]
      DayFrame <- data.frame(x=data$Days[stop:start])
      if (start == (nrow(data))) {
        lastday <- last(data$Days)
        DayFrame <- data.frame(x=c(data$Days[stop:start],(lastday+1):(lastday+10)))
      }
      pred.int <- cbind(DayFrame, 
                        predict(model, 
                                newdata = DayFrame, 
                                interval = "confidence", 
                                level = 0.975))
      mid_day <- first(data$Date) + as.integer(mean(DayFrame$x)) - first(data$Days)
      mid_y <- 10**pred.int$fit[as.integer(nrow(DayFrame)/2)]
      Date_start <- first(data$Date) - first(data$Days)
      endDate <- c(Date_start+first(DayFrame$x), 
                   Date_start+last(DayFrame$x))
      endy <- c(first(10**pred.int$fit), last(10**pred.int$fit))
      params <- list(m=c(params[["m"]] ,answers[["m"]]), 
                     b=c(params[["b"]], answers[["b"]]), 
                     Rsqr=c(params[["Rsqr"]], answers[["Rsqr"]]), 
                     mid_date=c(params[["mid_date"]], mid_day), 
                     yvalue=c(params[["yvalue"]], mid_y))
      ends <- ends %>% 
        add_row(Date=endDate[1], yvalue=endy[1]) %>% 
        add_row(Date=endDate[2], yvalue=endy[2]) 
      fits <-  fits %>% 
        add_row(Days=DayFrame$x,
                !!indep:=10**pred.int$fit,
                lower_conf=10**pred.int$lwr,
                upper_conf=10**pred.int$upr)
      results <- results %>% 
        add_row(start=start, stop=answers[["stop"]],
                m=answers[["m"]], b=answers[["b"]], 
                double=answers[["double"]],
                Rsqr=answers[["Rsqr"]], model=list(answers[["model"]]),
                tidiness=list(tidy(answers[["model"]])))
      start <- answers[["stop"]]-1
      
    } # end while loop
    fits <- arrange(fits, Days) # fix sorting
    Day_date <- data %>% 
      select(Days, Date)
    print(paste(last(Day_date$Days), last(Day_date$Date) ))
    Day_date <- rbind(Day_date, 
                      tibble(Days=last(Day_date$Days)+(1:10), 
                             Date=last(Day_date$Date)+(1:10)))
    fits <- left_join(fits, Day_date, by="Days")
    
    ########################################
     # browser()
    ########################################
    
    params$mid_date <- as_date(params$mid_date)
    ends$Date <- as_date(ends$Date)
    params$Ends <- ends
    print("----------------   fits here")
    print(head(fits))
    if (indep=="Cases") {
      case_fit <<- fits
      case_params <<- params
    } else if (indep=="Deaths") {
      death_fit <<- fits
      death_params <<- params
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
    print(coeffs)
    print(summary(logistic_model))
    
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
                                             "piecewise",
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
    
    badfit <- FALSE
    
    if (in_modeling == "logistic") { 
      if (is.null(case_params[["r"]]) || is.na(case_params[["r"]])){# if nonlinear fit failed
        showNotification("Failure to fit data")
        badfit <- TRUE
        #return(NULL)
      }
      EqText <- paste0("Fit is Cases = ",
                       signif(case_params[["K"]],3), "/( 1 + e^(",
                       signif(case_params[["r"]]*case_params[["xmid"]],3)," + ", 
                       signif(case_params[["r"]],3),"*Days))")
    } else if (in_modeling=="piecewise") {
      if ((is.null(last(case_params[["m"]]))) || (is.na(last(case_params[["m"]])))){# if fit failed
        showNotification("Failure to fit data")
        badfit <- TRUE
        #return(NULL)
      }
      print(paste("---EqText 1399", case_params[["m"]]))
      EqText <- paste0("Last fit is log(Cases) = ",
                       signif(last(case_params[["m"]]),3),"*Days + ",
                       signif(last(case_params[["b"]]),3))
    } else {
      if ((is.null(case_params[["m"]])) || (is.na(case_params[["m"]]))){# if fit failed
        showNotification("Failure to fit data")
        badfit <- TRUE
        #return(NULL)
      }
      EqText <- paste0("Fit is log(Cases) = ",
                       signif(case_params[["m"]],3),"*Days + ",
                       signif(case_params[["b"]],3))
    }
    
    #   Scaling for secondary Testing axis
    xform <- signif(max(TestingData$Total)/(2*max(subdata$Cases)),3)
    LastDate <- subdata[nrow(subdata),]$Date
 
    #  Basic canvas     
    p <- subdata %>% 
          ggplot(aes(x=Date, y=Cases))
    
    if (!badfit) {
    #------------------
    #  Error bars
    #------------------
      if (in_modeling=="do fit" || in_modeling=="logistic" || in_modeling=="piecewise") {
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
    #  if piecewise fit, show segment ends and doubling time
    #------------------
    if (in_modeling=="piecewise") {
      # Label segment with doubling time
      double <- signif(log10(2)/case_params$m,3)
      print(case_params)
      p <- p + 
           geom_text(data=tibble(x=case_params$mid_date,
                                y=case_params$yvalue,
                                lab=double),
                    color="blue",
                    aes(x=x, y=y, label=lab), 
                    hjust=1, vjust=0
                    ) 
     # highlight segment ends
      
      p <-  p + 
        geom_point(data=case_params[["Ends"]] ,
                   aes(x=Date, y=yvalue),
                   color="cyan")
      
    }
  }
    #------------------
    # Points
    #------------------
    p <- p + geom_point(aes(color="data"), size=2) 
      
    if (badfit) {return(p)}
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
      } else if (in_modeling=="piecewise") {
        print("---- case params -----")
        print(case_params[["Rsqr"]])
        output$data_details <- data_details(subdata,
                                             "Cases",
                                             EqText,
                                             first(case_params[["m"]]),
                                             first(case_params[["b"]]),
                                             first(case_params[["Rsqr"]]))
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
   
   return( tibble( Date=death_fit$Date - in_An_DeathLag,
                   Days=death_fit$Days - in_An_DeathLag,
                   est_cases=100*death_fit$Deaths/in_An_CFR
   ))
   
   # print("---- backest 1 -----")
   # if (is.null(death_params[["K"]])) { # did an exponential fit
   # print("---- backest 3 -----")
   #   Cases <- 10**(death_params[["m"]]*dayseq+death_params[["b"]])
   #   Cases <- 100 * Cases / in_An_CFR
   # } else { # logistic fit
   # print("---- backest 2 -----")
   #   ################################
   #   #browser()
   #   ################################
   #   Co <- death_params[["r"]]*death_params[["xmid"]]
   #   Cases <- death_params[["K"]]/(1  + exp(Co - death_params[["r"]]*dayseq))
   #   Cases <- 100 * Cases / in_An_CFR
   # }
  
   # return( tibble(Date=dateseq,
   #               Days=dayseq,
   #               est_cases=Cases))
  
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
    LastDate <- subdata[nrow(subdata),]$Date
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
                       signif(death_params[["K"]],3), "/( 1 + e^(",
                       signif(death_params[["r"]]*death_params[["xmid"]],3)," + ", 
                       signif(death_params[["r"]],3),"*Days))")
    } else if (in_death_modeling=="piecewise") {
      if ((is.null(last(death_params[["m"]]))) || (is.na(last(death_params[["m"]])))){# if fit failed
        showNotification("Failure to fit data")
        badfit <- TRUE
        return(NULL)
      }
      print(paste("---EqText 1399", death_params[["m"]]))
      EqText <- paste0("Last fit is log(Deaths) = ",
                       signif(last(death_params[["m"]]),3),"*Days + ",
                       signif(last(death_params[["b"]]),3))
      print(EqText)
    }
    else {
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
    #if (!in_Deaths_logscale) {
    #    p <- p + geom_col(alpha = 2/3) # +
    #         #geom_label(aes(label=Deaths), 
    #         #               stat='identity',
    #         #               size = 3) 
    # } else {
        
        p <- p + geom_point(aes(color="data"), size=2) 
    # } 
    print("---- build_death_plot 4 ------")
      p <- p + #geom_label(data=death_fit[(nrow(death_fit)-9):nrow(death_fit),],
             #     aes(label=as.integer(Deaths+.5)),
             #     hjust=1, vjust=0) +

          theme(text = element_text(size=20)) +
          labs(title=paste0("COVID-19 Deaths in ",PopLabel$Label), 
               subtitle=paste0(" as of ", last(subdata$Date)))
      
    #------------------
    #  if piecewise fit, show segment ends and doubling time
    #------------------
    if (in_death_modeling=="piecewise") {
    print("---- build_death_plot 7 ------")
      # Label segment with doubling time
      double <- signif(log10(2)/death_params$m,3)
      print(death_params)
      p <- p + 
           geom_text(data=tibble(x=death_params$mid_date,
                                y=death_params$yvalue,
                                lab=double),
                    color="blue",
                    aes(x=x, y=y, label=lab), 
                    hjust=1, vjust=0
                    ) 
     # highlight segment ends
      
      p <-  p + 
        geom_point(data=death_params[["Ends"]] ,
                   aes(x=Date, y=yvalue),
                   color="cyan")
      
    }
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
      p <- p + geom_point(data=subdata, aes(y=Cases, x=Date, color="data"), 
                          size=2, shape=4)# +
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
      } else if (in_death_modeling=="piecewise") {
        print("----8------")
        output$death_details <- data_details(subdata,
                                             "Deaths",
                                             EqText,
                                             first(death_params[["m"]]),
                                             first(death_params[["b"]]),
                                             first(death_params[["Rsqr"]]))
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
      str3 <- paste("           Doubling Time =", signif(log10(2)/m,3), "days",
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
      str3 <- paste("Asymptote for fit is",signif(K,3),
                    "<br> ", EqText)
      str2 <- paste("Growth rate =", signif(r,3), 
                    "&nbsp; &nbsp; &nbsp; &nbsp; Date of inflection is",
                    data$Date[1]+as.integer(xmid+0.5))
      HTML(paste(str1, str3, str2, sep = '<br/>'))
    })
  }
  
  #---------------------------------------------------    
  #------------------- Build Indicator Plot ----------
  #---------------------------------------------------    
  
  build_slope_plot <- function(
                                in_window,
                                in_slopetype,
                                in_ind_percap,
                                in_ind_avg,
                                in_smooth,
                                in_smthlength=3
                                ){
    print(":::::::  build_slope_plot")
    
    # what is the Y axis
    
    print(in_slopetype)
    my_indicator <- in_slopetype
    if (in_ind_percap & 
        calc_controls$percap[calc_controls$base==in_slopetype]) {
      my_indicator <- paste0(my_indicator, "_percap")
    }
    if (in_ind_avg &
        calc_controls$avg[calc_controls$base==in_slopetype]) {
      my_indicator <- paste0("avg_", my_indicator)
    }
    print(my_indicator)
    
    foo <- subdata
    
    print("-------- slope  5.75 --------------")
    #---------------   smoothing (de-spiking)
    if (in_smooth) {
      foo$med <- zoo::rollmedian(foo$new_cases, in_smthlength, 
                    fill=c(0, NA, last(foo$new_cases)))  
      
      foo <- foo %>% 
        mutate(new_cases=if_else(((new_cases-med)>(2*med)),med,new_cases)) #%>% 
        #select(-med)
    }
    halfwidth <- as.integer(in_window/2)
    Population <- PopLabel$Population
    print("-------- slope  1 --------------")
    foo <- foo %>% 
      arrange(Cases) %>% 
      mutate(pct_chg=100*new_cases/lag(Cases, default=Cases[1])) %>% 
      mutate(avg_pct_chg=zoo::rollmean(pct_chg, in_window, 
                                  fill=c(0, NA, last(pct_chg)))) %>% 
      mutate(active_cases=Cases-lag(Cases, n=9, default=0)) %>%
      mutate(avg_active_cases=zoo::rollmean(active_cases, in_window, 
                              fill=c(0, NA, last(active_cases)))) %>% 
      mutate(avg_new_cases=zoo::rollmean(new_cases, in_window, 
                              fill=c(0, NA, last(new_cases)))) %>% 
      mutate(avg_doubling=zoo::rollmean(doubling, in_window, 
                              fill=c(0, NA, last(doubling)))) %>% 
      mutate(new_cases_percap=1.e5*new_cases/Population) %>% 
      mutate(avg_new_cases_percap=1.e5*avg_new_cases/Population) %>% 
      mutate(active_cases_percap=1.e5*active_cases/Population) %>% 
      mutate(avg_active_cases_percap=1.e5*avg_active_cases/Population) #%>% 
    print("-------- slope  2 --------------")
    
    foo <- foo %>% 
      mutate(m = case_when(
        my_indicator=="pct_chg" ~ pct_chg,
        my_indicator=="avg_pct_chg" ~ avg_pct_chg,
        my_indicator=="avg_doubling" ~ avg_doubling,
        my_indicator=="new_cases" ~ new_cases,
        my_indicator=="avg_new_cases" ~ avg_new_cases,
        my_indicator=="new_cases_percap" ~ new_cases_percap,
        my_indicator=="avg_new_cases_percap" ~ avg_new_cases_percap,
        my_indicator=="active_cases" ~ active_cases,
        my_indicator=="avg_active_cases" ~ avg_active_cases,
        my_indicator=="active_cases_percap" ~ active_cases_percap,
        my_indicator=="avg_active_cases_percap" ~ avg_active_cases_percap,
        TRUE ~ 0
      ))
    print("-------- slope  3 --------------")
    
    my_title <- list("cases"="Slope of Cum Case Count in ",
                  "pct_chg"="Percent Change of Cases in ",
                  "doubling"="Doubling Time for ",
                  "avg_doubling"="Avg Doubling Time for ",
                  "avg_pct_chg"="Avg Pct Change in Cases in ",
                  "active_cases"="Active Cases in ",
                  "avg_active_cases"="Avg Active Cases in ",
                  "active_cases_percap"="Active Cases per 100,000 in ",
                  "avg_active_cases_percap"="Avg Active Cases per 100,000 in ",
                  "new_cases"="New Cases in ",
                  "avg_new_cases"="Avg New Cases in ",
                  "new_cases_percap"="New Cases per 100,000 in ",
                  "avg_new_cases_percap"="Avg New Cases per 100,000 in ")
    my_ylab <- list("cases"="Slope: Change in cum num cases/ num days ",
                "pct_chg"="Daily percent change",
                "doubling"="Doubling Time in Days",
                "avg_doubling"="Doubling Time in Days",
                "avg_pct_chg"="Avg Pct Change in Cases",
                "active_cases"="Active Cases",
                "avg_active_cases"="Avg Active Cases",
                "active_cases_percap"="Active Cases per 100,000",
                "avg_active_cases_percap"="Avg Active Cases per 100,000",
                "new_cases"="New Cases",
                "avg_new_cases"="Avg New Cases",
                "new_cases_percap"="New Cases per 100,000",
                "avg_new_cases_percap"="Avg New Cases per 100,000")
    
    print("-------- slope  4 --------------")
    if (my_indicator=="cases") {
      foo <- foo %>% 
        mutate(log_cases=Cases)
    } else {
      foo <- foo %>% 
        mutate(log_cases=log10(Cases))
    }
    
    print("-------- slope  5 --------------")
    if (my_indicator=="doubling" || my_indicator=="cases") { # calc slope
    print("-------- slope  5.1 --------------")
      foo <- foo %>%
        mutate(
          model = slider::slide(
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
   # if (!grepl("percent", my_indicator)) {
   #   foo <- foo %>% filter(m>0.0)
   # }
    #   calculate doubling time
    if (my_indicator=="doubling") {
      foo <- foo %>% 
        mutate(m=signif(log10(2)/m,3),
               sd=signif(log10(2)/(m-sd),3)) %>% 
        mutate(m=replace(m, m>200, NA)) %>%  
        mutate(m=replace(m, m< -200, NA)) 
       # filter(m<200)
    }
    
    print("-------- slope  6 --------------")
    print(tail(foo))
    #-------------------------   plot
    
    p <- foo %>% 
      ggplot(aes(x=Date, y=m)) +
      geom_point() +
      theme(text = element_text(size=20)) +
      geom_smooth(span=0.2) +
      labs(title=paste0(my_title[[my_indicator]]
                        ,PopLabel$Label),
           y=my_ylab[[my_indicator]])
    
    if (my_indicator=="cases") {
      p <- p +
      geom_errorbar(aes(ymax=m+sd, ymin=m-sd)) +
        labs(subtitle=paste0("Fit over ",in_window," days"))
    }
  #------------------- Build Data Details Panel ------
  output$indicator_details <- renderUI({
      str1 <- paste("Most recent value, on",last(foo$Date),
                    "was<b>", formatC(last(foo$m),
                                      format="d", big.mark=","),"</b>",
                    my_ylab[[my_indicator]])
      
     # if (has_name(PopLabel,"Region")) {
        HTML(paste(str1, sep = '<br/>'))
     # } else { # Counties - find ranking
     #   
     #   
     # }
    })
    
  return(p)
  }
  
  #---------------------------------------------------    
  #------------------- Build Tests Plot --------------
  #---------------------------------------------------    
  
  build_tests_plot <- function(
                                in_tests_logscale,
                                in_tests_Percapita,
                                in_tests_New,
                                in_test_display
                                ){
    print(":::::::  build_tests_plot")
    
    print(in_test_display)
    #   Make testing it easier
    test_display <- str_c(in_test_display, collapse=",")
    print(test_display)
    
    data <- subdata %>% 
      filter(Tests>0) %>% 
      mutate(ifelse(Pct_pos>50, NA, Pct_pos)) %>% 
      mutate(ifelse(Pct_pos<0, NA, Pct_pos)) %>% 
      filter(!is.na(Tests)) 
    
    if (in_tests_New) { # Daily number space
      if (in_tests_Percapita) { # perCapita scaling
        p <- data %>% 
          ggplot(aes(x=Date, y=new_tests_percap, color="tests")) +
          geom_point() +
          geom_smooth() 
        
        ymin <- min(data$new_tests_percap, na.rm=TRUE)
        ymax <- max(data$new_tests_percap, na.rm=TRUE)
        ylabs <- "Tests per Day per 100,000"
          
        if(grepl("Cases", test_display)) {
          p <- p + 
            geom_point(aes(y=new_cases_percap), shape=23) +
            geom_smooth(aes(y=new_cases_percap, color="cases"))
            
          ymin <- min(ymin, data$new_cases_percap, na.rm=TRUE)
          ymax <- max(ymax, data$new_cases_percap, na.rm=TRUE)
        }
        
      } else { # Absolute number scaling
        p <- data %>% 
          ggplot(aes(x=Date, y=new_tests, color="tests")) +
          geom_point() +
          geom_smooth() 
        
        ymin <- min(data$new_tests, na.rm=TRUE)
        ymax <- max(data$new_tests, na.rm=TRUE)
        ylabs <- "Tests per Day"
        
        #if (!is.null(in_test_display) && 
        #    in_test_display[1]=="Cases") {
        if(grepl("Cases", test_display)) {
          p <- p + 
            geom_point(aes(y=new_cases), shape=23) +
            geom_smooth(aes(y=new_cases, color="cases"))
            
          ymin <- min(ymin, data$new_cases, na.rm=TRUE)
          ymax <- max(ymax, data$new_cases, na.rm=TRUE)
        }
      }
      
    } else { # cumulative number space
      if (in_tests_Percapita) { # perCapita scaling
        p <- data %>% 
          ggplot(aes(x=Date, y=Tests_percap, color="tests")) +
          geom_point() +
          geom_smooth() 
        
        ymin <- min(data$Tests_percap, na.rm=TRUE)
        ymax <- max(data$Tests_percap, na.rm=TRUE)
        ylabs <- "Cumulative Tests per 100,000"
        
        if(grepl("Cases", test_display)) {
            p <- p + 
              geom_point(aes(y=Cases_percap), shape=23) +
              geom_smooth(aes(y=Cases_percap, color="cases"))
            
          ymin <- min(ymin, data$Cases_percap, na.rm=TRUE)
          ymax <- max(ymax, data$Cases_percap, na.rm=TRUE)
        }
        
      } else { # Absolute number scaling
        p <- data %>% 
          ggplot(aes(x=Date, y=Tests, color="tests")) +
          geom_point() +
          geom_smooth() 
        
        ymin <- min(data$Tests, na.rm=TRUE)
        ymax <- max(data$Tests, na.rm=TRUE)
        ylabs <- "Cumulative Tests"
        
        if(grepl("Cases", test_display)) {
            p <- p + 
              geom_point(aes(y=Cases), shape=23) +
              geom_smooth(aes(y=Cases, color="cases"))
          ymin <- min(ymin, data$Cases, na.rm=TRUE)
          ymax <- max(ymax, data$Cases, na.rm=TRUE)
        }
      }
    }
    
    if (in_tests_logscale) {
      min_limit <- max(ymin, 2)
      p <- p + scale_y_continuous(limits=c(min_limit, ymax),
                                  trans="log10",
                                  breaks = logTicks(n = 4), 
                                  minor_breaks = logTicks(n = 40)) 
    }
    
    # --------------  secondary axis and pct positive
    xform <- 50/ymax
    print(paste("xform =", xform))
    if(grepl("Pct Pos", test_display)) {
      if (in_tests_logscale ) {
        min_limit <- 1
        
        p <- p + scale_y_continuous(sec.axis = sec_axis(~.*xform, 
                                                        name = "Test % Positive"),
                                    trans="log10",
                                    breaks = logTicks(n = 4), 
                                    minor_breaks = logTicks(n = 40)) +
        geom_point( aes(y=Pct_pos/xform),
                   size=3, shape=21, 
                   fill="white") +
        geom_smooth(aes(y=Pct_pos/xform, color="pct"), linetype=2)
      } else {
        min_limit <- 0
        p <- p + scale_y_continuous(limits=c(min_limit, ymax),
                                    sec.axis = sec_axis(~.*xform, 
                                                        name = "Test % Positive"),
                                    trans="identity" ) +
        geom_point( aes(y=Pct_pos/xform),
                   size=3, shape=21, 
                   fill="white") +
        geom_smooth(aes(y=Pct_pos/xform, color="pct"), linetype=2)
      }
    }
    
    p <- p +
          theme(text = element_text(size=20)) +
          labs(title=paste0("COVID-19 Tests in ",PopLabel$Label), 
               subtitle=paste0(" as of ", last(subdata$Date)),
               y=ylabs)
    
    p <-  build_legend(p, "Testing",
                       c("Tests", "Cases", "% Pos"), # Labels for legend
                       c("blue", "green", "red"), # Color values
                       c("tests", "cases", "pct"), # Breaks (named lists)
                       c("blue", "green", "white"), # fill values
                       in_tests_logscale
    )
    
    
    
    
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
      "active_cases", TRUE, TRUE,  FALSE, FALSE,
      "deaths_percase", FALSE, FALSE,  TRUE, TRUE,
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
    counties_case <- County_calc %>% 
      filter(Cases>in_case_start) %>%  
      select(-n) %>% 
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
                     "active_cases"="Number of Active Cases",
                     "active_cases_percap"="Active Cases per 100,000",
                     "avg_active_cases"="Avg Active Cases",
                     "avg_active_cases_percap"="Avg Active Cases per 100,000",
                     "Deaths"="Number of Deaths",
                     "Deaths_percap"="Deaths per 100,000",
                     "avg_Deaths"="Avg Deaths",
                     "avg_Deaths_percap"="Avg Deaths per 100,000",
                     "new_deaths"="Number of New Deaths",
                     "new_deaths_percap"="New Deaths per 100,000",
                     "avg_new_deaths"="Avg New Deaths",
                     "avg_new_deaths_percap"="Avg New Deaths per 100,000",
                     "deaths_percase"="Deaths per Case",
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
      if (sorting){# for doubling
        print("c")
        dplyr::filter(df, Mselect<=(unique(Mselect)[min(length(unique(Mselect))-2,5)]))
      } else { # for all but doubling
        print("d")
        dplyr::filter(df, Mselect>=(unique(Mselect)[max(min(length(unique(Mselect))-2,5),1)]))
        }
    }
    #browser()
    
    #foo <- 
    counties_case %>% 
      arrange(Date) %>% 
      group_by(County) %>% 
        mutate(Mselect=last(!!as.name(in_counties_selector))) %>% 
        mutate(end_case=last(!!as.name(in_counties_y_axis)), end_day=max(day)) %>% 
      ungroup() %>% 
      do_sort(sorting) %>% 
      do_filter(sorting) %>% 
      select(-Mselect) -> counties_case_filt
    
    #   Stretch scale
    daylimit <- max(counties_case_filt$day, na.rm = TRUE)*1.1
    
    print("-------------  counties plot 5")
    #   Plot county data
    p <- 
    counties_case %>% 
      ggplot(aes(x=day, y=!!as.name(in_counties_y_axis))) + 
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
        rename(!!sym(in_counties_selector):=2, Total_Cases=3) %>% 
        mutate(label=y_labels[[in_counties_selector]]) %>% 
        arrange(desc(!!as.name(in_counties_selector))) %>% 
        mutate(text=paste0(County,": ", label, " = ", 
                           !!as.name(in_counties_selector),
                           " and Total Cases = ", Total_Cases))
    #print("-------------  counties plot 7")
      
    details %>% select(-label, -text) %>%
      gt::gt() %>%
      gt::tab_header(title="Highlighted Details") %>% 
      gt::cols_label(County=gt::md("**County**"), 
                     !!sym(in_counties_selector):=gt::md(paste0("**",y_labels[[in_counties_selector]],"**")), 
                     Total_Cases=gt::md("**Cases**")) %>% 
      gt::tab_style(style=gt::cell_fill(color="lightcyan"),
                    locations=gt::cells_title())
    
     # HTML(paste(details$text[1:6], collapse = '<br/>'))
      
    })
    
    
    return(p)
    
  }
  #---------------------------------------------------    
  #------------------- Build Regions Plot -----------
  #---------------------------------------------------    
  
  build_Regions_plot <- function(
                                in_y_axis,
                                in_selector,
                                in_case_start,
                                in_log
                                ){
    print(":::::::  build_Regions_plot")
    print(paste(in_y_axis, in_selector, in_case_start, in_log))
    
    # Texas is always selected
    in_selector <- c("Texas", in_selector)
    
    window <- 5
    
    summarylist <- c("Cases", "Deaths", "new_cases", "new_deaths",
                     "active_cases")
    summarylist <- c(summarylist, str_replace(summarylist, "^", "avg_"))
    summarylist <- c(summarylist, "Population")
    
    inputs <- calc_controls$base[calc_controls$percap==TRUE]
    inputs <- c(paste0("avg_", inputs), inputs)
    
    #   Create summary of "All Others"
    all_others <- MSAs %>% 
      filter(!(MSA %in% in_selector)) %>% 
      filter(!(MSA=="Texas")) %>% 
      group_by(Date) %>% 
        summarise_at(summarylist, sum, na.rm = TRUE) %>% 
        mutate_at(inputs, list(percap = ~ . / Population * 1.e5)) %>% 
      ungroup() %>% 
      mutate(MSA="All Others")
    
    chosen <- MSAs[MSAs$MSA %in% in_selector ,]
    
    MSA_case <- bind_rows(chosen, all_others)
    
    print("-------------  Regions plot 2")
    
    
    print("-------------  Regions plot 3")
    y_labels <- list(
                     "Cases"="Number of Cases",
                     "Cases_percap"="Cases per 100,000",
                     "avg_Cases"="Avg Cases",
                     "avg_Cases_percap"="Avg Cases per 100,000",
                     "new_cases"="Number of New Cases",
                     "new_cases_percap"="New Cases per 100,000",
                     "avg_new_cases"="Avg New Cases",
                     "avg_new_cases_percap"="Avg New Cases per 100,000",
                     "active_cases"="Number of Active Cases",
                     "active_cases_percap"="Active Cases per 100,000",
                     "avg_active_cases"="Avg Active Cases",
                     "avg_active_cases_percap"="Avg Active Cases per 100,000",
                     "Deaths"="Number of Deaths",
                     "Deaths_percap"="Deaths per 100,000",
                     "avg_Deaths"="Avg Deaths",
                     "avg_Deaths_percap"="Avg Deaths per 100,000",
                     "new_deaths"="Number of New Deaths",
                     "new_deaths_percap"="New Deaths per 100,000",
                     "avg_new_deaths"="Avg New Deaths",
                     "avg_new_deaths_percap"="Avg New Deaths per 100,000",
                     "deaths_percase"="Deaths per Case",
                     "pct_chg"="Percent Change",
                     "avg_pct_chg"="5-day Avg Percent Change",
                     "doubling"="Doubling Time in Days",
                     "avg_doubling"="Avg Doubling Time in Days"
                     )
    
    print("-------------  Regions plot 4")
    #     Apply selector
    sorting <- grepl("doubling", in_selector)
    print(paste("--->>> sorting = ", sorting))
    
    title_label <- "Greatest"
    if (sorting) {title_label <- "Smallest"}
    
    ##sort_direction <- 2*sorting-1
    
    do_sort <- function(df, sorting) {
      if (sorting){
        print("a")
        dplyr::arrange(df, Mselect)
      } else {
        print("b")
        dplyr::arrange(df, desc(Mselect))
        }
    }
    
    MSA_case_filt <- MSA_case %>% 
      arrange(Date) %>% 
      group_by(MSA) %>% 
        mutate(end_case=last(!!as.name(in_y_axis)), end_day=max(Date)) %>% 
      ungroup() 
      
    
    #   Stretch scale
    daylimit <- last(MSA_case_filt$Date)+ 10
    
    print("-------------  regions plot 5")
    #   Plot region data
    p <- 
    MSA_case_filt %>% 
      ggplot(aes(x=Date, y=!!as.name(in_y_axis))) + 
      theme(legend.position = "none", text = element_text(size=20)) +
      geom_line(aes(color=MSA)) + 
      geom_label(aes(y=end_case,x=end_day,label=MSA, color=MSA),
                 size=3.0,
                 label.size = 0.15,
                 vjust="top", hjust="left") +
      expand_limits(x=daylimit) + # make room for labels
      labs(title=paste(y_labels[[in_y_axis]], "by Region"),
           x=paste0("Date"),
           y=paste(y_labels[[in_y_axis]])) 
    
    if (in_log){
      p <- p + scale_y_log10(breaks = logTicks(n = 4), minor_breaks = logTicks(n = 40))
    }
    
    print("-------------  regions plot 6")
    #-----------   Data details
    output$regions_details <- gt::render_gt({
      
      sorting <- grepl("doubling", in_y_axis)
      sort_direction <- 2*sorting-1
      
      details <- MSA_case_filt %>% 
        group_by(MSA) %>% 
          summarise(signif(last(!!as.name(in_y_axis)),3),
                    last(Cases)) %>%
        rename(!!sym(in_y_axis):=2, Total_Cases=3) %>% 
        mutate(label=y_labels[[in_y_axis]]) %>% 
        arrange(sort_direction*(!!as.name(in_y_axis))) %>%
        #arrange(desc(!!as.name(in_y_axis))) %>% 
        mutate(text=paste0(MSA,": ", label, " = ", 
                           !!as.name(in_y_axis),
                           " and Total Cases = ", Total_Cases))
    print("-------------  counties plot 7")
      
    tab <-  details %>% select(-label, -text) %>%
      gt::gt() %>%
      gt::tab_header(title="Highlighted Details") %>% 
      gt::cols_label(MSA=gt::md("**Region**"), 
                     !!sym(in_y_axis):=gt::md(paste0("**", y_labels[[in_y_axis]],"**")), 
                     Total_Cases=gt::md("**Cases**")) %>% 
      gt::tab_style(style=gt::cell_fill(color="lightcyan"),
                    locations=gt::cells_title())
    
     # HTML(paste(details$text[1:6], collapse = '<br/>'))
    
    ###############    time
    etime <- proc.time() - ptm
    print(paste("time 3:", etime[[3]]))
    ptm <<- proc.time()
    ###############    time
    
    
    tab
      
    })
    
    
    return(p)
    
  }
   
  #---------------------------------------------------    
  #------------------- Build Prisons Plot ------------
  #---------------------------------------------------    
  
  build_prisons_plot <- function(
                                in_prisons_y_axis,
                                in_prisons_selector,
                                in_prison_log
                                ){
    print(":::::::  build_prisons_plot")
    
    window <- 5
    

    
    print("-------------  prisons plot 1")
    
    #---------------  Control matrix
    
    calc_controls <- tribble(
      ~base,       ~avg, ~percap, ~trim, ~positive,
      "Cases",      TRUE, TRUE,  FALSE, TRUE,
      "pct_chg",    TRUE, FALSE, TRUE, TRUE,
      "doubling",   TRUE, FALSE, TRUE, TRUE,
      "new_cases",  TRUE, TRUE,  TRUE, TRUE
    )
    
    #--------------- Clean up unuseable choices
    
    if (grepl("percap", in_prisons_y_axis)&
        (grepl("pct_chg",in_prisons_y_axis)
         || grepl("doubling",in_prisons_y_axis))) {
      in_prisons_y_axis <- str_remove(in_prisons_y_axis, "_percap")
    }
    if (grepl("percap", in_prisons_selector)&
        (grepl("pct_chg",in_prisons_selector)
         || grepl("doubling",in_prisons_selector))) {
      in_prisons_selector <- str_remove(in_prisons_selector, "_percap")
    }
    
    print("-------------  prisons plot 2")
    # Start each county at the minimum case spot and create an x-axis variable
    prisons_case <- Prison_data %>% 
      filter(Cases>10) %>%  
      group_by(Unit) %>% 
        arrange(Date) %>% 
        mutate(day = row_number()) %>% 
        add_tally() %>% 
      ungroup() %>% 
      filter(n>5) # must have at least 5 datapoints
    
    print("-------------  prisons plot 3")
    y_labels <- list(
                     "Cases"="Number of Cases",
                     "Cases_percap"="Cases percent Pop",
                     "avg_Cases"="Avg Cases",
                     "avg_Cases_percap"="Avg Cases percent Pop",
                     "new_cases"="Number of New Cases",
                     "new_cases_percap"="New Cases percent Pop",
                     "avg_new_cases"="Avg New Cases",
                     "avg_new_cases_percap"="Avg New Cases percent Pop",
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
    
    print("-------------  prisons plot 4")
    #     Apply selector
    sorting <- grepl("doubling", in_prisons_selector)
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
        dplyr::filter(df, Mselect<(unique(Mselect)[min(length(unique(Mselect))-2,7)]))
        #dplyr::filter(df, Mselect<(unique(Mselect)[7]))
      } else {
        print("d")
        dplyr::filter(df, Mselect>(unique(Mselect)[min(length(unique(Mselect))-2,7)]))
        #dplyr::filter(df, Mselect>(unique(Mselect)[7]))
        }
    }
    
    prisons_case %>% 
      arrange(Date) %>% 
      group_by(Unit) %>% 
        mutate(Mselect=last(!!as.name(in_prisons_selector))) %>% 
        mutate(end_case=last(!!as.name(in_prisons_y_axis)), end_day=max(day)) %>% 
        do_sort(sorting) %>% 
      ungroup() %>% 
      do_filter(sorting) %>% 
      select(-Mselect) -> prisons_case_filt
    
    #   Stretch scale
    daylimit <- max(prisons_case_filt$day)*1.1
    
    print("-------------  prisons plot 5")
    #   Plot Unit data
    p <- 
    prisons_case %>% 
      ggplot(aes(x=day, y=!!as.name(in_prisons_y_axis))) + 
      #scale_y_log10(breaks = logTicks(n = 4), minor_breaks = logTicks(n = 40)) +
      theme(legend.position = "none", text = element_text(size=20)) +
      geom_line(aes(group=Unit),colour = alpha("grey", 0.7)) +
      geom_line(data=prisons_case_filt,
                aes(color=Unit)) + 
      geom_label(data=prisons_case_filt,
                 aes(y=end_case,x=end_day,label=Unit, color=Unit),
                 size=3.0,
                 label.size = 0.15,
                 vjust="top", hjust="left") +
      expand_limits(x=daylimit) + # make room for labels
      labs(title=paste("Prisons With",title_label,y_labels[[in_prisons_selector]]),
           x=paste0("Days after reaching 10 Cases"),
           y=paste(y_labels[[in_prisons_y_axis]])) 
    
    if (in_prison_log){
      p <- p + scale_y_log10(breaks = logTicks(n = 4), minor_breaks = logTicks(n = 40))
    }
    
    print("-------------  prisons plot 6")
    #-----------   Data details
    output$prisons_details <- gt::render_gt({
      
      details <- prisons_case_filt %>% 
        group_by(Unit) %>% 
          summarise(signif(last(!!as.name(in_prisons_selector)),3),
                    last(Cases), last(County)) %>% 
        rename(!!sym(in_prisons_selector):=2, Total_Cases=3, County=4) %>% 
        mutate(label=y_labels[[in_prisons_selector]]) %>% 
        arrange(desc(!!as.name(in_prisons_selector))) %>% 
        mutate(text=paste0(Unit,": ", label, " = ", 
                           !!as.name(in_prisons_selector),
                           " and Total Cases = ", Total_Cases))
      
    details %>% select(-label, -text) %>%
      gt::gt() %>%
      gt::tab_header(title="Highlighted Details") %>% 
      gt::cols_label(Unit=gt::md("**Unit**"), 
                     !!sym(in_prisons_selector):=gt::md(paste0("**",y_labels[[in_prisons_selector]],"**")), 
                     Total_Cases=gt::md("**Cases**"),
                     County=gt::md("**County**")) %>% 
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
  draw_map2 <- function(in_county_color,
                        in_min_case,
                        in_map_prisons,
                        in_map_meat_packers,
                        in_map_top5) {
    
    print(":::::::  draw_map2")
    
    draw_anim_legend <<- "none" # reset animated map legend
    
    df <- MappingData %>% 
      filter(Cases>=in_min_case)
    ##in_map_highlight <- TRUE # stub to replace when control is added
    QuantScale <- TRUE
    # Create color scale
    Range <- range(MappingData[[in_county_color]], na.rm=TRUE)
    Ncuts <- min(as.integer(sum(MappingData[[in_county_color]]>Range[1], na.rm=TRUE)/
                            sum(MappingData[[in_county_color]]==Range[1], na.rm=TRUE)), 8)
    Ncuts <- replace_na(Ncuts, 8)
    if (Ncuts<=4) {QuantScale <- FALSE}
    print(paste("Ncuts etc", Ncuts, in_county_color, QuantScale, Range))
    
    #   Subset of counties with >1% prison pop
    
    prisons <- MappingData %>% 
      filter(prison_size=="Large Inmate Pop")
    
    #   Subset of counties with meat packing
    
    meat <- MappingData %>% 
      filter(meat)
    
    #   Top five counties for chosen measure
    
    sorting <- grepl("doubling|new_tests", in_county_color)
    sort_direction <- 2*sorting-1
    
    print(paste("Sorting:", sorting, in_county_color))
    
    details <- df %>% 
      arrange(sort_direction*(!!as.name(in_county_color))) %>%
      head(5)  
    
    #   Legend titles
    #my_titles <- list("Cases"="Total Cases",
    #                  "percapita"="Cases per 100,000",
    #                  "Deaths"="Total Deaths",
    #                  "Deaths_percap"="Deaths per 100,000",
    #                  "doubling"="Doubling Time",
    #                  "avg_pct_chg"="Recent Avg Pct Change",
    #                  "deaths_percase"="Deaths per Case")
    
    my_titles <- list(
                     "Cases"="Number of Cases",
                     "Cases_percap"="Cases pct Pop",
                     "avg_Cases"="Avg Cases",
                     "avg_Cases_percap"="Avg Cases pct Pop",
                     "new_cases"="Number of New Cases",
                     "new_cases_percap"="New Cases pct Pop",
                     "avg_new_cases"="Avg New Cases",
                     "avg_new_cases_percap"="Avg New Cases pct Pop",
                     "active_cases"="Number of Active Cases",
                     "active_cases_percap"="Active Cases per 100,000",
                     "avg_active_cases"="Avg Active Cases",
                     "avg_active_cases_percap"="Avg Active Cases per 100,000",
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
                     "avg_doubling"="Avg Doubling Time in Days",
                     "deaths_percase"="Deaths per Case"
                     )
    # Usually reverse scale, but not always
    color_reverse <- TRUE
    #if (grepl("doubl", in_county_color)) {color_reverse <- FALSE}
    if (sorting) {color_reverse <- FALSE}
    #browser()
    
    if (QuantScale) {
      print("----- color Quantile")
      pal <- colorQuantile(palette = heat.colors(Ncuts), 
                           domain = MappingData[[in_county_color]], 
                           n = Ncuts, 
                           na.color = "transparent", 
                           alpha = FALSE, 
                           reverse = color_reverse,
                           right = FALSE) 
    } else {
      print("----- color Numeric")
      pal <- colorNumeric(palette = heat.colors(8),
                          na.color = "transparent",
                          reverse=color_reverse,
                          domain = MappingData[[in_county_color]])
    }
    
    print("---Map   1")
    
    # Draw the map
    
    output$TexasMap <- renderLeaflet({
      #   Basemap
      my_map <- leaflet(MappingData) %>% 
        setView(lng = MapCenter[1] , lat = MapCenter[2], zoom = init_zoom ) %>%   
        addTiles() %>%
        addPolygons(data = MappingData, 
                    group="polys",
                    stroke = TRUE,
                    weight = 1,
                    smoothFactor = 0.2, 
                    fillOpacity = 0.7,
                    label = MapLabels,
                    fillColor = ~pal(MappingData[[in_county_color]]))  
      
    print("---Map   2")
      if (in_map_prisons) {
        my_map <- my_map %>% 
                  addPolylines(data=prisons, color="black", weight=2, opacity=1)
      }
    print("---Map   3")
      if (in_map_meat_packers) {
        my_map <- my_map %>% 
                  addPolylines(data=meat, color="blue", weight=2, opacity=1)
      }
    print("---Map   3.5")
      if (in_map_top5) {
        my_map <- my_map %>% 
                  addPolylines(data=details, color="white", weight=2, opacity=1)
      }
    print("---Map   4")
      
      if (QuantScale) {
        my_map %>% 
        addLegend("bottomleft", pal = pal, 
                  group="map_legend",
                  values = MappingData[[in_county_color]], 
                  title = my_titles[[in_county_color]],
                  labels= as.character(seq(Range[1], Range[2], length.out = 5)),
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0(signif(cuts[-n],2), " &ndash; ", signif(cuts[-1],2))
                  },
                  opacity = 1)
      } else {
        my_map %>% 
          addLegend("bottomleft", pal = pal, 
                    group="map_legend",
                    values = MappingData[[in_county_color]], 
                    title = my_titles[[in_county_color]],
                    opacity = 1)
      }
    }) 
    
    output$map_details <- gt::render_gt({ 
      
      details <- details %>% 
        as_tibble() %>% 
        #select(-SHAPE) %>% 
        select(County, !!as.name(in_county_color))
      
    tab <-  details %>%
      gt::gt() %>%
      gt::tab_header(title="Worst Five") %>% 
      gt::cols_label(County=gt::md("**County**"), 
                     !!sym(in_county_color):=gt::md(paste0("**", my_titles[[in_county_color]],"**"))) %>% 
      gt::tab_style(style=gt::cell_fill(color="lightcyan"),
                    locations=gt::cells_title()) 
    
    tab
    } )
  }
  
###################   end of map
  
  ######################  Animated Map ########################
  #---------------------------------------------------    
  #------------------- Build Model -------------------
  #---------------------------------------------------    
  draw_animap <- function(in_county_color,
                          in_map_anim_date,
                          in_map_meat_packers,
                          in_map_prisons,
                          in_map_top5
                        ) {
    
    print(":::::::  draw_animap")
    
    print(paste("Animap:", draw_anim_map, draw_anim_legend))
    
    #####    First generate a scale for the chosen variable
    
    Range <- range(county_animate[[in_county_color]], na.rm=TRUE)
    Ncuts <- min(as.integer(sum(county_animate[[in_county_color]]>Range[1], na.rm=TRUE)/
                            sum(county_animate[[in_county_color]]==Range[1], na.rm=TRUE)), 8)
    Ncuts <- replace_na(Ncuts, 8)
    QuantScale <- TRUE
    if (Ncuts<=3) {QuantScale <- FALSE}
    print(paste("Ncuts etc", Ncuts, in_county_color, QuantScale, Range))
    
    ###########   test new binning
    
    kmeans_loc <- c(1+which(diff(kmeans(sort(county_animate[[in_county_color]]), 5)[["cluster"]])!=0))
    kmeans_bins <- signif(c(0,
                     sort(county_animate[[in_county_color]])[kmeans_loc],
                     max(county_animate[[in_county_color]], na.rm=TRUE)),3)
    
    ###########   end test
    
    sorting <- grepl("doubling|new_tests", in_county_color)
    sort_direction <- 2*sorting-1
    
    my_titles <- list(
                     "Cases"="Number of Cases",
                     "Cases_percap"="Cases pct Pop",
                     "new_cases"="Number of New Cases",
                     "new_cases_percap"="New Cases pct Pop",
                     "active_cases"="Number of Active Cases",
                     "active_cases_percap"="Active Cases per 100,000",
                     "Deaths"="Number of Deaths",
                     "Deaths_percap"="Deaths per 100,000",
                     "new_deaths"="Number of New Deaths",
                     "new_deaths_percap"="New Deaths per 100,000",
                     "pct_chg"="Percent Change",
                     "doubling"="Doubling Time in Days",
                     "deaths_percase"="Deaths per Case"
                     )
    
    #   Subset of counties with >1% prison pop
    
    prisons <- MappingData %>% 
      filter(prison_size=="Large Inmate Pop")
    
    #   Subset of counties with meat packing
    
    meat <- MappingData %>% 
      filter(meat)
    
    # Usually reverse scale, but not always
    color_reverse <- TRUE
    if (sorting) {color_reverse <- FALSE}
    
    if (QuantScale) {
      print("----- color Quantile")
      pal <- colorQuantile(palette = heat.colors(Ncuts), 
                           domain = county_animate[[in_county_color]], 
                           n = Ncuts, 
                           na.color = "transparent", 
                           alpha = FALSE, 
                           reverse = color_reverse,
                           right = FALSE) 
    } else {
      print("----- color Numeric")
      pal <- colorNumeric(palette = heat.colors(8),
                          na.color = "transparent",
                          reverse=color_reverse,
                          domain = county_animate[[in_county_color]])
    }
    
    #######   kmeans
    pal <- colorBin(palette = heat.colors(5), 
                    bins = kmeans_bins, 
                    pretty = TRUE,
                    na.color = "transparent",
                    reverse=color_reverse,
                    domain = county_animate[[in_county_color]],
                    alpha = FALSE, 
                    right = FALSE)
    #######   kmeans
        #####    Extract data for given date
    
    One_week <- county_animate %>% 
      filter(Date==in_map_anim_date) %>% 
      filter(!is.na((!!sym(in_county_color)))) # select rows with data
    
    #   Top five counties for chosen measure
    
    sorting <- grepl("doubling|new_tests", in_county_color)
    sort_direction <- 2*sorting-1
    
    print(paste("Sorting:", sorting, in_county_color))
    
    details <- One_week %>% 
      arrange(sort_direction*(!!as.name(in_county_color))) %>%
      head(5)  %>% 
      mutate(!!in_county_color:=signif(!!as.name(in_county_color), 3))
    
   output$TexasMap <- renderLeaflet({
        my_map <- leaflet(county_animate) %>%
          addTiles() %>%
          setView(lng = MapCenter[1] , lat = MapCenter[2], zoom = init_zoom )   
      draw_anim_map <<- TRUE
    

    # Build labels for map
    
    Map_anim_labels <- lapply(seq(nrow(One_week)), function(i) {
      htmltools::HTML(
        str_replace_all(
          paste( One_week[i,]$County, 'County<br>' 
          ),
          "NA", "Zero"))
    })

    #####    Draw counties
    
    #leafletProxy("TexasMap") %>% 
    my_map <-  my_map %>% 
        clearGroup(group="polys") %>% 
        addPolygons(data = One_week, 
                    group="polys",
                    stroke = TRUE,
                    weight = 1,
                    smoothFactor = 0.2, 
                    fillOpacity = 0.7,
                    label=Map_anim_labels,
                    fillColor = ~pal(One_week[[in_county_color]]))  
    #     Highlight prisons
    if (in_map_prisons) {
      my_map <- my_map %>% 
        addPolylines(data=prisons, color="black", weight=2, opacity=1)
    }
    print("---Map   3")
    #      Highlight meat packers
    if (in_map_meat_packers) {
      my_map <- my_map %>% 
        addPolylines(data=meat, color="blue", weight=2, opacity=1)
    }
    #      Highlight worst 5
      if (in_map_top5) {
        my_map <- my_map %>% 
                  addPolylines(data=details, color="white", weight=2, opacity=1)
      }
    #####     Draw legend, if necessary
    
      if (QuantScale) {
        my_map <- my_map %>%
        clearGroup(group="map_legend") %>% 
        addLegend("bottomleft", pal = pal, 
                  group="map_legend",
                  values = county_animate[[in_county_color]],
                  title = my_titles[[in_county_color]],
                  labels= as.character(seq(Range[1], Range[2], length.out = 5)),
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0(signif(cuts[-n],2), " &ndash; ", signif(cuts[-1],2))
                  },
                  opacity = 1)
      } else {
        my_map <- my_map %>%
        clearGroup(group="map_legend") %>% 
        addLegend("bottomleft", pal = pal, 
                  group="map_legend",
                  values = county_animate[[in_county_color]],
                  title = my_titles[[in_county_color]],
                  opacity = 1)
      }
      draw_anim_legend <<- in_county_color
    my_map # draw the bugger

     } )
       
    output$map_details <- gt::render_gt({ 
      
      details <- details %>% 
        as_tibble() %>% 
        select(County, !!as.name(in_county_color))
      
    tab <-  details %>%
      gt::gt() %>%
      gt::tab_header(title="Worst Five") %>% 
      gt::cols_label(County=gt::md("**County**"), 
                     !!sym(in_county_color):=gt::md(paste0("**", my_titles[[in_county_color]],"**"))) %>% 
      gt::tab_style(style=gt::cell_fill(color="lightcyan"),
                    locations=gt::cells_title()) 
    
    tab
    } )
  }
  
###################   end of map  
  
  ######################  Harris ########################
  #---------------------------------------------------    
  #------------------- Build Model -------------------
  #---------------------------------------------------    
   draw_harris <- function(in_color,
                          in_har_school,
                          in_har_extreme_value,
                          in_har_extremes,
                          in_har_anim_date,
                          in_min_har_case,
                          in_har_cross,
                          in_map_top5) {
     
    print(":::::::  draw_harris")
    
    print(paste("Harris:", in_har_school, in_color))
    
    my_titles <- list(
      "Cases"="Number of Cases",
      "Cases_percap"="Cases pct Pop",
      "new_cases"="Number of New Cases",
      "new_cases_percap"="New Cases pct Pop",
      "active_cases"="Number of Active Cases",
      "active_cases_percap"="Active Cases per 1,000",
      "pct_chg"="Percent Change",
      "Doubling"="Doubling Time in Days"
    )
  my_x_label <- list(
    "Med_Income"="Median Income",
    "MedianAge"="Median Age",
    "WhitePct"="Pct White",
    "BlackPct"="Pct Black",
    "HispanicPct"="Pct Hispanic",
    "MultigenPct"="Pct Multigenerational",
    "Density"="People per sq mi",
    "blueness"="Percent vote for Clinton",
    "Age0to4"="Ages 0-4 %",
    "Age5to9"="Ages 5-9 %",
    "Age10to14"="Ages 10-14 %",
    "Age15to19"="Ages 15-19 %",
    "Age20to24"="Ages 20-24 %",
    "Age25to34"="Ages 25-34 %",
    "Age35to44"="Ages 35-44 %",
    "Age45to54"="Ages 45-54 %",
    "Age55to59"="Ages 55-54 %",
    "Age60to64"="Ages 60-64 %",
    "Age65to74"="Ages 65-74 %",
    "Age75to84"="Ages 75-85 %",
    "Age85andup"="Over Age 85 %"
  )
  my_y_label <- list(
    "Cases"="Cases",
    "new_cases"="New Cases",
    "pct_chg"="Week-on-Week Percent Change",
    "active_cases"="Active Cases (est)",
    "Doubling"="Doubling Time in Days",
    "Cases_percap"="Cases per 1,000 Pop",
    "new_cases_percap"="New Cases per 1,000 Pop",
    "active_cases_percap"="Active Cases per 1,000 Pop (est)"
  )
    
  
    #   Throw out zipcodes with less than the minimum number of cases
    
    Grouper <- rlang::sym(in_har_school)
    
    if (in_har_school=="Zip") {
      input_data <- Harris_zip
      input_data_polys <- Harris_zip_polys
    }
    if (in_har_school=="District") {
      input_data <- Harris_schools
      input_data_polys <- Harris_school_polys
    }
    
    History <- input_data %>% 
      group_by(!!Grouper) %>% 
        mutate(maxcase=max(Cases, na.rm=TRUE)) %>% 
      ungroup() %>% 
      filter(maxcase >= in_min_har_case) %>% 
      select(-maxcase) %>% 
      arrange(!!Grouper) %>% 
      mutate(id=row_number()-1)  
    
    #aa<<-History
    #   Sort direction for ranking
    sorting <- grepl("Doubling", in_color)
    if (is.null(in_color)){ # first time through
      sorting <- FALSE
    }
    sort_direction <- 2*sorting-1
    
    #   Pick dataset and week to display
    
    start_date <- lubridate::ymd("2020-01-01")
    anim_week <- ((interval(start_date, in_har_anim_date) %/% lubridate::weeks(1)) + 1)
      #     Map Data
    print(paste("------ 3641", start_date, anim_week))
      map_data <- History %>% 
        select(-id) %>% 
        ####filter(week==lubridate::week(in_har_anim_date)) %>% 
        filter(week==anim_week) %>% 
        filter(!is.na((!!sym(in_color)))) %>%  # select rows with data
        arrange(sort_direction*(!!sym(in_color))) %>% 
        mutate(rank=1:n()) %>% 
        mutate(id=row_number()-1) %>% 
        left_join(., input_data_polys, by=in_har_school)
    ##  mutate(week=(interval(start_date, Date) %/% weeks(1)) + 1)  
      
      #aa<<-map_data
      #     Top and Bottom subsets
      cutoff <- round(in_har_extreme_value*1.54)
      top_zips <- map_data %>% 
        filter(rank<=cutoff) %>% 
        select(Grouper)
      print(top_zips)
      bot_zips <- map_data %>% 
        filter((153-rank)<=cutoff) %>% 
        select(Grouper)
      print(bot_zips)
      
      #     Color scales for map
      
      kmeans_loc <- c(1+which(diff(kmeans(sort(History[[in_color]]), 5)[["cluster"]])!=0))
      kmeans_bins <- signif(c(0,
                              sort(input_data[[in_color]])[kmeans_loc],
                              max(input_data[[in_color]], na.rm=TRUE)*1.05),3)
      
      # Usually reverse scale, but not always
      color_reverse <- TRUE
      if (sorting) {color_reverse <- FALSE}
      pal <- colorBin(palette = heat.colors(5), 
                      bins = kmeans_bins, 
                      pretty = TRUE,
                      na.color = "transparent",
                      reverse=color_reverse,
                      domain = input_data[[in_color]],
                      alpha = FALSE, 
                      right = FALSE)
      
      #     History Data
      top_filt <<- History %>% 
        filter(!!Grouper %in% top_zips[[1]])
      
    #########    Draw map
    
   output$HarrisMap <- renderLeaflet({
        # Build labels for map
        
      map_data <- sf::st_as_sf(map_data)
      
        Map_labels <- lapply(seq(nrow(map_data)), function(i) {
          htmltools::HTML(
            str_replace_all(
              paste( map_data[i,][[in_har_school]], 
                     '<br>',
                     map_data[i,][[in_color]]),
              "NA", "Zero"))
        })
        
        my_map <- leaflet(map_data) %>%
          addTiles() %>%
          setView(lng = -95.3103, 
                  lat = 29.7752, 
                  zoom = 9 )   
        my_map <-  my_map %>% 
          clearGroup(group="polys") %>% 
          addPolygons(data = map_data, 
                      group="polys",
                      stroke = TRUE,
                      weight = 1,
                      smoothFactor = 0.2, 
                      fillOpacity = 0.7,
                      label=Map_labels,
                      fillColor = ~pal(map_data[[in_color]]))  
        my_map <- my_map %>%
          clearGroup(group="map_legend") %>% 
          addLegend("bottomleft", pal = pal, 
                    group="map_legend",
                    values = map_data[[in_color]],
                    title = my_titles[[in_color]],
                    opacity = 1)
        
        isLayer <- FALSE
        id <- selected_polys()
        if (!is.null(id)){
          print(paste("====id====", id))
          isLayer <- TRUE
           #my_map <- my_map %>% 
           #  addPolylines(data=map_data[map_data$id==id,], color="white", weight=2, opacity=1)
          leafletProxy("HarrisMap", data=map_data[map_data$id==id,]) %>% 
             addPolylines(color="white", weight=2, opacity=1, layerId="xplot")
        } else {
          if (isLayer) {removeShape("HarrisMap", "xplot")}
        }
        #    can't link id back to zip
        # id <- selected_hpolys()
        # if (!is.null(id)){
        #   zip <- History[History$id==id,][[Grouper]]
        #   print(paste("zip", zip))
        #   leafletProxy("HarrisMap", data=map_data[map_data[[Grouper]]==zip,]) %>% 
        #      addPolylines(color="white", weight=2, opacity=1)
        # }
        my_map
   })
   
   ##########    Draw History plot
   
   output$har_history <- renderPlotly({
     
     my_history <- ggplot(History, 
                          aes(x=Date, 
                              y=Cases)) +
       theme(legend.position = "none") +
       geom_line(aes(group=!!as.name(in_har_school)),
                 colour = alpha("grey", 0.7)) +
       geom_vline(xintercept=as.numeric(in_har_anim_date)) +
       ###geom_point(aes(group=!!as.name(in_har_school))) +
       geom_line(data=top_filt,
                 aes(color=!!as.name(in_har_school)))# +  
       ###labs(title=paste("Highlighted by",my_y_label[[in_color]])) +
       ###theme(plot.title = element_text(margin = margin(t = -10, b = -60)))
       ###      x=paste0("Days after reaching ",in_case_start," Cases"),
     ggplotly(my_history, source='history') %>% 
                config(modeBarButtonsToRemove = c(
                  "toImage", "hoverCompareCartesian", "toggleSpikelines"))
     
   }) # end Draw History Plot
     
   
   ##########    Draw Cross plot
             
   output$har_xplot <- renderPlotly({
     
     
     my_xplot <-
         ggplot(data=map_data, 
                        aes(x=!!as.name(in_har_cross), 
                            y=!!as.name(in_color),
                            label=!!as.name(in_har_school))) +
         geom_point() +
         geom_smooth(method='lm') +
         labs(x=my_x_label[[in_har_cross]],
              y=my_y_label[[in_color]]) 
     
     ggplotly(my_xplot, source='xplot') %>% 
                config(modeBarButtonsToRemove = c(
                  "toImage", "hoverCompareCartesian", "toggleSpikelines"))
       
   })  #  end Draw Cross Plot
   
   ##########    Draw Histogram
   
   output$har_histogram <- renderPlotly({
     
     my_histogram <- ggplot(data=map_data, 
                        aes(x=!!as.name(in_color))) +
       geom_histogram() +
       labs(x=my_y_label[[in_color]])
     
     ggplotly(my_histogram) %>% 
                config(modeBarButtonsToRemove = c(
                  "toImage", "hoverCompareCartesian", "toggleSpikelines"))
       
   })  #  end Draw Histogram
   
    output$HarrisTitle <- renderUI(HTML(paste0("<center><h4>Week of ", 
                                               sf(in_har_anim_date),
                                               ", Analysis By ",
                                               in_har_school,
                                               "</h4></center>")))
   } 
###################   end of Draw Harris  
     
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
   
  selected_polys <- reactive({
    print("========= selected_polys")
        eventdata <- event_data('plotly_click', source = 'xplot')
        print(eventdata)
        id <- as.numeric(eventdata[['pointNumber']])
        sub <- eventdata
        if (!is.null(eventdata)) {
          sub <- id
        }
        return(sub)
  })    
  #     Can't figure out how to link id back to zip code
  # selected_hpolys <- reactive({
  #   print("========= selected_hpolys")
  #       eventdata <- event_data('plotly_click', source = 'history')
  #       print(eventdata)
  #       id <- as.numeric(eventdata[['pointNumber']])
  #       sub <- eventdata
  #       if (!is.null(eventdata)) {
  #         sub <- id
  #       }
  #       return(sub)
  # })    
   
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
      fit_logistic() } 
    else if (input$modeling=="piecewise") {
        fit_piecewise(indep="Cases",
                      projection=10,
                      #min_length=8,
                      #min_rsqr=0.99,
                      #pct_chg=0.05,
                      input$min_length,
                      input$min_rsqr,
                      input$pct_chg*0.01,
                      calc_conf=TRUE) 
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
    } else if (input$death_modeling=="piecewise") {
        fit_piecewise(indep="Deaths",
                      projection=10,
                      calc_conf=TRUE) 
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
                                 input$ind_percap,
                                 input$ind_avg,
                                 input$smooth,
                                 input$smthlength
                                 )
          output$plot_slopes <- renderPlot({print(p)})
        } else {
          showNotification("Too little case data")
        }
      } 
  #---------------------------------
  #------------- Tests tab
  #---------------------------------
      if (input$An_tabs == "Tests") {
        if (sum(!is.na(subdata$Tests))>15) {
          p <- build_tests_plot(
            input$Tests_logscale,
            input$Tests_Percapita,
            input$Tests_New,
            input$test_display
          )
          output$plot_tests <- renderPlot({print(p)})
        } else {
          showNotification("Too little test data")
        }
      } 
      #if (input$An_tabs == "Something") {
         # p <- build_something()
      #}
      
      print("============== end select An data ==================")
  }) #   end of respond to data change
   

  #---------------------------------------------------    
  #------------------- Rerun fitting -----------------
  #---------------------------------------------------    
  
  observeEvent({ #  fit cases
                input$modeling #do fit, piecewise, standard,  user, logistic
                input$slope
                input$intercept
                input$min_length
                input$min_rsqr
                input$pct_chg
                1}, { # change display
                  
    # open or close parameter menu  
    #Case_Parameters(input, output)
    # fit a model to the data
    if (input$modeling=="logistic") {
      fit_logistic()
    } else if (input$modeling=="piecewise") {
      fit_piecewise(indep="Cases",
                    projection=10,
                    #input$min_length=8,
                    #input$min_rsqr=0.99,
                    #input$pct_chg=0.05,
                    input$min_length,
                    input$min_rsqr,
                    input$pct_chg*0.01,
                    calc_conf=TRUE) 
    } else {
      m <- input$slope
      b <- input$intercept
      if (is.null(m)) {m <- global_slope}
      if (is.null(b)) {b <- 1}
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
    } else  if (input$death_modeling=="logistic") {
      fit_logistic(indep="Deaths")
    } else if (input$death_modeling=="piecewise") {
      fit_piecewise(indep="Deaths",
                    #projection=10,
                    #input$min_length,
                    #input$min_rsqr,
                    #input$pct_chg*0.01,
                    calc_conf=TRUE) 
    } else {print("Whoa! Bad input!!") }
    
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
      #if (input$An_tabs == "Something") {
         # p <- build_something()
      #}
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
                input$ind_percap
                input$ind_avg
                input$smooth
                input$smthlength
                input$An_tabs
                1} , { # 
      if (input$An_tabs == "Indicators") {
          if (sum(!is.na(subdata$Cases))>15) {
            p <- build_slope_plot(
              input$window,
              input$slopetype,
              input$ind_percap,
              input$ind_avg,
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
  #------------------- Testing -----------------------
  #---------------------------------------------------    
  observeEvent({
                input$Tests_logscale
                input$Tests_Percapita
                input$Tests_New
                input$test_display
                1} , { # 
          if (sum(!is.na(subdata$Tests))>15) {
            p <- build_tests_plot(
              input$Tests_logscale,
              input$Tests_Percapita,
              input$Tests_New,
              input$test_display
            )
            output$plot_tests <- renderPlot({print(p)})
          } else {
            showNotification("Too little test data")
          }
  }) 
  #---------------------------------------------------    
  #------------------- Analysis Tab ------------------
  #---------------------------------------------------    
 # observeEvent({input$An_tabs
 #               1} , { # 
 #   print(":::::::  observe_event 2")
 # })
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
    if (input$counties_avg &
        calc_controls$avg[calc_controls$base==input$counties_y_axis]) {
      y_axis <- paste0("avg_", y_axis)}
    if (input$counties_percap &
        calc_controls$percap[calc_controls$base==input$counties_y_axis]) {
      y_axis <- paste0(y_axis,"_percap")}
                  
    selector <- input$counties_selector
    if (input$counties_select_avg &
        calc_controls$avg[calc_controls$base==input$counties_selector]) {
      selector <- paste0("avg_", selector)}
    if (input$counties_select_percap &
        calc_controls$percap[calc_controls$base==input$counties_selector]) {
      selector <- paste0(selector,"_percap")}
    
    p <- build_counties_plot(y_axis,
                             selector,
                             input$case_start,
                             input$county_log
                            )
                  
    output$plot_counties <- renderPlot({print(p)})             
  })
  #---------------------------------------------------    
  #------------------- Regions Tab ------------------
  #---------------------------------------------------    
  
  Regions_y_axis <- callModule(module=attribute_select_server, 
                               id="Regions", 
                               tab_name="Regions")
  
  observeEvent({#input$RegionsTab
                Regions_y_axis()$y_axis
                1} , { # 
    print(":::::::  observe_event RegionsTab y axis only")
    print(paste("--selector",Regions_y_axis()$y_axis ))
    print(paste("--selector2",input$Regions_selector ))
    
    sorting <- grepl("doubling", Regions_y_axis()$y_axis)
    if (is.null(Regions_y_axis()$y_axis)){ # first time through
      sorting <- FALSE
    }
    
    sort_direction <- 2*sorting-1
    print(paste("sort direction = ", sort_direction))
    
    choices <- MSAs %>% 
      filter(MSA!="Texas") %>% 
      filter(!(MSA %in% input$Regions_selector)) %>% 
      group_by(MSA) %>% 
        summarise_all(last) %>%
      arrange(sort_direction*(!!sym(Regions_y_axis()$y_axis))) %>% 
      select(MSA) %>% 
      unique() 
        
        choices <- choices[[1]]
    print(head(choices))
    
    output$RegionSelect <- renderUI({
      selectInput('Regions_selector', 
                  label=NULL, 
                  choices=choices,
                  multiple=TRUE, 
                  selectize=TRUE)
    })
    
    print(head(choices))
  })
  observeEvent({#input$RegionsTab
                input$Regions_selector
                input$Regions_case_start
                input$Regions_log
                1} , { # 
    print(":::::::  observe_event RegionsTab")
    
    ###############    time
    ptm <<- proc.time()
    ###############    time
    
    p <- build_Regions_plot(Regions_y_axis()$y_axis,
                             input$Regions_selector,
                             input$Regions_case_start,
                             input$Regions_log
                            )
    print("ready to plot regions")
    
    ###############    time
    etime <- proc.time() - ptm
    print(paste("time 1:", etime[[3]]))
    ptm <<- proc.time()
    ###############    time
    
                  
    output$plot_Regions <- renderPlot({print(p)})             
    
    ###############    time
    etime <- proc.time() - ptm
    print(paste("time 2:", etime[[3]]))
    ptm <<- proc.time()
    ###############    time
    
  })
    
    
  #---------------------------------------------------    
  #------------------- Prisons Tab ------------------
  #---------------------------------------------------    
  observeEvent({input$PrisonsTab
                input$prisons_y_axis
                input$prisons_selector
                input$prisons_avg
                input$prisons_percap
                input$prisons_select_avg
                input$prisons_select_percap
                input$prison_log
                1} , { # 
    print(":::::::  observe_event PrisonsTab")
                  
                  print(paste(input$prisons_y_axis,
                              input$prisons_selector,
                              input$prison_log))
                  
    y_axis <- input$prisons_y_axis
    if (input$prisons_avg) {y_axis <- paste0("avg_", y_axis)}
    if (input$prisons_percap) {y_axis <- paste0(y_axis,"_percap")}
                  
    selector <- input$prisons_selector
    if (input$prisons_select_avg) {selector <- paste0("avg_", selector)}
    if (input$prisons_select_percap) {selector <- paste0(selector,"_percap")}
    
    y_axis <- input$prisons_y_axis
    if (input$prisons_avg &
        calc_controls$avg[calc_controls$base==input$prisons_y_axis]) {
      y_axis <- paste0("avg_", y_axis)}
    if (input$prisons_percap &
        calc_controls$percap[calc_controls$base==input$prisons_y_axis]) {
      y_axis <- paste0(y_axis,"_percap")}
                  
    selector <- input$prisons_selector
    if (input$prisons_select_avg &
        calc_controls$avg[calc_controls$base==input$prisons_selector]) {
      selector <- paste0("avg_", selector)}
    if (input$prisons_select_percap &
        calc_controls$percap[calc_controls$base==input$prisons_selector]) {
      selector <- paste0(selector,"_percap")}
    
    p <- build_prisons_plot(y_axis,
                             selector,
                             input$county_log
                            )
                  
    output$plot_prisons <- renderPlot({print(p)})             
  })
    
  #---------------------------------------------------    
  #------------------- Mapping Controls --------------
  #---------------------------------------------------    
  observeEvent({
    input$map_color
    input$map_avg
    input$map_percap
    input$map_prisons
    input$map_meat_packers
    input$map_top5
    input$min_case
    input$map_animate
    input$map_anim_date
    1} , { #  draw map
      
    if (!input$map_animate) {
      in_county_color <- input$map_color
      if (input$map_avg) {in_county_color <- paste0("avg_", in_county_color)}
      if (input$map_percap) {in_county_color <- paste0(in_county_color,"_percap")}
      
      #--------------- Clean up unuseable choices
      
      if (grepl("percap", in_county_color)&
          (grepl("pct_chg",in_county_color)
           || grepl("doubling",in_county_color)
           || grepl("Pct_pos",in_county_color)
           || grepl("percase", in_county_color))) {
        in_county_color <- str_remove(in_county_color, "_percap")
      }
      if (grepl("avg_", in_county_color)&
          (grepl("percase",in_county_color)
           || grepl("Pct_pos", in_county_color)
           || grepl("new_tests", in_county_color))){
        in_county_color <- str_remove(in_county_color, "avg_")
      }
      
      draw_map2(in_county_color,
                input$min_case,
                input$map_meat_packers,
                input$map_prisons,
                input$map_top5)
      }
    if (input$map_animate) {
      print("----------   animap")
      
      in_county_color <- input$map_color
      if (input$map_percap) {in_county_color <- paste0(in_county_color,"_percap")}
      
      #--------------- Clean up unuseable choices
      
      if (grepl("percap", in_county_color)&
          (grepl("pct_chg",in_county_color)
           || grepl("doubling",in_county_color)
           || grepl("Pct_pos",in_county_color)
           || grepl("percase", in_county_color))) {
        in_county_color <- str_remove(in_county_color, "_percap")
      }
      
      print(paste0("----- in_county_color", input$map_color, in_county_color))
      
      draw_animap(in_county_color,
                  input$map_anim_date,
                  input$map_meat_packers,
                  input$map_prisons,
                  input$map_top5
                )
      }
    })
  
  #---------------------------------------------------    
  #------------- Harris County Controls --------------
  #---------------------------------------------------    
  # observeEvent({
  #   input$history_click
  # 1} , ignoreInit = TRUE, {
  #   Pt <- nearPoints(Harris_zip, input$history_click, addDist = TRUE)
  #   print(Pt)
  #   #showNotification(Pt$Zip[1])
  # })
  # observeEvent({
  #   input$xplot_click
  # 1} , ignoreInit = TRUE, {
  #   Pt <- nearPoints(Harris_zip, input$xplot_click, addDist = TRUE)
  #   print(Pt)
  #   #showNotification(Pt$Zip[1])
  # })
  observeEvent({
    input$har_school
    input$har_percap
    input$har_color
    input$har_extreme_value
    input$har_extremes
    input$har_anim_date
    input$min_har_case
    input$har_top5
    input$har_cross
    #input$history_click
    1} , { 
      
    in_color <- input$har_color
    if (input$har_percap) {in_color <- paste0(in_color,"_percap")}
    
    #--------------- Clean up unusable choices
    
    if (grepl("percap", in_color)&
        (grepl("pct_chg",in_color)
         || grepl("Doubling",in_color)
         )) {
      in_color <- str_remove(in_color, "_percap")
    }
    
    draw_harris(in_color,
                input$har_school,
                input$har_extreme_value,
                input$har_extremes,
                input$har_anim_date,
                input$min_har_case,
                input$har_cross,
                input$map_top5)
    
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

