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


# Add dummy Estimate field

#DF <- DF %>% 
#    mutate(Estimate=Cases)


#   Last date in dataset formatted for plotting

sf <- stamp_date("Sunday, Jan 17, 1999")
lastdate <- sf(DF$Date[nrow(DF)])

LastDate <- DF[nrow(DF),]$Date


# Load population of counties into tibble
Counties <- tribble(
    ~County, ~Population,
    "Harris", 4602523, "Dallas", 2586552, "Tarrant", 2019977,
    "Bexar", 1925865, "Travis", 1203166, "Collin", 944350, "Hidalgo", 849389,
    "El Paso", 837654, "Denton", 807047, "Fort Bend", 739342,
    "Montgomery", 554445, "Williamson", 527057, "Cameron", 421750,
    "Nueces", 360486, "Brazoria", 353999, "Bell", 342236,
    "Galveston", 327089, "Lubbock", 301454, "Webb", 272053,
    "Jefferson", 255210, "McLennan", 248429, "Smith", 225015,
    "Brazos", 219193, "Hays", 204150, "Ellis", 168838,
    "Midland", 164194, "Johnson", 163475, "Ector", 158342,
    "Guadalupe", 155137, "Taylor", 136348, "Comal", 135097,
    "Randall", 132475, "Wichita", 131818, "Parker", 129802,
    "Grayson", 128560, "Gregg", 123494, "Potter", 120899,
    "Kaufman", 118910, "Tom Green", 117466, "Bowie", 93858,
    "Rockwall", 93642, "Hunt", 92152, "Victoria", 91970,
    "Angelina", 87607, "Orange", 84047, "Bastrop", 82577,
    "Liberty", 81862, "Henderson", 80460, "Coryell", 75389,
    "Walker", 71539, "San Patricio", 67046, "Harrison", 66645,
    "Nacogdoches", 65558, "Wise", 64639, "Starr", 63894,
    "Maverick", 57970, "Anderson", 57863, "Hood", 56901,
    "Hardin", 56379, "Van Zandt", 54368, "Rusk", 53595,
    "Cherokee", 51903, "Kerr", 51365, "Waller", 49987,
    "Lamar", 49532, "Medina", 49334, "Val Verde", 49027,
    "Atascosa", 48828, "Navarro", 48583, "Wilson", 48198,
    "Polk", 47837, "Burnet", 45750, "Wood", 43815,
    "Kendall", 41982, "Wharton", 41551, "Erath", 41482,
    "Caldwell", 41401, "Jim Wells", 41192, "Upshur", 40769,
    "Chambers", 40292, "Cooke", 39571, "Brown", 37834,
    "Matagorda", 36743, "Howard", 36667, "Hopkins", 36240,
    "Jasper", 35504, "Hill", 35399, "Washington", 34796,
    "Fannin", 34175, "Hale", 34113, "Titus", 32730,
    "Bee", 32691, "Kleberg", 31425, "Cass", 30087,
    "Austin", 29565, "Palo Pinto", 28317, "San Jacinto", 27819,
    "Grimes", 27630, "Uvalde", 27009, "Gillespie", 26208,
    "Shelby", 25478, "Fayette", 25066, "Aransas", 24763,
    "Milam", 24664, "Limestone", 23515, "Panola", 23440,
    "Hockley", 23162, "Houston", 22955, "Gray", 22685,
    "Calhoun", 21807, "Moore", 21801, "Bandera", 21763,
    "Willacy", 21754, "Hutchinson", 21571, "Tyler", 21496,
    "Colorado", 21022, "Gonzales", 20667,  "Lampasas", 23399,
    "Llano", 20640, 
    "DeWitt", 20435, "Gaines", 20321, "Lavaca", 19941,
    "Jones", 19891, "Freestone", 19709, "Montague", 19409,
    "Frio", 19394, "Deaf Smith", 18899, "Eastland", 18270,
    "Bosque", 18122, "Young", 18114, "Burleson", 17863,
    "Andrews", 17818, "Falls", 17299, "Scurry", 17239,
    "Leon", 17098, "Lee", 16952, "Robertson", 16890,
    "Pecos", 15797, "Karnes", 15387, "Reeves", 15125,
    "Nolan", 14966, "Jackson", 14820, "Trinity", 14569,
    "Zapata", 14369, "Madison", 14128, "Newton", 14057,
    "Callahan", 13770, "Comanche", 13495, "Lamb", 13262,
    "Dawson", 12964, "Wilbarger", 12906, "Camp", 12813,
    "Terry", 12615, "Morris", 12424, "Red River", 12275,
    "Zavala", 12131, "Live Oak", 12123, "Ward", 11586,
    "Rains", 11473, "Duval", 11355, "Blanco", 11279,
    "Franklin", 10679, "Dimmit", 10663, "Sabine", 10458,
    "Clay", 10387, "Ochiltree", 10348, "Runnels", 10310,
    "Marion", 10083, "Parmer", 9852, "Stephens", 9372,
    "Brewster", 9216, "Jack", 8842, "Archer", 8789,
    "Somervell", 8743, "Yoakum", 8571, "Mitchell", 8558,
    "Coleman", 8391, "San Augustine", 8327, "Hamilton", 8269,
    "McCulloch", 8098, "Winkler", 7802, "Castro", 7787,
    "Goliad", 7531, "Swisher", 7484, "La Salle", 7409,
    "Dallam", 7243, "Refugio", 7236, "Childress", 7226,
    "Brooks", 7180, "Presidio", 7123, "Bailey", 7092,
    "Garza", 6288, "Carson", 6032, "San Saba", 5962,
    "Floyd", 5872, "Crosby", 5861, "Haskell", 5809,
    "Lynn", 5808, "Hartley", 5767, "Martin", 5614,
    "Hansford", 5547, "Wheeler", 5482, "Jim Hogg", 5282,
    "Delta", 5215, "Mills", 4902, "Crane", 4839,
    "Kimble", 4408, "Concho", 4233, "Mason", 4161,
    "Hudspeth", 4098, "Hemphill", 4061, "Hardeman", 3952,
    "Fisher", 3883, "Sutton", 3865, "Reagan", 3752,
    "Knox", 3733, "Kinney", 3675, "Upton", 3634,
    "Crockett", 3633, "Baylor", 3591, "Lipscomb", 3469,
    "Real", 3389, "Donley", 3387, "Shackelford", 3311,
    "Coke", 3275, "Hall", 3074, "Schleicher", 3061,
    "Sherman", 3058, "Collingsworth", 2996, "Cochran", 2904,
    "Culberson", 2241, "Jeff Davis", 2234, "Dickens", 2216,
    "Menard", 2123, "Oldham", 2090, "Edwards", 2055,
    "Armstrong", 1916, "Cottle", 1623, "Throckmorton", 1567,
    "Briscoe", 1546, "Irion", 1524, "Glasscock", 1430,
    "Foard", 1408, "Stonewall", 1385, "Motley", 1156,
    "Sterling", 1141, "Roberts", 885, "Terrell", 862,
    "Kent", 749, "Borden", 665, "McMullen", 662,
    "Kenedy", 595, "King", 228, "Loving", 102
)

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
            "Austin", 2058351, "Austin Metro Region")

DefineRegions <- tribble(
    ~Region, ~List,
    "Texas", c("Total"),
    "Houston-Galv", c("Harris", "Fort Bend", "Galveston", "Waller", "Montgomery", "Liberty", "Brazoria", "Chambers", "Austin"),
    "Dallas-Fort Worth", c("Collin", "Dallas", "Denton", "Ellis", "Hood", "Hunt", "Johnson", "Kaufman", "Parker", "Rockwall", "Somervell", "Tarrant", "Wise"),
    "San Antonio", c("Atascosa", "Bandera", "Bexar", "Comal", "Guadalupe", "Kendall", "Medina", "Wilson"), 
    "Austin", c("Bastrop", "Caldwell", "Hays", "Travis", "Williamson")
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

# Add current cases to county for labeling selector

Counties <- left_join(Counties, TodayData, by="County") %>% 
  select(County, Population=Population.x, Cases) %>% 
  replace_na(list(Cases=0))

MappingData <-  merge(Texas, TodayData,
                      by.x = c("County"), by.y = c("County"),
                      all.x = TRUE) 

# Build labels for map

MappingData <- MappingData %>%
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
              MappingData[i,]$DPerCap, " Deaths per 100,000"),
      "NA", "Zero"))
})

span <- function(vector){
  foo <- range(vector, na.rm=TRUE)
  return(max(foo) - min(foo))
}

##################################################
# Define UI for displaying data for Texas
##################################################
ui <- basicPage(
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
         tabPanel( ##########   Slope Change
                   "Slope Change",
                   fluid = TRUE,
                   value = "SlopeChange",
                   plotOutput("plot_slopes",
                              height = "700px")#,
                  # h4("Details on displayed data"),
                  # htmlOutput("death_details")
         ), # end tab panel Slope Change
         tabPanel( ##########   Missed Tests
                   "Missed Tests",
                   fluid = TRUE,
                   value = "Tests",
                   HTML("<hr>")
         ), # end tab panel MIssed Tests
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
 #                   checkboxInput(
 #                     inputId = "weights",
 #                     label = strong("Weight fit"),
 #                     value = FALSE
 #                   ),
                    #HTML("<hr>")
                   # checkboxInput(
                   #   inputId = "mult",
                   #   label = strong("Multiply Cases"),
                   #   value = FALSE
                   # ),
                   # numericInput("mult_pos", label = h5("Factor"), 
                   #              min = 1.,
                   #              max = 20,
                   #              value = 2)
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
                    #    Slope Change Tab
                    condition = "input.An_tabs == 'SlopeChange'",
                      wellPanel(
                          numericInput(
                              "window",
                              label = h5("Days to fit over"),
                              step = 2,
                              value = 3,
                              min=3,
                              max=15
                          )
                      ),
                      wellPanel(
                        radioButtons(
                          "slopetype",
                          label = h4("Y-Axis"),
                          choices = list(
                            "Cum Cases" = "cases",
                            "Doubling Time" = "doubling"
                          ),
                      selected = "cases"
                        ), 
                        checkboxInput(
                          inputId = "smooth",
                          label = strong("Smooth?"),
                          value = FALSE
                        ),
                      )
                  )
                 ) # end column Controls
         ) # end fluid page
                
     ), # end tabPanel Analysis
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
                                 "Deaths/Cases" = "deathpercase"
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
  #hideTab(inputId = "An_tabs", target="SlopeChange")   
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
                        in_area="Texas"
                        ) { 
    print(":::::::  prep_data")
    if (in_dataset=="Region") { # work with regions
      PopLabel <<- Regions %>% filter(Region==in_area)
      target <- unlist(DefineRegions$List[DefineRegions$Region==in_area])
      subdata <<- DF %>% 
          filter(County %in% target) %>% 
          group_by(Date) %>% 
          summarise(Cases=sum(Cases), 
                    Days=mean(Days), 
                    Deaths=sum(Deaths, na.rm=TRUE)) %>% 
          mutate(actual_deaths=Deaths-lag(Deaths, 1, 0)) %>%  
          mutate(Deaths=na_if(Deaths, 0)) 
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
                         mutate(actual_deaths=Deaths-lag(Deaths, 1, 0))  
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
          geom_point(data=TestingData,
                        aes(x=Date, y=Total/xform, color="tests"),
                     size=3, shape=21, fill="white") +
          geom_text(data=TestingData,
                    aes(x=Date, y=Total/xform, label=Total),
                    nudge_x=-1.50, nudge_y=0.0) +
          theme(text = element_text(size=20)) +
          labs(title=paste0("COVID-19 Cases in ",PopLabel$Label), 
               subtitle=paste0(" as of ", lastdate))
    #------------------
    #  Plot recovered estimate
    #------------------
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
        p <- p + geom_col(alpha = 2/3)  +
             geom_label(aes(label=Cases), 
                            stat='identity',
                            size = 3) 
     } else {
        
        p <- p + geom_point(aes(color="data"), size=2) +
                 geom_text(aes(label=Cases),
                           nudge_x=-1.50, nudge_y=0.0)
     }       

      
    #------------------
    #  Log scaling
    #------------------
      if (in_logscale) {
        trans_value <- "log10"
        min_limit <- min(subdata$Cases[1], 10)
      } else {
        trans_value <- "identity"
        min_limit <- 0
      }
    #------------------
    #  Zoom
    #------------------
    zoom_factor <- 6
    if (in_modeling=="logistic") {zoom_factor <- 4}
      if (in_zoom) { # bigger scale
          # limit height of modeled fit
        p <- p + scale_y_continuous(#limits=c(min_limit, 
                                    #         zoom_factor*max(subdata$Cases)),
                                    sec.axis = sec_axis(~.*xform, 
                                    name = "Statewide Test Total"),
                                    trans=trans_value) +
          coord_cartesian(ylim=c(min_limit, 
                                 zoom_factor*max(subdata$Cases))
                          )
      } else { # normal scale
        p <- p + scale_y_continuous(#limits=c(min_limit, max(1.2*case_fit$upper_conf)),
                                    sec.axis = sec_axis(~.*xform, 
                                    name = "Statewide Test Total"),
                                    trans=trans_value)+
          coord_cartesian(ylim=c(min_limit, 
                                 1.5*max(subdata$Cases))
                          )
      }
    #------------------
    #  Legend
    #------------------
      leg_labs <- c("Data", "Fit", "Tests", "Recovered", "Active Cases") # Labels for legend
      leg_vals <- c("black", "blue", "black", "red", "green") # Color values
      leg_brks <- c("data", "fit", "tests", "recovered", "active") # Breaks (named lists)
    #------------------
    #  Multiply cases
    #------------------
 #     if (in_mult) {
 #         p <- add_mult(p, subdata, in_mult_pos=in_mult_pos, in_weights = in_weights)
 #         leg_labs <- c(leg_labs, "Multiplied")
 #         leg_vals <- c(leg_vals, "red")
 #         leg_brks <- c(leg_brks, "mult")
 #     } 
    #------------------
    #  Estimate missed cases
    #------------------
    #  if (in_estmiss) {
    #      p <- add_estmiss(p, subdata)
    #      leg_labs <- c(leg_labs, "Missed\nCases")
    #      leg_vals <- c(leg_vals, "green")
    #      leg_brks <- c(leg_brks, "est_miss")
    #  } 
    #------------------
    #  Crowdsize
    #------------------
      if (in_avoid) {
          p <- add_crowdsize(p, in_zoom)
      }
      
      p <-  build_legend(p, "Cases",
                             leg_labs, # Labels for legend
                             leg_vals, # Color values
                             leg_brks # Breaks (named lists)
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
  #------------------- Add mult Plot ---------------
  #---------------------------------------------------    
 # add_mult <- function(p, subdata, in_mult_pos, in_weights) {
 #   print(":::::::  add_mult")
 #   #   Case with estimates of undercount
 #   subdata <- subdata %>% # update mult cases in case needed
 #     mutate(Estimate=Cases*replace_na(in_mult_pos,0.1))
 #   
 #   
 #   # Build an exponential model
 #   foo <- build_expmodel(subdata,
 #                         indep="Estimate",
 #                         in_weights=in_weights,
 #                         fit="all")
 #   
 #   Est_layer <-   geom_line(data=foo$Line[[1]],
 #                            aes(x=Date, y=Estimate,
 #                                color="mult"),
 #                            size=1,
 #                            linetype="dotted")
 #   p <- p + Est_layer 
 #   
 #   return(p)
 # }
  
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
  
  build_legend <- function(p, title, plabs, pvals, pbrks){
    print(":::::::  build_legend")
    
    #   Create list of named characters
    names(pvals) <- pbrks
    
    legend <- theme(legend.position=c( 0.12, 0.5 ))
      
    print(pvals)
    print(plabs)
    print(pbrks)
    Legend_layer <- scale_color_manual(name = title, 
                                         values = pvals,
                                         labels = plabs,
                                         breaks = pbrks
                                         )
    p <- p + legend + Legend_layer 
    
    return(p)
  }
  
  #---------------------------------------------------    
  #------------------- Add crowd size ----------------
  #---------------------------------------------------    
  # When is probability of 1% contact reached?
  add_crowdsize <- function(p, in_zoom) {
    print(":::::::  add_crowdsize")
      begin <- subdata$Date[1] # date of first reported case
      LastDate <- subdata[nrow(subdata),]$Date
      lastday <- as.integer(LastDate - begin) + 1  # last day of real data
      Population <- PopLabel[2][[1]]
      dayseq <- 0:(as.integer(LastDate - begin) + 10)
      dateseq <- as_date(begin:(LastDate + 10))
      Cases <- case_fit$Cases

      TestDays <- as.integer(LastDate 
                             - begin) + c(0,5,10) + 1
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

      y_anno <- 6*max(Cases)*0.9
      if (in_zoom) {
        y_anno <- Cases[length(Cases)]*0.9
      }
      
      p <- p + CrowdLayer1 + 
               CrowdLayer2 +
               annotate("label", label=CrowdText, 
                        x=begin+5, y=y_anno, 
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
      p <- p + geom_point(data=subdata, aes(y=Cases, x=Date, color="data"), size=2) +
               geom_text(data=subdata, aes(y=Cases, x=Date, label=Cases),
                         nudge_x=-1.50, nudge_y=0.0)
      
      p <-  build_legend(p, "Deaths",
                             c("Data", "Fit", "Est Cases"), # Labels for legend
                             c("black", "blue", "red"), # Color values
                             c("data", "fit", "est") # Breaks (named lists)
                             )
    } else {
      p <-  build_legend(p, "Deaths",
                             c("Data", "Fit"), # Labels for legend
                             c("black", "blue"), # Color values
                             c("data", "fit") # Breaks (named lists)
                             )
    }      
    #-------------------------------- Log scaling?
      if (in_Deaths_logscale) {
        trans_value <- "log10"
        min_limit <- min(death_fit$Deaths[1], 2)
      } else {
        trans_value <- "identity"
        min_limit <- 0
      }
      if (in_Deaths_zoom) {
          # limit height of modeled fit
        p <- p + scale_y_continuous(limits=c(min_limit, 6*ymax),
                                    trans=trans_value)
      } else {
        p <- p + scale_y_continuous(limits=c(min_limit, ymax),
                                    trans=trans_value)
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
    
    #m <- death_params[["m"]]
    #b <- death_params[["b"]] 
    #Rsqr <- death_params[["Rsqr"]]
    #output$death_details <- data_details(data,
    #                                     "Deaths",
    #                                     EqText,
    #                                     m,
    #                                     b,
    #                                     Rsqr)

    return(p)
  }
  
  #---------------------------------------------------    
  #------------------- Build Data Details Panel ------
  #---------------------------------------------------    
  data_details <- function(data, variable, EqText, m, b, rsqr) {
    renderUI({
      str1 <- paste("Most recent value, on",data$Date[nrow(data)],
                    "was<b>", data[nrow(data), variable],"</b>",variable)
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
                                in_smooth
                                ){
    halfwidth <- as.integer(in_window/2)
    
    if (in_slopetype=="cases") {
      foo <- subdata %>% 
        mutate(log_cases=Cases)
      my_title <- "Slope of Cum Case Count in "
      my_ylab <- "Slope: Change in Cum num cases / number of days"
    } else {
      foo <- subdata %>% 
        mutate(log_cases=log10(Cases))
      my_title <- "Doubling Time for "
      my_ylab <- "Doubling Time in Days"
    }
    
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
      rename(sd=std.error, m=estimate) %>% 
      filter(m>0.0)
    
    #   calculate doubling time
    if (in_slopetype=="doubling") {
      foo <- foo %>% 
        mutate(m=signif(log10(2)/m,2),
               sd=signif(log10(2)/(m-sd),2)) %>% 
        filter(m<200)
    }
    if (in_smooth) {
      foo$m <- fractal::medianFilter(foo$m,3)
    }
    
    foo %>% 
      ggplot(aes(x=Date, y=m)) +
      geom_point() +
      geom_errorbar(aes(ymax=m+sd, ymin=m-sd)) +
      geom_line() +
      theme(text = element_text(size=20)) +
      labs(title=paste0(my_title
                        ,PopLabel$Label),
           subtitle=paste0("Fit over ",in_window," days"),
           y=my_ylab)
    
    
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
    print(as.character(seq(DPerCRange[1], DPerCRange[2], length.out = 5)))
    
    print("Map --2--")
    #   Calculate proper number of quantile cuts
    nCase <- as.integer(sum(MappingData$Cases>0, na.rm=TRUE)/sum(MappingData$Cases==1, na.rm=TRUE))
    nDeath <- as.integer(sum(MappingData$Deaths>0, na.rm=TRUE)/sum(MappingData$Deaths==1, na.rm=TRUE))
    
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
    
    paldeathpercap <- colorQuantile(
                            na.color = "transparent",
                            palette = heat.colors(8),
                            n = 8, 
                            reverse=TRUE,
                            right = FALSE,
                            domain = MappingData$DPerCap)
    
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
                1}, { # Change data selection
    print(":::::::  observe_event Analysis Data")
                  
#  Set up geographic area desired and create data selection
    in_area <- case_when(
      input$dataset == "Region" ~ input$region,
      input$dataset == "County" ~ input$county
    )
    # ===============================
    prep_data(input$dataset,
              in_area
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
  #------------- Slope Change tab
  #---------------------------------
      if (input$An_tabs == "SlopeChange") {
        if (sum(!is.na(subdata$Cases))>15) {
          p <- build_slope_plot(
                                 input$window,
                                 input$slopetype,
                                 input$smooth
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
      if (input$An_tabs == "SlopeChange") {
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
                input$An_tabs
                1} , { # 
      if (input$An_tabs == "SlopeChange") {
          if (sum(!is.na(subdata$Cases))>15) {
            p <- build_slope_plot(
              input$window,
              input$slopetype,
              input$smooth
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

