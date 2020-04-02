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

# drop rows with zero cases

DF <- DF %>% filter(Cases>0)

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

DF <- DF %>% 
    mutate(Estimate=Cases)


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
    "Colorado", 21022, "Gonzales", 20667, "Lampasas and Llano", 20640,
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

MappingData <-  merge(Texas, TodayData,
                      by.x = c("County"), by.y = c("County"),
                      all.x = TRUE) 

# Build labels for map

MappingData <- MappingData %>%
  mutate(percapita=signif(percapita,3)) %>% 
  mutate(Deaths=na_if(Deaths, 0)) %>% 
  mutate(DPerC=na_if(signif(Deaths/Cases,2),0))

MapLabels <- lapply(seq(nrow(MappingData)), function(i) {
  htmltools::HTML(
    str_replace_all(
      paste0( MappingData[i,]$County, ' County<br>', 
              MappingData[i,]$Cases,' Cases Total<br>', 
              MappingData[i,]$percapita, " per 100,000<br>",
              MappingData[i,]$Deaths, " Deaths<br>",
              MappingData[i,]$DPerC, " Deaths per Case"),
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
    #    Graph, Map, Documentation
    tabsetPanel(id = "tabs",
                ##########   Graph Tab
                tabPanel(
                    "Graph",
                    fluid = TRUE,
                    value = "GraphTab",
                    fluidPage(
                        #-------------------- Plot
                        column(
                            9,
                            # Plot
                            plotOutput("plot_graph",
                                       height = "800px"),
                            h4("Details on displayed data"),
                            htmlOutput("data_details"),
                        ),
                        # end of column Plot
                        #-------------------- Controls
                        column(
                            3,
                            # Controls
                            #-------------------- Select Data
                            wellPanel(
                                # Select data to plot
                                h4("Choose the data"),
                                radioButtons(
                                    "dataset",
                                    label = strong("Which Data?"),
                                    choices = list("Region" = "Region",
                                                   "County" = "County"),
                                    selected = "Region",
                                    width = '90%',
                                    inline = TRUE
                                ),
                                conditionalPanel(
                                    #    Select Region
                                    condition = "input.dataset == 'Region'",
                                    selectInput("region", "Choose a Region:",
                                                Regions$Region,
                                                selected = "Texas")
                                ),
                                # end conditional panel
                                conditionalPanel(
                                    #    Select County
                                    condition = "input.dataset == 'County'",
                                    selectInput(
                                        "county",
                                        label = "Choose a County:",
                                        Counties$County,
                                        selected = "Harris"
                                    )
                                ),
                                # end conditional panel
                            ),
                            # end wellPanel select data to plot
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
                                    inputId = "estmiss",
                                    label = strong("Est missed cases"),
                                    value = FALSE
                                ),
                                checkboxInput(
                                    inputId = "logscale",
                                    label = strong("Log Scaling"),
                                    value = FALSE
                                ),
                                
                            ),
                            # end wellPanel Control plot options
                            #-------------------- Modeling parameters
                            wellPanel(
                                # Modeling parameters
                                h4("Data Fits"),
                                radioButtons(
                                    "modeling",
                                    label = h5("Exponential Fit"),
                                    choices = list(
                                        "Fit data" = "do fit",
                                        "Worldwide (0.13)" = "standard",
                                        "User entry" = "user"
                                    ),
                                    selected = "do fit"
                                ),
                                splitLayout(
                                  numericInput(
                                      "fit",
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
                                ),
                                checkboxInput(
                                    inputId = "weights",
                                    label = strong("Weight fit"),
                                    value = TRUE
                                ),
                                HTML("<hr>"),
                                checkboxInput(
                                    inputId = "mult",
                                    label = strong("Multiply Cases"),
                                    value = FALSE
                                ),
                                numericInput("mult_pos", label = h5("Factor"), 
                                             min = 1.,
                                             max = 20,
                                             value = 2),
                            ),
                            # end wellPanel Modeling parameters
                            
                        ) # end column Controls
                    )
                    
                ),
                # end tabPanel Graph
    ##########   Analysis Tab
    tabPanel( "Analysis", fluid = TRUE, value = "AnalysisTab",
        HTML("<hr>"),
        fluidPage(#-------------------- Analysis Tabs
        column( 9, # Tabs
     tabsetPanel(id = "An_tabs",
                ##########   Graph Tab
          tabPanel(
                    "Deaths",
                    fluid = TRUE,
                    value = "Deaths",
                    HTML("<hr>"),
                    plotOutput("plot_deaths",
                               height = "800px"),
                    h4("Details on displayed data"),
                    htmlOutput("death_details"),
                  ), # end tab panel Deaths
          tabPanel(
                    "Slope Change",
                    fluid = TRUE,
                    value = "SlopeChange",
                    HTML("<hr>"),
                  ), # end tab panel Deaths
          tabPanel(
                    "Missed Tests",
                    fluid = TRUE,
                    value = "Tests",
                    HTML("<hr>"),
                  ), # end tab panel Deaths
          tabPanel(
                    "Something",
                    fluid = TRUE,
                    value = "Something",
                    HTML("<hr>"),
                  ) # end tab panel Deaths
               ), # end TabSet panel An_tabs
             ), # end column 
            #-------------------- Data Selection
           column(3, # Controls
                   # Select data to plot
                   h4("Choose the data"),
                   radioButtons(
                     "An_dataset",
                     label = strong("Which Data?"),
                     choices = list("Region" = "Region",
                                    "County" = "County"),
                     selected = "Region",
                     width = '90%'
                   ),
                   conditionalPanel(
                     #    Select Region
                     condition = "input.An_dataset == 'Region'",
                     selectInput("An_region", "Choose a Region:",
                                 Regions$Region,
                                 selected = "Texas")
                   ), # end conditional panel
                   conditionalPanel(
                     #    Select County
                     condition = "input.An_dataset == 'County'",
                     selectInput(
                       "An_county",
                       label = "Choose a County:",
                       Counties$County,
                       selected = "Harris"
                     )
                   ), # end conditional panel
                  conditionalPanel(
                    #    Deaths Tab
                    condition = "input.An_tabs == 'Deaths'",
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
                    HTML("<hr>"),
                      checkboxInput(
                        inputId = "Deaths_back_est",
                        label = strong("Est Cases from Deaths"),
                        value = FALSE
                    ),
                      splitLayout(
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
                    HTML("<hr>"),
                    
                  ),
                  # end conditional panel
                  conditionalPanel(
                    #    Slope Change Tab
                    condition = "input.An_tabs == 'SlopeChange'",
                    selectInput(
                      "county",
                      label = "Choose a County:",
                      Counties$County,
                      selected = "Harris"
                    )
                  ),
                  # end conditional panel
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
                    HTML("<hr>"),
            ), # end of column Plot
            #-------------------- Controls
            column(3, # Controls
                   #    Select quantity to color counties with
                   radioButtons("county_color", 
                                label = strong("Display which variable?"),
                                choices = list( "Total Cases" = "casetotal", 
                                               "Cases per 100,000 population" = "percapita",
                                               "Deaths" = "deaths",
                                               "Deaths/Cases" = "deathpercase"
                                               ), 
                                selected = "casetotal",
                                width='90%',
                                inline=FALSE),
                   ) # end column control
         ) # end fluid page
                   
     ), # end tabPanel Map
    ##########   Documentation Tab
                           tabPanel("Documentation", fluid=TRUE, value="DocumentationTab",
                                    withMathJax(includeMarkdown("Documentation.Rmd")),
                                    HTML("<hr>"),

          )  # end tabPanel Documentation
        )  # end tabset 
    ) # end basic page

# Define server logic 
server <- function(input, output) {
    
#   Global variables are
#    PopLabel = list(Region, Population, Label)
#    subdata = tibble of data subsetted 
#    begin = date of first reported case
       
  #---------------------------------------------------    
  #------------------- Prep Data ---------------------
  #---------------------------------------------------    
  prep_data <- function(){ # return population, label for graph title and tibble subset
    print(":::::::  prep_data")
    if (input$dataset=="Region") { # work with regions
        print(paste("--1--", DefineRegions$List[DefineRegions$Region==input$region]))######################### print
      PopLabel <<- Regions %>% filter(Region==input$region)
      target <- unlist(DefineRegions$List[DefineRegions$Region==input$region])
      subdata <<- DF %>% 
          filter(County %in% target) %>% 
          group_by(Date) %>% 
          summarise(Cases=sum(Cases), Days=mean(Days), Estimate=sum(Estimate))
      print(paste("---1.5---", subdata, input$region))######################### print
      begin <<- subdata$Date[1] # date of first reported case
      return()
    } else {
        print(paste("--2--", input$county))######################### print
      #   Is there any data?
      if (! input$county %in% DF$County) {
        showNotification(paste("No reported cases in", input$county))
        return()
      }
      
      PopLabel <<- Counties %>% filter(County==input$county) %>% 
                     mutate(Label=paste(input$county, "County"))
      subdata <<- DF %>% filter(County==input$county)
      begin <<- subdata$Date[1] # date of first reported case
      return()
    }
  } # end prep_data
    
  #---------------------------------------------------    
  #------------------- Build Model -------------------
  #---------------------------------------------------    
  #build_model <- function(subdata, weight_flag){ 
  #  print(":::::::  build_model")
  #    # Linear fits to log(cases)
  #  ##########   Base case with actual data  
  #  weights <- (1:nrow(subdata))**1.5
  #  if (weight_flag) {
  #    LogFits <- lm(log10(Cases)~Days, data=subdata, weights=weights)
  #  } else {
  #    LogFits <- lm(log10(Cases)~Days, data=subdata)
  #  }
  #  m <- LogFits[["coefficients"]][["Days"]]
  #  b <- LogFits[["coefficients"]][["(Intercept)"]]
  #  Rsqr <- summary(LogFits)$adj.r.squared
  #  std_dev <- sigma(LogFits)
  #  print(paste("--3--", m, b, Rsqr, std_dev))  ######################### print
  #  # return a tibble
  #  tribble(~m, ~b, ~Rsqr, ~std_dev,
  #           m,  b,  Rsqr,  std_dev)
  #}
      
  #build_est_model <- function(){ 
  #  print(":::::::  build_est_model")
      # Linear fits to log(cases)
    ##########   Case with estimates of undercount
  #  subdata <<- subdata %>% # update mult cases in case needed
  #               mutate(Estimate=Cases*replace_na(input$mult_pos,0.1))
  #  weights <- (1:nrow(subdata))**1.5
  #  if (input$weights) {
  #    LogFits <- lm(log10(Estimate)~Days, data=subdata, weights=weights)
  #  } else {
  #    LogFits <- lm(log10(Estimate)~Days, data=subdata)
  #  }
  #  #LogFits <- lm(log10(Estimate)~Days, data=subdata)
  #  m <- LogFits[["coefficients"]][["Days"]]
  #  b <- LogFits[["coefficients"]][["(Intercept)"]]
  #  Rsqr <- summary(LogFits)$adj.r.squared
  #  std_dev <- sigma(LogFits)
  #  print(paste("--3.9--", m, b))  ######################### print
  #  # return a tibble
  #  tribble(~m, ~b, ~Rsqr, ~std_dev,
  #           m,  b,  Rsqr,  std_dev)
  #}
  #---------------------------------------------------    
  #---------------New  Build Model -------------------
  #---------------------------------------------------    
  ####################   not used
  #new_build_model <- function(x, y, my_formula, weights){ 
  #  print(":::::::  new_build_model")
  ## Linear fits to log(cases)
#
#    LogFits <- lm(my_formula, weights=weights)
#    m <- LogFits[["coefficients"]][["x"]]
#    b <- LogFits[["coefficients"]][["(Intercept)"]]
#    Rsqr <- summary(LogFits)$adj.r.squared
#    std_dev <- sigma(LogFits)
#    print(paste("--A3--", m, b, Rsqr, std_dev))  ######################### print
#    # return a tibble
#    tribble(~m, ~b, ~Rsqr, ~std_dev,
#             m,  b,  Rsqr,  std_dev)
#  }
    
  #---------------------------------------------------    
  #------------------- Build exponential curve -------
  #---------------------------------------------------    
    #build_expline <- function(crv=c('real', 'est')){
    #print(":::::::  build_expline")
    #  #   Go 10 days into future
    #  lastday <- as.integer(LastDate - begin) + 1 # last day of real data
    #  dayseq <- 0:(lastday + 9)
    #  dateseq <- as_date(begin:(LastDate+10))
    #  model <- build_model(subdata, input$weights)
    #  est_model <- build_est_model()
    #  m <- model$m
    #  b <- model$b
    #  Rsqr <- model$Rsqr
    #  m_est <- est_model$m
    #  b_est <- est_model$b
    #  print(paste("----build_expline----", crv, model))
    #  if (input$modeling=="do fit") {
    #    Cases <- case_when(
    #      crv=="real" ~  10**(m*dayseq+b),
    #      crv=="est"  ~  10**(m_est*dayseq+b_est)
    #    )
    #  } else if (input$modeling=="user") {
    #    m <- input$fit
    #    b <- input$intercept
    #    Cases <- case_when (
    #      crv=="real" ~  10**(m*dayseq+b),
    #      crv=="est"  ~  10**(m*dayseq+b)*replace_na(input$mult_pos, 0.1)
    #    )
    #  } else {
    #    m <- global_slope
    #    b <- case_when(
    #      crv=="real" ~  log10(subdata$Cases[lastday]) - m*(lastday-1),
    #      crv=="est"  ~  log10(subdata$Estimate[lastday]) - m*(lastday-1)
    #    )
    #    Cases <- 10**(m*dayseq+b)
    #  }
    #  SD_upper <- Cases+Cases*model$std_dev
    #  SD_lower <- Cases-Cases*model$std_dev
    #  ExpLine <- tibble( Days=dayseq, Date=dateseq, Cases=Cases,
    #                     SD_upper=SD_upper, SD_lower=SD_lower)
      #print(paste("--5--", ExpLine))######################### print
      #  return a tibble
    #  tribble(~Line, ~m, ~b, ~Rsqr,
    #           ExpLine, m, b, Rsqr)
    #}  
  
  #---------------------------------------------------    
  #-----------Build an exponential model -------------
  #---------------------------------------------------    
  build_expmodel <- function(data, 
                             indep="Cases", # independent variable
                             in_weights, 
                             fit=c('all', 'none', "b_only", "m_only"),
                             m=1.3,
                             b=1){
    print(":::::::  build_expmodel")
      #   Go 10 days into future
      lastday <- as.integer(LastDate - begin) + 1 # last day of real data
      dayseq <- 0:(lastday + 9)
      dateseq <- as_date(begin:(LastDate+10))
      x <- data$Days
      y <- data[,indep][[1]]
      if (in_weights) {
        weights <- (1:nrow(subdata))**2.5
      } else {
        weights <- replicate(nrow(subdata), 1)
      }
      my_data <- tibble(x=x, y=y, weights=weights)
      print("my_data")
      print(my_data)
        
      if (fit=="all") { 
        model <- lm(log10(y)~x, data=my_data, weights=weights)
        m <- model[["coefficients"]][["x"]]
        b <- model[["coefficients"]][["(Intercept)"]]
        Rsqr <- summary(model)$adj.r.squared
        std_dev <- sigma(model)
      } else if (fit=="none") {
        m <- m
        b <- b
        Rsqr <- 1
        std_dev <- 0
      } else if (fit=="b_only") {
        #model <- lm(log10(y)-m*x~1, weights=weights)
        #m <- model[["coefficients"]][["x"]]
        #b <- model[["coefficients"]][["(Intercept)"]]
        b <- log10(data$Cases[lastday]) - m*(lastday-1)
        #Rsqr <- summary(model)$adj.r.squared
        #std_dev <- sigma(model)
        Rsqr <- 1
        std_dev <- 0
        print(paste("----build_expline-2--", model))
      } else if (fit=="m_only") {
        model <- lm(I(x - b) ~ 0 + log10(y), weights=weights)
        m <- model[["coefficients"]][["x"]]
        b <- model[["coefficients"]][["(Intercept)"]]
        Rsqr <- summary(model)$adj.r.squared
        std_dev <- sigma(model)
        print(paste("----build_expline-3--", model))
      }
  
      print(paste("m and b",m,b))
      Cases <- 10**(m*dayseq+b)
      SD_upper <- Cases+Cases*std_dev
      SD_lower <- Cases-Cases*std_dev
      
      ExpLine <- tibble( Days=dayseq, Date=dateseq,!!indep:=Cases,
                         SD_upper=SD_upper, SD_lower=SD_lower) 
      print(paste("--5--"))######################### print
      print(ExpLine)######################### print
      #  return a tibble
      tribble(~Line, ~m, ~b, ~Rsqr,
               ExpLine, m, b, Rsqr)
    }  
  
  #---------------------------------------------------    
  #------------------- Build Basic Plot --------------
  #---------------------------------------------------    
  
  build_basic_plot <- function(in_modeling=c("do fit", "standard", "user"), 
                               in_fit,
                               in_intercept,
                               in_weights,
                               in_logscale,
                               in_zoom,
                               in_mult,
                               in_mult_pos,
                               in_estmiss,
                               in_avoid
    ){
      # Build exponential line for plot
    print(":::::::  build_basic_plot")
#    if (in_weights) {
#      weights <- (1:nrow(subdata))**1.5
#    } else {
#      weights <- replicate(nrow(subdata), 1)
#    }
    # Build an exponential model
    if (in_modeling == "do fit") { # full fit
      foo <- build_expmodel(subdata,
                            indep="Cases",
                            in_weights=in_weights,
                            fit="all")
    } else if (in_modeling == "standard") { # global standard
      foo <- build_expmodel(subdata,
                            indep="Cases",
                            in_weights=in_weights,
                            fit="b_only",
                            m=global_slope)
    } else { # user
      foo <- build_expmodel(subdata,
                            indep="Cases",
                            in_weights=in_weights,
                            fit="none",
                            m=in_fit,
                            b=in_intercept)
    }
      
    #foo <- build_expline("real")
    EqText <- paste0("Fit is log(Cases) = ",
                     signif(foo$m,3),"*Days + ",
                     signif(foo$b,3))
    ExpLine <- foo$Line[[1]]
    xform <- 2*signif(max(TestingData$Total)/(6*max(subdata$Cases)),2)
    #xform <- 10.0 # transform for secondary axis. Multiply primary by this
    #grob <- grid::grid.text(EqText, x=0.7,  y=0.0, gp=grid::gpar(col="black", fontsize=15))
      p <- subdata %>% 
          ggplot(aes(x=Date, y=Cases)) +
          geom_col(alpha = 2/3) +
          geom_label(aes(label=Cases), stat='identity', size = 3) +
          expand_limits(x = LastDate+10) +
          geom_line(data=ExpLine,
                    aes(x=Date, y=Cases,
                        color="fit"),
                    size=1,
                    linetype="dashed") +
          geom_line(data=ExpLine[1:(nrow(ExpLine)-10),],
                    aes(x=Date, y=Cases,
                        color="fit" ),
                    size=1,
                    linetype="solid") +
          geom_point(data=ExpLine[(nrow(ExpLine)-9):nrow(ExpLine),],
                        aes(x=Date, y=Cases),
                     shape=20, size=2, fill="blue") +
          geom_point(data=TestingData,
                        aes(x=Date, y=Total/xform, color="tests"),
                     size=3, shape=23, fill="black") +
          geom_text(data=TestingData,
                    aes(x=Date, y=Total/xform, label=Total),
                    nudge_x=-1.00, nudge_y=0.0) +
          theme(text = element_text(size=20)) +
          labs(title=paste0("COVID-19 Cases in ",PopLabel$Label), 
               subtitle=paste0(" as of ", lastdate))
      
      #p <- p + annotate("label", 
      #                  x=(LastDate - begin + 10)/1.5+begin, 
      #                  y=subdata$Cases[nrow(subdata)]/3, 
      #                  label=EqText)
      
      if (!is.nan(ExpLine$SD_lower[1]) & in_modeling=="do fit"){
           p <- p + geom_errorbar(data=ExpLine[(nrow(ExpLine)-9):nrow(ExpLine),],
                        aes(x=Date, y=Cases, ymin=SD_lower, ymax=SD_upper)) 
      }
      
      if (in_logscale) {
        trans_value <- "log10"
        min_limit <- min(subdata$Cases[1], 10)
      } else {
        trans_value <- "identity"
        min_limit <- 0
      }
      if (!in_zoom) {
          # limit height of modeled fit
        p <- p + scale_y_continuous(limits=c(min_limit, 6*max(subdata$Cases)),
                                    sec.axis = sec_axis(~.*xform, 
                                    name = "Statewide Test Total"),
                                    trans=trans_value)
      } else {
        p <- p + scale_y_continuous(limits=c(min_limit, max(ExpLine$Cases)),
                                    sec.axis = sec_axis(~.*xform, 
                                    name = "Statewide Test Total"),
                                    trans=trans_value)
      }
      leg_labs <- c("Data", "Tests")
      leg_vals <- c("blue", "black")
      leg_brks <- c("fit", "tests")
      if (in_mult) {
          p <- add_mult(p, in_mult_pos=in_mult_pos, in_weights = in_weights)
          leg_labs <- c(leg_labs, "Multiplied")
          leg_vals <- c(leg_vals, "red")
          leg_brks <- c(leg_brks, "mult")
      } 
      if (in_estmiss) {
          p <- add_estmiss(p)
          leg_labs <- c(leg_labs, "Missed\nCases")
          leg_vals <- c(leg_vals, "green")
          leg_brks <- c(leg_brks, "est_miss")
      } 
      if (in_avoid) {
        if (in_logscale) {
          showNotification("Crowdsize not available with log scale")
        } else {
          p <- add_crowdsize(p, in_mult, in_mult_pos, in_weights)
        }
      }
      p <-  build_legend(p, "Cases",
                             leg_labs, # Labels for legend
                             leg_vals, # Color values
                             leg_brks # Breaks (named lists)
      )
      m <- foo$m
      b <- foo$b 
      Rsqr <- foo$Rsqr
      output$data_details <- data_details(subdata,
                                           "Cases",
                                           EqText,
                                           m,
                                           b,
                                           Rsqr)
      
    return(p)
  }
  

  #---------------------------------------------------    
  #------------------- Add mult Plot ---------------
  #---------------------------------------------------    
  add_mult <- function(p, in_mult_pos=2, in_weights) {
    print(":::::::  add_mult")
    #   Case with estimates of undercount
    subdata <<- subdata %>% # update mult cases in case needed
      mutate(Estimate=Cases*replace_na(in_mult_pos,0.1))
    
    # Build an exponential model
    #weights <- replicate(nrow(subdata), 1)
    foo <- build_expmodel(subdata,
                          indep="Estimate",
                          in_weights=in_weights,
                          fit="all")
    
    Est_layer <-   geom_line(data=foo$Line[[1]],
                             aes(x=Date, y=Estimate,
                                 color="mult"),
                             size=1,
                             linetype="dotted")
    p <- p + Est_layer 
    
    return(p)
  }
  
  #---------------------------------------------------    
  #------------------- Add Estimate of missed cases --
  #---------------------------------------------------    
  # use worldwide slope, derive b from last number of cases
  add_estmiss <- function(p) {
    print(":::::::  add_estmiss")
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
      
    Legend_layer <- scale_color_manual(name = title, 
                                         values = pvals,
                                         labels = plabs,
                                         breaks = pbrks)
    p <- p + legend + Legend_layer 
    
    return(p)
  }
  
  #---------------------------------------------------    
  #------------------- Add crowd size ----------------
  #---------------------------------------------------    
  # When is probability of 1% contact reached?
  add_crowdsize <- function(p, in_mult, in_mult_pos, in_weights) {
    print(":::::::  add_crowdsize")
      Population <- PopLabel[2][[1]]

      #foo <- build_expline("real")     # Build an exponential model
      #weights <- replicate(nrow(subdata), 1)
      foo <- build_expmodel(subdata,
                            indep="Cases",
                            in_weights=in_weights,
                            fit="all")
      ExpLine <- foo$Line[[1]]
      print("---------- crowd 1")
      print(ExpLine)
      m <- foo$m
      b <- foo$b
      #foo <- build_expline("est")     # Build an exponential model
      #weights <- replicate(nrow(subdata), 1)
      foo <- build_expmodel(subdata,
                            indep="Estimate",
                            in_weights=in_weights,
                            fit="all")
      ExpLine_est <- foo$Line[[1]]
      print("---------- crowd 2")
      print(ExpLine_est)
      m_est <- foo$m 
      b_est <- foo$b  
      
      TestDays <- as.integer(LastDate 
                             - begin) + c(0,5,10) + 1
      TestDates <- LastDate + c(0,5,10)
      Crowdsize <- signif((0.01*Population)/(10**(TestDays*m+b)), 2)
      print(paste("--->>> m, b", m, b))
      Crowdsize_est <- signif((0.01*Population)/(10**(TestDays*m_est+b_est)
                                                 *replace_na(in_mult_pos,0.1)), 2)
      
      dayseq <- 0:(as.integer(LastDate - begin) + 10)
      dateseq <- as_date(begin:(LastDate + 10))
      print("---------- crowd 3")
      Cases <- ExpLine$Cases
      Cases_est <- ExpLine_est$Estimate
      print(Cases)
      print(Cases_est)
      print("---------- crowd 4")
      #  Scale cases so similar scaling to days
      CaseScale <- length(dateseq)/max(Cases)
      Delta <- (Cases[TestDays] - Cases[TestDays-1])*CaseScale
      r <-  2 # radius distance
      x_nudge <- -r*cos((pi/2 -atan(Delta)))- 0.5
      y_nudge <- r*sin((pi/2 -atan(Delta)))/CaseScale
      y_extra <- max(ExpLine$Cases)/30
      y_extra <- 1
      
      print("---------- crowd 5")
      # Build label tibble
      CrowdLabels <- tibble(Date=TestDates,
                            Crowd=Crowdsize,
                            Cases=Cases[TestDays],
                            Delta=Delta,
                            x_nudge=x_nudge,
                            y_nudge=y_nudge)
      
      print("---------- crowd 6")
      CrowdLabels_est <- tibble(Date=TestDates,
                                Crowd=Crowdsize_est,
                                Cases=Cases_est[TestDays],
                                Delta=Delta,
                                x_nudge=x_nudge,
                                y_nudge=y_nudge)
      
      print("---------- crowd 7")
      print(paste("===Crowds:", CrowdLabels))
      CrowdLayer1 <-  geom_point(data=CrowdLabels,
                                 aes(x=Date, y=Cases)) 
      CrowdLayer2 <- geom_label(data=CrowdLabels,
                                aes(x=Date, y=Cases, label=Crowd),
                                nudge_x=x_nudge,
                                nudge_y=y_nudge) 
      CrowdLayer3 <- geom_segment(data=CrowdLabels, 
                                  aes(x = Date+x_nudge+.5, 
                                      xend = Date,
                                      y = Cases + y_nudge - y_extra,
                                      yend = Cases), 
                                  colour = "black", 
                                  size=0.5) 
      
      CrowdLayerest1 <-  geom_point(data=CrowdLabels_est,
                                    aes(x=Date, y=Cases)) 
      CrowdLayerest2 <- geom_label(data=CrowdLabels_est,
                                   aes(x=Date, y=Cases, label=Crowd),
                                   nudge_x=x_nudge,
                                   nudge_y=y_nudge) 
      CrowdLayerest3 <- geom_segment(data=CrowdLabels_est, 
                                     aes(x = Date+x_nudge+.5, 
                                         xend = Date,
                                         y = Cases + y_nudge - y_extra,
                                         yend = Cases), 
                                     colour = "black", 
                                     size=0.5)
      
#   Crowdsize text

CrowdText <- "Sizes of groups to avoid to keep\nchance of meeting a contagious\nperson below 1%"
grob2 <- grid::grid.text(CrowdText, x=0.3,  y=0.8, gp=grid::gpar(col="black", fontsize=16))

      p <- p + CrowdLayer1 + 
               CrowdLayer2 + 
               CrowdLayer3 +
               annotation_custom(grob2)
      
      if (in_mult) { # Add for fit and estimate 
      print("---------- crowd 8")
          return(p + CrowdLayerest1 + CrowdLayerest2 + CrowdLayerest3)
      } else { # Add for fit only
          return(p)
      }
  }
  

  ################################## Analysis
  
       
  #---------------------------------------------------    
  #------------------- Prep Analysis Data ------------
  #---------------------------------------------------    
  prep_An_data <- function(){ # return population, label for graph title and tibble subset
    print(":::::::  prep_An_data")
    if (input$An_dataset=="Region") { # work with regions
        print(paste("--A1--", DefineRegions$List[DefineRegions$Region==input$An_region]))######################### print
      An_PopLabel <<- Regions %>% filter(Region==input$An_region)
      target <- unlist(DefineRegions$List[DefineRegions$Region==input$An_region])
      print(DF)
      #browser()
      An_subdata <<- DF %>% 
          filter(County %in% target) %>% 
          group_by(Date) %>% 
          summarise(Cases=sum(Cases), 
                    Days=mean(Days), 
                    Estimate=sum(Estimate),
                    Deaths=sum(Deaths, na.rm=TRUE)) %>% 
          #filter(Cases>0) %>% 
          mutate(Deaths=na_if(Deaths, 0))
      #An_subdata$Days <- seq(0:(nrow(An_subdata)-1))
      print(paste("---A1.5---", An_subdata, input$An_region))######################### print
      An_begin <<- An_subdata$Date[1] # date of first reported case
      return()
    } else {
        print(paste("--A2--", input$An_county))######################### print
      #   Is there any data?
      if (! input$An_county %in% DF$County) {
        showNotification(paste("No reported cases in", input$An_county))
        return()
      }
      
      An_PopLabel <<- Counties %>% filter(County==input$An_county) %>% 
                     mutate(Label=paste(input$An_county, "County"))
      An_subdata <<- DF %>% filter(County==input$An_county) %>% 
          filter(Cases>0)
#      An_subdata$Days <- seq(0:(nrow(An_subdata)-1))
#      An_begin <<- An_subdata$Date[1] # date of first reported case
      return()
    }
  } # end prep_data
    
  #---------------------------------------------------    
  #------------------- Build Death Plot --------------
  #---------------------------------------------------    
  
  build_deaths_plot <- function(in_weights=FALSE){
      # Build exponential line for plot
    print(":::::::  build_death_plot")
    print(An_subdata)
    
    #   Set no weighting
    #weights <- replicate(nrow(An_subdata), 1)
    foo <- build_expmodel(An_subdata,
                          indep="Deaths",
                          in_weights=in_weights,
                          fit="all")
    print("build_deaths_plot --- 1")
    EqText <- paste0("Fit is log(Cumulative Deaths) = ",
                     signif(foo$m,3),"*Days + ",
                     signif(foo$b,3))
    print("build_deaths_plot --- 2")
    ExpLine <- foo$Line[[1]]
    print(summary(ExpLine))
    print("build_deaths_plot --- 3")
    # Build Est Cases from Deaths
    
    
    p <- An_subdata %>% 
        ggplot(aes(x=Date, y=Deaths)) 
    
    p <- p + 
      expand_limits(x = LastDate+10) +
      geom_line(data=ExpLine,
                aes(x=Date, y=Deaths,
                    color="fit"),
                size=1,
                linetype="dashed") +
      geom_line(data=ExpLine[1:(nrow(ExpLine)-10),],
                aes(x=Date, y=Deaths,
                    color="fit" ),
                size=1,
                linetype="solid") +
      geom_point(data=ExpLine[(nrow(ExpLine)-9):nrow(ExpLine),],
                    aes(x=Date, y=Deaths),
                 shape=20, size=2, fill="blue") 
    if (!input$Deaths_logscale) {
        p <- p + geom_col(alpha = 2/3)  +
             geom_label(aes(label=Deaths), 
                            stat='identity',
                            size = 3) 
     } else {
        
        p <- p + geom_point(aes(color="data"), size=2) 
     } 
    print("build_deaths_plot --- 4")
      p <- p + geom_label(data=ExpLine[(nrow(ExpLine)-9):nrow(ExpLine),],
                  aes(label=as.integer(Deaths+.5)),
                  hjust=1, vjust=0) +
          #geom_line(data=ExpLine,
          #          aes(x=Date-13, y=Deaths*60,
          #              color="tests"),
          #          size=1,
          #          linetype="dashed") +
          #geom_point(data=TestingData,
          #              aes(x=Date, y=Total/xform, color="tests"),
          #           size=3, shape=23, fill="black") +
          #geom_text(data=TestingData,
          #          aes(x=Date, y=Total/xform, label=Total),
          #          nudge_x=-1.00, nudge_y=0.0) +
          theme(text = element_text(size=20)) +
          labs(title=paste0("COVID-19 Deaths in ",PopLabel$Label), 
               subtitle=paste0(" as of ", lastdate))
      
      #p <- p + annotate("label", 
      #                  x=(LastDate - begin + 10)/1.5+begin, 
      #                  #y=An_subdata$Deaths[nrow(An_subdata)]/5, 
      #                  y=0,
      #                  label=EqText)
      
      #if (!is.nan(ExpLine$SD_lower[1]) & input$modeling=="do fit"){
      #     p <- p + geom_errorbar(data=ExpLine[(nrow(ExpLine)-9):nrow(ExpLine),],
      #                  aes(x=Date, y=Deaths, ymin=SD_lower, ymax=SD_upper)) 
      #}
      
     # if (input$Deaths_logscale) {
     #   trans_value <- "log10"
     #   min_limit <- min(ExpLine$Deaths[1], 2)
     # } else {
     #   trans_value <- "identity"
     #   min_limit <- 0
     # }
     # if (input$Deaths_zoom) {
     #     # limit height of modeled fit
     #   p <- p + scale_y_continuous(limits=c(min_limit, 6*max(ExpLine$Deaths)),
     #                               #sec.axis = sec_axis(~.*xform, 
     #                               #name = "Statewide Test Total"),
     #                               trans=trans_value)
     # } else {
     #   p <- p + scale_y_continuous(limits=c(min_limit, max(ExpLine$Deaths)),
     #                               #sec.axis = sec_axis(~.*xform, 
     #                               #name = "Statewide Test Total"),
     #                               trans=trans_value)
     # }
  
    
    #-------------------------------- back estimate?
    print("build_deaths_plot --- 5")
    if (input$Deaths_back_est) {#  Build a model for the number of cases from deaths
      print(ExpLine)
      begin <- min(which(as.logical(An_subdata$Deaths)))
      dateseq <- as_date((An_subdata$Date[begin]-input$An_DeathLag):(LastDate+10))
      dayseq <- 0:(length(dateseq)-1)
      est_cases <- 10**(m*dayseq+b)/(input$An_CFR/100)
      ExpLine <- tibble( Days=dayseq, Date=dateseq, est_cases=est_cases)
      #ExpLine <- ExpLine %>% 
       # mutate(est_cases=Deaths/(input$An_CFR/100))
      print(ExpLine)
      p <- p + geom_line(data=ExpLine,
                    aes(x=Date, y=est_cases,
                        color="est"),
                    size=1,
                    linetype="dashed") 
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
      if (input$Deaths_logscale) {
        trans_value <- "log10"
        min_limit <- min(ExpLine$Deaths[1], 2)
      } else {
        trans_value <- "identity"
        min_limit <- 0
      }
      if (input$Deaths_zoom) {
          # limit height of modeled fit
        p <- p + scale_y_continuous(limits=c(min_limit, 6*max(ExpLine$Deaths)),
                                    #sec.axis = sec_axis(~.*xform, 
                                    #name = "Statewide Test Total"),
                                    trans=trans_value)
      } else {
        p <- p + scale_y_continuous(limits=c(min_limit, max(ExpLine$Deaths)),
                                    #sec.axis = sec_axis(~.*xform, 
                                    #name = "Statewide Test Total"),
                                    trans=trans_value)
      }
  
    
    print(":::::::  displayed_data")
    m <- foo$m
    b <- foo$b 
    Rsqr <- foo$Rsqr
    output$death_details <- data_details(An_subdata,
                                         "Deaths",
                                         EqText,
                                         m,
                                         b,
                                         Rsqr)

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
      
      if (!is.nan(Rsqr)){
        if (Rsqr>.8) {
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
  
  ######################  Map ########################
  #---------------------------------------------------    
  #------------------- Build Model -------------------
  #---------------------------------------------------    
  
  draw_map <- function() {
    
    print("Map --1--")
    Range <- range(MappingData$percapita, na.rm=TRUE)
    DPerCRange <- range(MappingData$DPerC, na.rm=TRUE)
    print(as.character(seq(DPerCRange[1], DPerCRange[2], length.out = 5)))
    
    print("Map --2--")
    palcap <-colorQuantile(palette = heat.colors(8), 
                           domain = MappingData$percapita, 
                           n = 8, 
                           na.color = "transparent", 
                           alpha = FALSE, 
                           reverse = TRUE,
                           right = FALSE) 
    
    print("Map --3--")
    palcase <- colorNumeric(
                            na.color = "transparent",
                            palette = heat.colors(8),
                            reverse=TRUE,
                            domain = MappingData$Cases)
    
    print("Map --4--")
    paldeath <- colorNumeric(
                            na.color = "transparent",
                            palette = heat.colors(8),
                            reverse=TRUE,
                            domain = MappingData$Deaths)
    
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
      output$TexasMap <- renderLeaflet({
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
                    opacity = 1)
      }) 
    } else if (input$county_color=="percapita") {
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
    } else if (input$county_color=="deaths")  { #  Deaths
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
    } else { # deathpercase
      output$TexasMap <- renderLeaflet({
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
   
  #------------------- Reactive bits ---------------------

  observeEvent(input$tabs, { # do stuff when tab changes
    print(paste("tab:", input$tabs))  
    if (input$tabs=="MapTab") { ##  Graph Tab ##
      draw_map()
    }
    if (input$tabs=="AnalysisTab") { ## Analysis Tab ##
      prep_An_data()
    }
  })
    
  #---------------------------------------------------    
  #------------------- Select Data -------------------
  #---------------------------------------------------    
  observeEvent({input$dataset
                input$region
                input$county
                1}, { # Change data selection
    print(":::::::  observe_event 1")
      prep_data()
      #foo <- build_expline("real")
      p <- build_basic_plot(input$modeling,
                            input$fit,
                            input$intercept,
                            input$weights,
                            input$logscale,
                            input$zoom,
                            input$mult,
                            input$mult_pos,
                            input$estmiss,
                            input$avoid)

      output$plot_graph <- renderPlot({
          print(p)
          })
      #displayed_data(foo$m, foo$Rsqr)
  })
   
  #---------------------------------------------------    
  #------------------- Select Analysis Data ----------
  #---------------------------------------------------    
  observeEvent({
                input$An_dataset
                input$An_region
                input$An_county
                input$An_tabs
                1}, { # Change data selection
    print(":::::::  observe_event Analysis Data")
                  
      prep_An_data()
      
      if (input$An_tabs == "Deaths") {
        if ((sum(!is.na(An_subdata$Deaths))>2) &
            (span(An_subdata$Deaths)>0)) {
          p <- build_deaths_plot(in_weights=FALSE)
          output$plot_deaths <- renderPlot({print(p)})
        } else {
          showNotification("Too little death data")
        }
      }
      if (input$An_tabs == "SlopeChange") {
         # p <- build_slope_plot()
      } 
      if (input$An_tabs == "Tests") {
         # p <- build_tests_plot()
      } 
      if (input$An_tabs == "Something") {
         # p <- build_something()
      }
      
     # displayed_data(foo$m, foo$Rsqr)
  })
   
  #---------------------------------------------------    
  #------------------- Analysis ----------------------
  #---------------------------------------------------    
  observeEvent({
                input$An_dataset
                input$An_region
                input$An_county
                input$Deaths_logscale
                input$Deaths_zoom
                input$Deaths_back_est
                input$An_CFR
                input$An_DeathLag
                1}, { # Change data selection
    print(":::::::  observe_event Analysis Data")
                  
      prep_An_data()
      
      if (input$An_tabs == "Deaths") {
        if ((sum(!is.na(An_subdata$Deaths))>2) &
            (span(An_subdata$Deaths)>0)) {
          p <- build_deaths_plot()
          output$plot_deaths <- renderPlot({print(p)})
        } else {
          showNotification("Too little death data")
        }
      }
      if (input$An_tabs == "SlopeChange") {
         # p <- build_slope_plot()
      } 
      if (input$An_tabs == "Tests") {
         # p <- build_tests_plot()
      } 
      if (input$An_tabs == "Something") {
         # p <- build_something()
      }
      
     # displayed_data(foo$m, foo$Rsqr)
  })

  #---------------------------------------------------    
  #------------------- Fiddle with Model -------------
  #---------------------------------------------------    
  observeEvent({input$modeling
                input$fit
                input$intercept
                input$mult
                input$mult_pos
                input$zoom
                input$avoid
                input$estmiss
                input$logscale
                input$weights
                1} , { # 
                  
    print(":::::::  observe_event 2")
      #foo <- build_expline("real")
                  
      p <- build_basic_plot(input$modeling,
                            input$fit,
                            input$intercept,
                            input$weights,
                            input$logscale,
                            input$zoom,
                            input$mult,
                            input$mult_pos,
                            input$estmiss,
                            input$avoid)
      
      output$plot_graph <- renderPlot({
          print(p)
          })
      #displayed_data(foo$m, foo$Rsqr)
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
