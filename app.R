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
                  summarise(Cases = sum(Cases)) %>% 
                  mutate(County="Total")
                 ) %>% 
    arrange(Date)

# Calc days since March 11

DF <- DF %>% 
    mutate(Days=as.integer(Date-ymd("2020-03-11")))

DeathData <- DeathData %>% 
    mutate(Days=as.integer(Date-ymd("2020-03-11")))

# Fix Deaths field

DF$Deaths <- str_replace(DF$Deaths,"-", "na")

DF <- DF %>% 
  mutate(Deaths=as.numeric(Deaths))


# Add dummy Estimate field

DF <- DF %>% 
    mutate(Estimate=Cases)


#   Last date in dataset formatted for plotting

sf <- stamp_date("Sunday, Jan 17, 1999")
lastdate <- sf(DF$Date[nrow(DF)])

LastDate <- DF[nrow(DF),]$Date


#   Crowdsize text

CrowdText <- "Sizes of groups to avoid to keep\nchance of meeting a contagious\nperson below 1%"
grob2 <- grid::grid.text(CrowdText, x=0.3,  y=0.8, gp=grid::gpar(col="black", fontsize=16))

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
  mutate(percapita=signif(percapita,3)) 

MapLabels <- lapply(seq(nrow(MappingData)), function(i) {
  htmltools::HTML(
    str_replace_all(
      paste0( MappingData[i,]$County, ' County<br>', 
              MappingData[i,]$Cases,' Cases Total<br>', 
              MappingData[i,]$percapita, " per 100,000<br>",
              MappingData[i,]$Deaths, " Deaths"),
      "NA", "Zero"))
})

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
                                             min = 0.01,
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
                                               "Cases per 100,000 population" = "percapita"
                                               #"Deaths" = "deaths"
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
  build_model <- function(subdata, weight_flag){ 
    print(":::::::  build_model")
      # Linear fits to log(cases)
    ##########   Base case with actual data  
    weights <- (1:nrow(subdata))**1.5
    if (weight_flag) {
      LogFits <- lm(log10(Cases)~Days, data=subdata, weights=weights)
    } else {
      LogFits <- lm(log10(Cases)~Days, data=subdata)
    }
    m <- LogFits[["coefficients"]][["Days"]]
    b <- LogFits[["coefficients"]][["(Intercept)"]]
    Rsqr <- summary(LogFits)$adj.r.squared
    std_dev <- sigma(LogFits)
    print(paste("--3--", m, b, Rsqr, std_dev))  ######################### print
    # return a tibble
    tribble(~m, ~b, ~Rsqr, ~std_dev,
             m,  b,  Rsqr,  std_dev)
  }
      
  build_est_model <- function(){ 
    print(":::::::  build_est_model")
      # Linear fits to log(cases)
    ##########   Case with estimates of undercount
    subdata <<- subdata %>% # update mult cases in case needed
                 mutate(Estimate=Cases*replace_na(input$mult_pos,0.1))
    weights <- (1:nrow(subdata))**1.5
    if (input$weights) {
      LogFits <- lm(log10(Estimate)~Days, data=subdata, weights=weights)
    } else {
      LogFits <- lm(log10(Estimate)~Days, data=subdata)
    }
    #LogFits <- lm(log10(Estimate)~Days, data=subdata)
    m <- LogFits[["coefficients"]][["Days"]]
    b <- LogFits[["coefficients"]][["(Intercept)"]]
    Rsqr <- summary(LogFits)$adj.r.squared
    std_dev <- sigma(LogFits)
    print(paste("--3.9--", m, b))  ######################### print
    # return a tibble
    tribble(~m, ~b, ~Rsqr, ~std_dev,
             m,  b,  Rsqr,  std_dev)
  }
  #---------------------------------------------------    
  #---------------New  Build Model -------------------
  #---------------------------------------------------    
  new_build_model <- function(x, y, weight_flag){ 
    print(":::::::  new_build_model")
    print(paste("x", x))
    print(paste("y", y))
      # Linear fits to log(cases)
    ##########   Base case with actual data  
    weights <- (1:length(x))**1.5
    if (weight_flag) {
      LogFits <- lm(log10(y)~x, weights=weights)
    } else {
      LogFits <- lm(log10(y)~x)
    }
    m <- LogFits[["coefficients"]][["x"]]
    b <- LogFits[["coefficients"]][["(Intercept)"]]
    Rsqr <- summary(LogFits)$adj.r.squared
    std_dev <- sigma(LogFits)
    print(paste("--A3--", m, b, Rsqr, std_dev))  ######################### print
    # return a tibble
    tribble(~m, ~b, ~Rsqr, ~std_dev,
             m,  b,  Rsqr,  std_dev)
  }
    
  #---------------------------------------------------    
  #------------------- Build exponential curve -------
  #---------------------------------------------------    
    build_expline <- function(crv=c('real', 'est')){
    print(":::::::  build_expline")
      #   Go 10 days into future
      lastday <- as.integer(LastDate - begin) + 1 # last day of real data
      dayseq <- 0:(lastday + 9)
      dateseq <- as_date(begin:(LastDate+10))
      model <- build_model(subdata, input$weights)
      est_model <- build_est_model()
      m <- model$m
      b <- model$b
      Rsqr <- model$Rsqr
      m_est <- est_model$m
      b_est <- est_model$b
      print(paste("----build_expline----", crv, model))
      if (input$modeling=="do fit") {
        Cases <- case_when(
          crv=="real" ~  10**(m*dayseq+b),
          crv=="est"  ~  10**(m_est*dayseq+b_est)
        )
      } else if (input$modeling=="user") {
        m <- input$fit
        b <- input$intercept
        Cases <- case_when (
          crv=="real" ~  10**(m*dayseq+b),
          crv=="est"  ~  10**(m*dayseq+b)*replace_na(input$mult_pos, 0.1)
        )
      } else {
        m <- global_slope
        b <- case_when(
          crv=="real" ~  log10(subdata$Cases[lastday]) - m*(lastday-1),
          crv=="est"  ~  log10(subdata$Estimate[lastday]) - m*(lastday-1)
        )
        Cases <- 10**(m*dayseq+b)
      }
      SD_upper <- Cases+Cases*model$std_dev
      SD_lower <- Cases-Cases*model$std_dev
      ExpLine <- tibble( Days=dayseq, Date=dateseq, Cases=Cases,
                         SD_upper=SD_upper, SD_lower=SD_lower)
      #print(paste("--5--", ExpLine))######################### print
      #  return a tibble
      tribble(~Line, ~m, ~b, ~Rsqr,
               ExpLine, m, b, Rsqr)
    }  
  
  #---------------------------------------------------    
  #-----------Build an exponential model -------------
  #---------------------------------------------------    
    build_expmodel <- function(data, 
                               y="Cases",
                               weights=TRUE, 
                               fit=c('all', 'm', 'b'),
                               m=1.3,
                               b=1){
    print(":::::::  build_expmodel")
      #   Go 10 days into future
      lastday <- as.integer(LastDate - begin) + 1 # last day of real data
      dayseq <- 0:(lastday + 9)
      dateseq <- as_date(begin:(LastDate+10))
      model <- new_build_model(data$Days, unlist(data[,y]), weights)
      
      m <- model$m
      b <- model$b
      Rsqr <- model$Rsqr
      print(paste("----build_expline----", model))
      if (fit=="all") {
          Cases <- 10**(m*dayseq+b)
      } else if (fit=="m") {
        m <- input$fit
        b <- input$intercept
      } else {
        m <- global_slope
        Cases <- 10**(m*dayseq+b)
      }
      SD_upper <- Cases+Cases*model$std_dev
      SD_lower <- Cases-Cases*model$std_dev
      ExpLine <- tibble( Days=dayseq, Date=dateseq,!!y:=Cases,
                         SD_upper=SD_upper, SD_lower=SD_lower) 
      print(paste("--5--", ExpLine))######################### print
      print(summary(ExpLine))
      #  return a tibble
      tribble(~Line, ~m, ~b, ~Rsqr,
               ExpLine, m, b, Rsqr)
    }  
  
  #---------------------------------------------------    
  #------------------- Build Basic Plot --------------
  #---------------------------------------------------    
  
  build_basic_plot <- function(){
      # Build exponential line for plot
    print(":::::::  build_basic_plot")
    foo <- build_expline("real")
    EqText <- paste0("Fit is log(Cases) = ",
                     signif(foo$m,3),"*Days + ",
                     signif(foo$b,3))
    ExpLine <- foo$Line[[1]]
    xform <- 2*signif(max(TestingData$Total)/(6*max(subdata$Cases)),2)
    #xform <- 10.0 # transform for secondary axis. Multiply primary by this
    grob <- grid::grid.text(EqText, x=0.7,  y=0.1, gp=grid::gpar(col="black", fontsize=15))
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
      
      p <- p + annotate("label", 
                        x=(LastDate - begin + 10)/1.5+begin, 
                        y=subdata$Cases[nrow(subdata)]/3, 
                        label=EqText)
      
      if (!is.nan(ExpLine$SD_lower[1]) & input$modeling=="do fit"){
           p <- p + geom_errorbar(data=ExpLine[(nrow(ExpLine)-9):nrow(ExpLine),],
                        aes(x=Date, y=Cases, ymin=SD_lower, ymax=SD_upper)) 
      }
      
      if (input$logscale) {
        trans_value <- "log10"
        min_limit <- min(subdata$Cases[1], 10)
      } else {
        trans_value <- "identity"
        min_limit <- 0
      }
      if (!input$zoom) {
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
    return(p)
  }
  

  #---------------------------------------------------    
  #------------------- Add mult Plot ---------------
  #---------------------------------------------------    
  add_mult <- function(p) {
    print(":::::::  add_mult")
    foo <- build_expline("est")
    Est_layer <-   geom_line(data=foo$Line[[1]],
                             aes(x=Date, y=Cases,
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
  
  build_legend <- function(p){
    print(":::::::  build_legend")
    
    Labels <- c("Data", "Tests")
    Values <- c("fit"="blue", "tests"="black")
    Breaks <- c("fit", "tests")
    
    if (input$mult) {
      Labels <- c(Labels, "Multiplied")
      Values <- c(Values, "mult"="red")
      Breaks <- c(Breaks, "mult")
    }
    if (input$estmiss) {
      Labels <- c(Labels, "Missed\nCases")
      Values <- c(Values, "est_miss"="green")
      Breaks <- c(Breaks, "est_miss")
    }
    
    Est_legend <- theme(legend.position=c( 0.1, 0.5 ))
      
    Legend_layer <- scale_color_manual(name = "Models", 
                                         values = Values,
                                         labels = Labels,
                                         breaks = Breaks)
    print(paste("Values:", Values))
    print(paste("Labels:", Labels))
    p <- p + Est_legend + Legend_layer 
    
    return(p)
  }
  
  #---------------------------------------------------    
  #------------------- Add crowd size ----------------
  #---------------------------------------------------    
  # When is probability of 1% contact reached?
  add_crowdsize <- function(p) {
    print(":::::::  add_crowdsize")
      Population <- PopLabel[2][[1]]

      foo <- build_expline("real") 
      ExpLine <- foo$Line[[1]]
      m <- foo$m
      b <- foo$b
      foo <- build_expline("est") 
      ExpLine_est <- foo$Line[[1]]
      m_est <- foo$m 
      b_est <- foo$b  
      
      TestDays <- as.integer(LastDate 
                             - begin) + c(0,5,10) + 1
      TestDates <- LastDate + c(0,5,10)
      Crowdsize <- signif((0.01*Population)/(10**(TestDays*m+b)), 2)
      print(paste("--->>> m, b", m, b))
      Crowdsize_est <- signif((0.01*Population)/(10**(TestDays*m_est+b_est)
                                                 *replace_na(input$mult_pos,0.1)), 2)
      
      dayseq <- 0:(as.integer(LastDate - begin) + 10)
      dateseq <- as_date(begin:(LastDate + 10))
      Cases <- ExpLine$Cases
      Cases_est <- ExpLine_est$Cases
      #  Scale cases so similar scaling to days
      CaseScale <- length(dateseq)/max(Cases)
      Delta <- (Cases[TestDays] - Cases[TestDays-1])*CaseScale
      r <-  2 # radius distance
      x_nudge <- -r*cos((pi/2 -atan(Delta)))- 0.5
      y_nudge <- r*sin((pi/2 -atan(Delta)))/CaseScale
      y_extra <- max(ExpLine$Cases)/30
      y_extra <- 1
      
      # Build label tibble
      CrowdLabels <- tibble(Date=TestDates,
                            Crowd=Crowdsize,
                            Cases=Cases[TestDays],
                            Delta=Delta,
                            x_nudge=x_nudge,
                            y_nudge=y_nudge)
      
      CrowdLabels_est <- tibble(Date=TestDates,
                                Crowd=Crowdsize_est,
                                Cases=Cases_est[TestDays],
                                Delta=Delta,
                                x_nudge=x_nudge,
                                y_nudge=y_nudge)
      
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
      
      p <- p + CrowdLayer1 + 
               CrowdLayer2 + 
               CrowdLayer3 +
               annotation_custom(grob2)
      
      if (input$mult) { # Add for fit and estimate 
          return(p + CrowdLayerest1 + CrowdLayerest2 + CrowdLayerest3)
      } else { # Add for fit only
          return(p)
      }
  }
  
  #---------------------------------------------------    
  #------------------- Displayed Data text -----------
  #---------------------------------------------------    
  displayed_data <- function(m, Rsqr){
    print(":::::::  displayed_data")
    output$data_details <- renderUI({
      str1 <- paste("Most recent value, on",subdata$Date[nrow(subdata)],
                    "was<b>", subdata$Cases[nrow(subdata)],"</b>Cases")
      str3 <- paste("           Doubling Time =", signif(log10(2)/m,2), "days")
      
      if (!is.nan(Rsqr) & input$modeling=="do fit"){
        if (Rsqr>.8) {
          str2 <- paste("R<sup>2</sup> value for fit =", signif(Rsqr,4))
        } else {
          str2 <- paste("<font color=\"red\">R<sup>2</sup> value for fit =", 
                        signif(Rsqr,4),
                        "<b>which is poor</b></font>")
        }
        HTML(paste(str1, str3, str2, sep = '<br/>'))
      } else {
        HTML(paste(str1, str3, sep = '<br/>'))
      }
    })
  }
  
  ################################## Analysis
  
       
  #---------------------------------------------------    
  #------------------- Prep Analysis Data ------------
  #---------------------------------------------------    
  prep_An_data <- function(){ # return population, label for graph title and tibble subset
    print(":::::::  prep_An_data")
    if (input$dataset=="Region") { # work with regions
        print(paste("--A1--", DefineRegions$List[DefineRegions$Region==input$region]))######################### print
      An_PopLabel <<- Regions %>% filter(Region==input$region)
      target <- unlist(DefineRegions$List[DefineRegions$Region==input$region])
      An_subdata <<- DF %>% 
          filter(County %in% target) %>% 
          group_by(Date) %>% 
          summarise(Cases=sum(Cases), Days=mean(Days), Estimate=sum(Estimate))
      print(paste("---A1.5---", subdata, input$region))######################### print
      An_begin <<- subdata$Date[1] # date of first reported case
      return()
    } else {
        print(paste("--A2--", input$county))######################### print
      #   Is there any data?
      if (! input$county %in% DF$County) {
        showNotification(paste("No reported cases in", input$county))
        return()
      }
      
      An_PopLabel <<- Counties %>% filter(County==input$county) %>% 
                     mutate(Label=paste(input$county, "County"))
      An_subdata <<- DF %>% filter(County==input$county)
      An_begin <<- subdata$Date[1] # date of first reported case
      return()
    }
  } # end prep_data
    
  #---------------------------------------------------    
  #------------------- Build Death Plot --------------
  #---------------------------------------------------    
  
  build_deaths_plot <- function(){
      # Build exponential line for plot
    print(":::::::  build_death_plot")
    foo <- build_expmodel(DeathData,
                          y="Cum_Deaths",
                          fit="all")
    print("build_deaths_plot --- 1")
    EqText <- paste0("Fit is log(Cumulative Deaths) = ",
                     signif(foo$m,3),"*Days + ",
                     signif(foo$b,3))
    print("build_deaths_plot --- 2")
    ExpLine <- foo$Line[[1]]
    print(summary(ExpLine))
    print("build_deaths_plot --- 3")
    #xform <- 2*signif(max(TestingData$Total)/(6*max(foo$Cum_Deaths)),2)
    print("build_deaths_plot --- 4")
    #xform <- 10.0 # transform for secondary axis. Multiply primary by this
    grob <- grid::grid.text(EqText, x=0.7,  y=0.1, gp=grid::gpar(col="black", fontsize=15))
    print("build_deaths_plot --- 5")
    if (!input$Deaths_logscale) {
        p <- DeathData %>% 
            ggplot(aes(x=Date, y=Cum_Deaths)) +
            geom_col(alpha = 2/3) 
     } else {
        
        p <- DeathData %>% 
            ggplot(aes(x=Date, y=Cum_Deaths)) +
            geom_point() 
     } 
        p <- p + geom_label(aes(label=Cum_Deaths), stat='identity', size = 3) +
          expand_limits(x = LastDate+10) +
          geom_line(data=ExpLine,
                    aes(x=Date, y=Cum_Deaths,
                        color="fit"),
                    size=1,
                    linetype="dashed") +
          geom_line(data=ExpLine[1:(nrow(ExpLine)-10),],
                    aes(x=Date, y=Cum_Deaths,
                        color="fit" ),
                    size=1,
                    linetype="solid") +
          geom_point(data=ExpLine[(nrow(ExpLine)-9):nrow(ExpLine),],
                        aes(x=Date, y=Cum_Deaths),
                     shape=20, size=2, fill="blue") +
          geom_label(data=ExpLine[(nrow(ExpLine)-9):nrow(ExpLine),],
                      aes(label=as.integer(Cum_Deaths+.5)),
                      hjust=1, vjust=0) +
          geom_line(data=ExpLine,
                    aes(x=Date-13, y=Cum_Deaths*60,
                        color="tests"),
                    size=1,
                    linetype="dashed") +
          #geom_point(data=TestingData,
          #              aes(x=Date, y=Total/xform, color="tests"),
          #           size=3, shape=23, fill="black") +
          #geom_text(data=TestingData,
          #          aes(x=Date, y=Total/xform, label=Total),
          #          nudge_x=-1.00, nudge_y=0.0) +
          theme(text = element_text(size=20)) +
          labs(title=paste0("COVID-19 Deaths in ",PopLabel$Label), 
               subtitle=paste0(" as of ", lastdate))
      
      p <- p + annotate("label", 
                        x=(LastDate - begin + 10)/1.5+begin, 
                        y=DeathData$Cum_Deaths[nrow(DeathData)]/5, 
                        label=EqText)
      
      if (!is.nan(ExpLine$SD_lower[1]) & input$modeling=="do fit"){
           p <- p + geom_errorbar(data=ExpLine[(nrow(ExpLine)-9):nrow(ExpLine),],
                        aes(x=Date, y=Cum_Deaths, ymin=SD_lower, ymax=SD_upper)) 
      }
      
      if (input$Deaths_logscale) {
        trans_value <- "log10"
        min_limit <- min(ExpLine$Cum_Deaths[1], 2)
      } else {
        trans_value <- "identity"
        min_limit <- 0
      }
      if (input$Deaths_zoom) {
          # limit height of modeled fit
        p <- p + scale_y_continuous(limits=c(min_limit, 6*max(ExpLine$Cum_Deaths)),
                                    #sec.axis = sec_axis(~.*xform, 
                                    #name = "Statewide Test Total"),
                                    trans=trans_value)
      } else {
        p <- p + scale_y_continuous(limits=c(min_limit, max(ExpLine$Cum_Deaths)),
                                    #sec.axis = sec_axis(~.*xform, 
                                    #name = "Statewide Test Total"),
                                    trans=trans_value)
      }
  
    print(":::::::  displayed_data")
    m <- foo$m
    b <- foo$b 
    Rsqr <- foo$Rsqr
    output$death_details <- renderUI({
      str1 <- paste("Most recent value, on",DeathData$Date[nrow(DeathData)],
                    "was<b>", DeathData$Cum_Deaths[nrow(DeathData)],"</b>Deaths")
      str3 <- paste("           Doubling Time =", signif(log10(2)/m,2), "days")
      
      if (!is.nan(Rsqr)){
        if (Rsqr>.8) {
          str2 <- paste("R<sup>2</sup> value for fit =", signif(Rsqr,4))
        } else {
          str2 <- paste("<font color=\"red\">R<sup>2</sup> value for fit =", 
                        signif(Rsqr,4),
                        "<b>which is poor</b></font>")
        }
        HTML(paste(str1, str3, str2, sep = '<br/>'))
      } else {
        HTML(paste(str1, str3, sep = '<br/>'))
      }
    })
  
    return(p)
  }
  
  ######################  Map ########################
  #---------------------------------------------------    
  #------------------- Build Model -------------------
  #---------------------------------------------------    
  
  draw_map <- function() {
    
    print("Map --1--")
    Range <- range(MappingData$percapita,na.rm=TRUE)
    
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
                      paste0(cuts[-n], " &ndash; ", cuts[-1])
                    },
                    opacity = 1)
      }) 
    } else { #  Deaths
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
      foo <- build_expline("real")
      p <- build_basic_plot()

      if (input$mult) {
          p <- add_mult(p)
      } 
      if (input$estmiss) {
          p <- add_estmiss(p)
      } 
      if (input$avoid) {
          p <- add_crowdsize(p)
      }
      p <-  build_legend(p)
      output$plot_graph <- renderPlot({
          print(p)
          })
      displayed_data(foo$m, foo$Rsqr)
  })
   
  #---------------------------------------------------    
  #------------------- Select Analysis Data ----------
  #---------------------------------------------------    
  observeEvent({
                input$An_dataset
                input$An_region
                input$An_county
                1}, { # Change data selection
    print(":::::::  observe_event Analysis Data")
                  
      prep_An_data()
      
      if (input$An_tabs == "Deaths") {
        p <- build_deaths_plot()
        output$plot_deaths <- renderPlot({print(p)})
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
                1}, { # Change data selection
    print(":::::::  observe_event Analysis Data")
                  
      prep_An_data()
      
      if (input$An_tabs == "Deaths") {
        p <- build_deaths_plot()
        output$plot_deaths <- renderPlot({print(p)})
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
      foo <- build_expline("real")
                  
      p <- build_basic_plot()
      
      if (input$mult) {
          p <- add_mult(p)
      } 
      if (input$estmiss) {
          p <- add_estmiss(p)
      } 
      if (input$avoid) {
        if (input$logscale) {
          showNotification("Crowdsize not available with log scale")
        } else {
          p <- add_crowdsize(p)
        }
      }
      p <-  build_legend(p)
      
      output$plot_graph <- renderPlot({
          print(p)
          })
      displayed_data(foo$m, foo$Rsqr)
  })
    
  #---------------------------------------------------    
  #------------------- Analysis Tab ------------------
  #---------------------------------------------------    
  observeEvent({input$An_tabs
                1} , { # 
                  
    print(":::::::  observe_event 2")
      foo <- build_expline("real")
                  
      p <- build_basic_plot()
      
      if (input$mult) {
          p <- add_mult(p)
      } 
      if (input$estmiss) {
          p <- add_estmiss(p)
      } 
      if (input$avoid) {
        if (input$logscale) {
          showNotification("Crowdsize not available with log scale")
        } else {
          p <- add_crowdsize(p)
        }
      }
      p <-  build_legend(p)
      
      output$plot_graph <- renderPlot({
          print(p)
          })
      displayed_data(foo$m, foo$Rsqr)
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
