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

#   Tibble database

DF <- readRDS(gzcon(url(paste0(DataLocation, "Covid.rds"))))

init_zoom <- 10

# Clean up footnotes

DF$County <- str_replace(DF$County, "\\d", "")

# Add Statewide Totals per day

DF <- DF %>% bind_rows(
                  DF %>%
                  group_by(Date) %>% 
                  summarise(Cases = sum(Cases)) %>% 
                  mutate(County="Total")
                 ) %>% 
    arrange(Date)
# Calc days since March 11

DF <- DF %>% 
    mutate(Days=as.integer(Date-ymd("2020-03-11")))

# Add dummy Estimate field

DF <- DF %>% 
    mutate(Estimate=Cases)

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



##################################################
# Define UI for displaying data for Texas
##################################################
ui <- basicPage(
        #    Graph, Map, Documentation
        tabsetPanel(id="tabs",
    ##########   Graph Tab
          tabPanel("Graph", fluid=TRUE,value="GraphTab",
            fluidPage(
        #-------------------- Plot
              column(9, # Plot
                   plotOutput("plot_graph",
                              height="800px"),
              ), # end of column Plot
        #-------------------- Controls
              column(3, # Controls
            #-------------------- Select Data
                wellPanel( # Select data to plot
                  h4("Choose the data to analyze"),
                  radioButtons("dataset", label = strong("Which Data?"),
                               choices = list("Region" = "Region", 
                                             "County" = "County"), 
                               selected = "Region",
                               width='90%',
                               inline=TRUE),
                    conditionalPanel(
                      #    Select Region
                      condition = "input.dataset == 'Region'",
                      selectInput("region", "Choose a Region:",
                                  Regions$Region,
                                  selected="Texas" )
                    ), # end conditional panel
                    conditionalPanel( 
                      #    Select County
                      condition = "input.dataset == 'County'",
                      selectInput("county", label="Choose a County:",
                                   Counties$County,
                                   selected="Harris")
                  ), # end conditional panel
                ), # end wellPanel select data to plot
            #-------------------- Plot controls
                wellPanel( # Control plot options
                  h4("Control plotting options"),
                  checkboxInput(inputId = "avoid",
                    label = strong("Crowd sizes to avoid"),
                                  value = FALSE),
                ), # end wellPanel Control plot options
            #-------------------- Modeling parameters
                wellPanel( # Modeling parameters
                  h4("Adjust modeling parameters"),
                  radioButtons("modeling", label = h5("Exponential Fit Controls"),
                               choices = list("Fit data" = "do fit", 
                                              "Worldwide (0.061)" = "standard", 
                                              "User entry" = "user"), 
                               selected = "do fit"),
                  numericInput("fit", 
                               label = h5("User entry value"),
                               step = 0.005,
                               value = 0.061),
                  HTML("<hr>"),
                  checkboxInput(inputId = "missed",
                    label = strong("Add model for missed positive tests"),
                                  value = FALSE),
                  numericInput("missed_pos", label = h5("Factor"), value = 2),
                ), # end wellPanel Modeling parameters
                
              ) # end column Controls
            ) 

          ), # end tabPanel Graph
    ##########   Map Tab
                           tabPanel("Map", fluid=TRUE, value="MapTab",
                                    HTML("<hr>"),

          ), # end tabPanel Map
    ##########   Documentation Tab
                           tabPanel("Documentation", fluid=TRUE, value="DocumentationTab",
#Counties %>% filter(County=="Harris"|County=="Fort Bend"|County=="Galveston"|County=="Waller"|County=="Montgomery"|County=="Liberty"|County=="Brazoria"|County=="Chambers"|County=="Austin") %>% summarise_at("Population",sum)
#Collin, Dallas, Denton, Ellis, Hood, Hunt, Johnson, Kaufman, Parker, Rockwall, Somervell, Tarrant and Wise
#Counties %>% filter(County=="Atascosa" |County=="Bandera" |County=="Bexar" |County=="Comal" |County=="Guadalupe" |County=="Kendall" |County=="Medina" |County=="Wilson") %>% summarise_at("Population",sum)
#Counties %>% filter(County=="Bastrop" |County=="Caldwell" |County=="Hays" |County=="Travis" |County=="Williamson") %>% summarise_at("Population",sum)
                                    HTML("<hr>"),

          )  # end tabPanel Documentation
        )  # end tabset 
    ) # end basic page

# Define server logic 
server <- function(input, output) {
    
#   Global variables are
#    PopLabel = list(Region, Population, Label)
#    subdata = tibble of data subsetted 
#    fit parameters m, b, m_est, b_est
#    plots: p=basic, avoid=avoid, missed=missed cases model
       
  #---------------------------------------------------    
  #------------------- Prep Data ---------------------
  #---------------------------------------------------    
  prep_data <- function(){ # return population, label for graph title and tibble subset
    if (input$dataset=="Region") { # work with regions
        print(paste("--1--", DefineRegions$List[DefineRegions$Region==input$region]))######################### print
      PopLabel <<- Regions %>% filter(Region==input$region)
      target <- unlist(DefineRegions$List[DefineRegions$Region==input$region])
      subdata <<- DF %>% 
          filter(County %in% target) %>% 
          group_by(Date) %>% 
          summarise(Cases=sum(Cases), Days=mean(Days), Estimate=sum(Estimate))
      #return(c(foo$Population, foo$Label, subdata))
      print(paste("---1.5---", subdata, input$region))######################### print
      return()
    } else {
        print(paste("--2--", input$county))######################### print
      PopLabel <<- Counties %>% filter(County==input$county)
      subdata <<- DF %>% filter(County==input$county)
      #return(c(foo$Population, paste(foo[1],"County"), subdata))
      return()
    }
  } # end prep_data
    
  #---------------------------------------------------    
  #------------------- Build Model -------------------
  #---------------------------------------------------    
  build_model <- function(){ 
      # Linear fits to log(cases)
    ##########   Base case with actual data  
      LogFits <- subdata %>% 
        lm(log10(Cases)~Days, data=.)
    m <<- LogFits[["coefficients"]][["Days"]]
    b <<- LogFits[["coefficients"]][["(Intercept)"]]
    print(paste("--3--", m, b))  ######################### print
    return()
  }
      
  build_est_model <- function(){ 
      # Linear fits to log(cases)
    ##########   Case with estimates of undercount
    subdata <- subdata %>% # update missed cases in case needed
                 mutate(Estimate=Cases*input$missed_pos)
    LogFits <- subdata %>% 
                 lm(log10(Estimate)~Days, data=.)
    m_est <<- LogFits[["coefficients"]][["Days"]]
    b_est <<- LogFits[["coefficients"]][["(Intercept)"]]
    print(paste("--3.9--", m_est, b_est))  ######################### print

    return()
  }
    
  #---------------------------------------------------    
  #------------------- Build exponential curve -------
  #---------------------------------------------------    
    build_expline <- function(m, b){
      #   Go 10 days into future
      dayseq <- 0:(as.integer(today() - ymd("2020-03-11")) + 10)
      dateseq <- as_date(ymd("2020-03-11"):(today()+10))
      if (input$modeling=="do fit") {
        Cases <- 10**(m*dayseq+b)
      } else if (input$modeling=="user") {
        m <<- input$fit
        b <<- log10(subdata$Cases[1])
        Cases <- 10**(m*dayseq+b)
      } else {
        m <<- 0.061
        b <<- log10(subdata$Cases[1])
        Cases <- 10**(m*dayseq+b)
      }
      ExpLine <- tibble( Days=dayseq, Date=dateseq, Cases=Cases )
      print(paste("--4.5--", m*dayseq+b))######################### print
      print(paste("--5--", ExpLine))######################### print
      return(ExpLine)
    }  
  
  #---------------------------------------------------    
  #------------------- Build Basic Plot --------------
  #---------------------------------------------------    
  
  build_basic_plot <- function(){
      # Build exponential line for plot
    print(paste("--4--", m, b))######################### print
    EqText <- paste0("Fit is log(Cases) = ",signif(m,3),"*Days + ",signif(b,3))
    ExpLine <- build_expline(m, b)
    grob <- grid::grid.text(EqText, x=0.7,  y=0.1, gp=grid::gpar(col="black", fontsize=10))
    print(paste("--6--"))######################### print
      p <- subdata %>% 
          ggplot(aes(x=Date, y=Cases)) +
          geom_col(alpha = 2/3)+
          expand_limits(x = today()+10) +
          geom_line(data=ExpLine,
                    aes(x=Date, y=Cases,
                        color="blue"),
                    size=1,
                    linetype="dashed") +
          geom_line(data=ExpLine[1:(nrow(ExpLine)-10),],
                    aes(x=Date, y=Cases,
                        color="blue"),
                    size=1,
                    linetype="solid") +
          ylim(0, 6*max(subdata$Cases)) + # limit height of modeled fit
          annotation_custom(grob) +
          labs(title=paste0("CORVID-19 Cases in ",PopLabel$Label))
    print(paste("--6.9--"))######################### print
          #annotation_custom(grob2) +
    return(p)
  }
  
  #---------------------------------------------------    
  #------------------- Add Missed Plot ---------------
  #---------------------------------------------------    
  add_missed <- function(p) {
    build_est_model()
    ExpLine_est <- build_expline(m_est, b_est)
    Est_layer <-   geom_line(data=ExpLine_est,
                             aes(x=Date, y=Cases,
                                 color="purple"),
                             size=1,
                             linetype="dotted")
    Est_legend <- theme(legend.position="right")
      
    Legend_layer <- scale_color_discrete(name = "Models", 
                                         labels = c("Data", "Estimate"))
    p <- p +
         Est_layer +
         Est_legend +
         Legend_layer
    
    return(p)
  }
  
  #------------------- Reactive bits ---------------------

  #observeEvent(input$tabs, { # do stuff when tab changes
  #  print(paste("tab:", input$tabs))  
  #  if (input$tabs=="GraphTab") { ##  Graph Tab ##
  #    print(paste("Population and title:",prep_data()))
  #  }
  #})
    
  #---------------------------------------------------    
  #------------------- Select Data -------------------
  #---------------------------------------------------    
  observeEvent({input$dataset
                input$region
                input$county
                1}, { # Change data selection
      prep_data()
      build_model()
      p <- build_basic_plot()
      if (input$missed) {
          p <- add_missed(p)
      } else {
          p <- p + theme(legend.position = "none")
      }
      output$plot_graph <- renderPlot({
          print(p)
          })
  })
    
  #---------------------------------------------------    
  #------------------- Fiddle with Model -------------
  #---------------------------------------------------    
  observeEvent({input$modeling
                input$fit
                input$missed
                input$missed_pos
                1} , { # 
      build_model()
      p <- build_basic_plot()
      if (input$missed) {
          p <- add_missed(p)
      } else {
          p <- p + theme(legend.position = "none")
      }
      output$plot_graph <- renderPlot({
          print(p)
          })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
