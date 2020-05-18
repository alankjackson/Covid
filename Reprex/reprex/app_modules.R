#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#  http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)

moduleServer <- function(id, module) {
  callModule(module, id)
}

indata <- tribble(
~Date,   ~Cases,  ~Deaths, 
"2020-03-10",  21,   0, 
"2020-03-11",  23,   1, 
"2020-03-12",  38,   2, 
"2020-03-13",  51,   3, 
"2020-03-14",  56,   4, 
"2020-03-15",  57,   5, 
"2020-03-16",  64,   6, 
"2020-03-17",  83,   7, 
"2020-03-18", 143,   8, 
"2020-03-19", 194,   9, 
"2020-03-20", 304,  10, 
"2020-03-21", 334,  11, 
"2020-03-22", 352,  12, 
"2020-03-23", 410,  13, 
"2020-03-24", 974,  14, 
"2020-03-25",  1396,  15, 
"2020-03-26",  1731,  16, 
"2020-03-27",  2052,  17, 
"2020-03-28",  2371,  18, 
"2020-03-29",  2877,  19, 
"2020-03-30",  3266,  20, 
"2020-03-31",  3997,  21, 
"2020-04-01",  4669,  22, 
"2020-04-02",  5330,  23, 
"2020-04-03",  6110,  24, 
"2020-04-04",  6812,  25, 
"2020-04-05",  7276,  26 
)
indata$Date <- as_date(indata$Date)

window <- 5
DF <- indata %>% 
  arrange(Date) %>% 
  mutate(new_cases=(Cases-lag(Cases, default=Cases[1]))) %>%
  mutate(new_cases=pmax(new_cases, 0)) %>% # truncate negative numbers
  mutate(new_deaths=(Deaths-lag(Deaths, default=Deaths[1]))) %>%
  mutate(new_deaths=pmax(new_deaths, 0)) # truncate negative numbers
  
DF <- DF %>% 
  mutate_at(c("Cases", "Deaths", "new_cases", "new_deaths"), 
      list(avg = ~ zoo::rollapply(., window, 
               FUN=function(x) mean(x, na.rm=TRUE),
               fill=c(first(.), NA, last(.))))) %>% 
  rename_at(vars(ends_with("_avg")), 
      list(~ paste("avg", gsub("_avg", "", .), sep = "_")))
#----------------------------------------------------------
#     create attribute selector
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
    HTML("<hr>"),
    radioButtons(
    ns("Attribute"),
    label = NULL,
    choices = list(
      "Cases" = "Cases",
      "New Cases" = "new_cases",
      "Deaths" = "Deaths",
      "New Deaths" = "new_deaths"
    ),
    selected = "Cases"
    ) 
  )
}

#   Combine variable name and attribute to create
#   the new variable name that has been selected
attribute_select_server <- function(input, output, session, id){
  #moduleServer(id, function(input, output, session) {
  returnval <- reactiveVal(NA_character_)
  observeEvent({input$Attribute
    input$Running_average
    1},{ 
    
    y_axis <- input$Attribute
    if (input$Running_average) {
      y_axis <- paste0("avg_", y_axis)
    }

    print(paste(id, "--- select:", y_axis))
    returnval(list("y_axis"=y_axis))
    #list("y_axis"=reactive(y_axis))
    }, ignoreInit = FALSE)
  returnval
}

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Reprex"),
  wellPanel(
    attribute_select_UI("Yaxis", "Choose the Y-Axis")
  ),
  wellPanel(
    attribute_select_UI("Highlight", "Highlight")
  ),
  HTML("<hr>"),
  checkboxInput(
    inputId = "log",
    label = strong("Log Scale"),
    value = TRUE
  ),
  verbatimTextOutput("out")
)

#    Server
server <- function(input, output) {
  
#---------------- build plot
  build_Regions_plot <- function(
    in_y_axis,
    in_highlight,
    in_log
  ){
    print(":::::::  build_plot")
    print(paste(in_y_axis, in_log))
  }
  
#----------------- reactive bits
 # Yaxis <- attribute_select_server(id="Yaxis", "Yaxis") 
 # Highlight <- attribute_select_server(id="Highlight","Highlight") 
  Yaxis <- callModule(module = attribute_select_server,
                      id="Yaxis","Yaxis") 
  Highlight <- callModule(module = attribute_select_server,id="Highlight","Highlight") 
  
#################### below here is where my understanding breaks down
#        This doesn't work, and I don't understand.
  
  output$out <- renderPrint(paste("Yaxis = ",Yaxis()$y_axis))
  
  ########## This is what I woudl like to do, but it doesn't
  ########## work. But I hope it captures the flavor
  observeEvent({
    Yaxis()$y_axis
    Highlight()$y_axis
    input$log
    1} , { # 
    print(":::::::  observe_event ")
    print(Yaxis()$y_axis)
    
    print(paste(Yaxis()$y_axis,
        Highlight()$y_axis,
        input$log))
    

    p <- build_Regions_plot(Yaxis()$y_axis,
            Highlight()$y_axis,
            input$log
    )
    #      
    #  output$plot_Regions <- renderPlot({print(p)})     
    })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
