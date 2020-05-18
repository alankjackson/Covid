#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(leafpop) # for popup on map
library(ggplot2)
library(stringr)
library(lubridate)
library(car)

indata <- tribble(
~Date,       ~Cases,  ~Days, 
"2020-03-10",    21,     0, 
"2020-03-11",    23,     1, 
"2020-03-12",    38,     2, 
"2020-03-13",    51,     3, 
"2020-03-14",    56,     4, 
"2020-03-15",    57,     5, 
"2020-03-16",    64,     6, 
"2020-03-17",    83,     7, 
"2020-03-18",   143,     8, 
"2020-03-19",   194,     9, 
"2020-03-20",   304,    10, 
"2020-03-21",   334,    11, 
"2020-03-22",   352,    12, 
"2020-03-23",   410,    13, 
"2020-03-24",   974,    14, 
"2020-03-25",  1396,    15, 
"2020-03-26",  1731,    16, 
"2020-03-27",  2052,    17, 
"2020-03-28",  2371,    18, 
"2020-03-29",  2877,    19, 
"2020-03-30",  3266,    20, 
"2020-03-31",  3997,    21, 
"2020-04-01",  4669,    22, 
"2020-04-02",  5330,    23, 
"2020-04-03",  6110,    24, 
"2020-04-04",  6812,    25, 
"2020-04-05",  7276,    26 
)

indata$Date <- as_date(indata$Date)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Reprex"),
    actionButton("do", "Click Me")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #---------------------------------------------------    
    #------------------- Fit Logistic function ---------
    #---------------------------------------------------    
    logistic <- function(data, 
                         indep="Cases", # independent variable
                         r=0.24,
                         projection=10){
        
        print(data)
        Asym <- max(data$Cases)*5
        xmid <- max(data$Days)*2
        scal <- 1/r
        
        ## using a selfStart model
        sigmo   <- nls(Cases ~ SSlogis(Days, Asym, xmid, scal), 
                       data=data)
        coeffs <- coef(sigmo)
        
        dayseq <- data$Days
        dayseq <- c(dayseq,(dayseq[length(dayseq)]+1):
                        (dayseq[length(dayseq)]-1+projection))
        dateseq <- data$Date
        dateseq <- as_date(c(dateseq,(dateseq[length(dateseq)]+1): 
                                 (dateseq[length(dateseq)]-1+projection)))
        foo <- tibble(Days=dayseq)
        predict2 <- function(x) {predict(x, foo)}
        f.boot <- car::Boot(sigmo,f=predict2)
        
        Cases <- predict(sigmo, data.frame(Days=dayseq))
        confidence <- confint(f.boot)
        preds <- tibble(dayseq, dateseq,
                        Cases,
                        confidence["2.5 %"],
                        confidence["97.5 %"])
        names(preds) <- c("Days", "Date","Cases","SD_upper","SD_lower")
        
        #  return a tibble
        tribble(~Line, ~r, ~K, ~xmid,
                preds, 1/coeffs[["scal"]], coeffs[["Asym"]], coeffs[["xmid"]])
        
    } 
    
    #-----------------   reactive bits
    
    observeEvent(input$do, {
        foo <- logistic(indata, 
                        indep="Cases", # independent variable
                        r=0.24,
                        projection=10)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
