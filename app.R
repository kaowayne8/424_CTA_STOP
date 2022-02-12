# sample R + Shiny example for CS 424 Spring 2022 UIC - Andy Johnson
# www.evl.uic.edu/aej/424

# This is a sample dashboard making use of the evl room temperature data and displaying
# it in a variery of ways to show off some of the different capabilities of R and Shiny
# and the Shiny Dashboard.

#libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)


# assume all of the tsv files in this directory are data of the same kind that I want to visualize
uic <- read.table(file="uic.tsv", quote="", sep="\t", header=TRUE)


stations <- c("UIC-Halsted", "O'Hare Airport")
graphs <- c("All Years", "Each Day", "Each Month", "Each Day of Week")

# Create the shiny dashboard
ui <- shinyUI(
  navbarPage("App Title",
             tabPanel("Plot",
                      
                      fluidPage(
                        fluidRow(
                          #UIC Halsted (left) control panel
                          column(6, align="center",
                                selectInput("select_left", "Select Station", stations, selected="UIC-Halsted"),
                                selectInput("select_left_graph", "Select Graph", graphs, selected="All Years")
                                
                          ),
                          #O'Hare (right) control panel
                          column(6, align="center",
                                 selectInput("select_right", "Select Station", stations, selected="O'Hare Airport")
                          )
                        ),
                        
                        fluidRow(
                          #UIC Halsted (left) chart
                          column(6,
                                 plotOutput("left_plot")
                          ),
                          #O'Hare (right) chart
                          column(6,
                                 h4("R2C2")
                          )
                        )
                        
                      )
              ),
             tabPanel("About",
                      h4("About page")
              )
  )
)

server <- function(input, output) {
  
  #allYearReactive <- reactive({subset(allData, year(allData$newDate) == input$Year)})
  
  output$left_plot <- renderPlot({
    #UIC by year
    if(input$select_left_graph == "All Years"){
      ggplot(data=uic, aes(factor(year(ymd(newDates))), rides, fill=factor(year(ymd(newDates))))) +
        geom_bar(stat="identity") + labs(x = "Year", y = "Rides", title = "UIC-Halsted Rides per Year", fill="Years") +
        theme(axis.text=element_text(size=6))
    }
    else if(input$select_left_graph == "Each Day"){
      uic2021 <- subset(uic, year(ymd(newDates)) == 2021)
      #uic2021
      ggplot(data=uic2021, aes(uic2021$newDates, uic2021$rides) ) +
        geom_bar(stat="identity") + labs(x = "Days in 2021", y = "Rides", title = "UIC(2021) Rides per Day")
    }
    else if(input$select_left_graph == "Each Month"){
      
    }
    else{
      
    }
    
  })
}

shinyApp(ui = ui, server = server)
