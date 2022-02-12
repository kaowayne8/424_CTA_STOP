# CTA stations data
# This app takes CTA station data, from UIC, O'Hare airport, and 
# and gives visualizations on riders within each year, month, day, day of the week
# Author: Wayne Kao

#TODO:
# Fix colors on graphs
# Add to about page
# Test on Shiny app
# Add third station for dataset


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
library(gridExtra)


# assume all of the tsv files in this directory are data of the same kind that I want to visualize
uic <- read.table(file="uic.tsv", quote="", sep="\t", header=TRUE)
ohare <- read.table(file="ohare.tsv", quote="", sep="\t", header=TRUE)


stations <- c("UIC-Halsted", "O'Hare Airport")
graphs <- c("All Years", "Each Day", "Each Month", "Each Day of Week", "Table")
years <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)

# Create the shiny dashboard
ui <- shinyUI(
  navbarPage("CTA Riders", position = "fixed-bottom",
             tabPanel("Plot",
                      fluidPage(
                        fluidRow(style="padding-top: 5%",
                          #UIC Halsted (left) chart
                          column(1),
                          column(5,
                                 plotOutput("left_plot", height = 800)
                          ),
                          #O'Hare (right) chart
                          column(5,
                                 plotOutput("right_plot", height = 800)
                          ),
                          column(1)
                        ),
                      
                        fluidRow( style="padding-top: 2%; padding-bottom: 5%",
                          #UIC Halsted (left) control panel
                          column(6, align="center",
                                selectInput("select_left", "Select Station", stations, selected="UIC-Halsted"),
                                selectInput("select_left_graph", "Select Graph", graphs, selected="All Years"),
                                selectInput("select_left_year", "Select Year", years, selected="2021")
                                
                          ),
                          #O'Hare (right) control panel
                          column(6, align="center",
                                 selectInput("select_right", "Select Station", stations, selected="O'Hare Airport"),
                                 selectInput("select_right_graph", "Select Graph", graphs, selected="All Years"),
                                 selectInput("select_right_year", "Select Year", years, selected="2021")
                          )
                        )
                      )
              ),
             tabPanel("About",
                      h4("About page")
              ),
             tags$style(type="text/css", 
                        '.navbar{
                font-size: 30px;
             }')
  )
)

server <- function(input, output) {
  
  leftYearReactive <- reactive({
    dataset <- uic
    if(input$select_left == "UIC-Halsted"){
      dataset <- uic
    }
    else if(input$select_left == "O'Hare Airport"){
      dataset <- ohare
    }
    else{
      dataset <- uic
    }
    subset(dataset, year(ymd(newDates)) == input$select_left_year)
  })
  
  rightYearReactive <- reactive({
    dataset <- uic
    if(input$select_right == "UIC-Halsted"){
      dataset <- uic
    }
    else if(input$select_right == "O'Hare Airport"){
      dataset <- ohare
    }
    else{
      dataset <- uic
    }
    subset(dataset, year(ymd(newDates)) == input$select_right_year)
  })
  
  output$right_plot <- renderPlot({
    p_station <- input$select_right
    p_year <- input$select_right_year
    p_data <- rightYearReactive()
    p_year_select <- input$select_right_graph
    p_all_data = uic
    
    if(p_station == "UIC-Halsted"){
      p_all_data = uic
    }
    else if(p_station == "O'Hare Airport"){
      p_all_data = ohare
    }
    else{
      p_all_data = uic
    }
    
    uic2021 <- p_data
    monthLimit = c(1,2,3,4,5,6,7,8,9,10,11,12)
    monthLabel <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    monthColor <- c("red","red","red","red","red","red","red","red","red","red","red","red")
    weekDayLimit = c(1,2,3,4,5,6,7)
    weekDayLabel <- c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
    
    #UIC by year
    if(p_year_select == "All Years"){
      ggplot(data=p_all_data, aes(factor(year(ymd(newDates))), rides, fill=factor(year(ymd(newDates))))) +
        geom_bar(stat="identity") + labs(x = "Year", y = "Rides", title = paste(p_station,"Rides per Year"), fill="Years") +
        theme(axis.text=element_text(size=6))
    }
    else if(p_year_select == "Each Day"){
      ggplot(data=uic2021, aes(newDates, rides) ) +
        geom_bar(stat="identity") + labs(x = paste("Days in", p_year), y = "Rides", title = paste(p_station,"Rides per Day in",p_year))
    }
    else if(p_year_select == "Each Month"){
      ggplot(data=uic2021, aes(factor(month(ymd(newDates))), as.numeric(rides), 
                               fill=factor(month(ymd(newDates))))) +
        geom_bar(stat="identity") + 
        labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="Month") +
        scale_x_discrete(limit = factor(monthLimit), labels=monthLabel) +
        scale_fill_discrete(name = "Month", labels = monthLabel)
    }
    else if(p_year_select == "Each Day of Week"){
      ggplot(data=uic2021, aes(factor(wday(newDates)), rides, fill=factor(wday(newDates)))) + 
        geom_bar(stat="identity") + 
        labs(x = paste("Weekdays in", p_year), y = "Rides", title = paste(p_station,"Rides per Weekday in", p_year), fill="Weekday") +
        scale_x_discrete(limit = factor(weekDayLimit), labels=weekDayLabel) +
        scale_fill_discrete(name = "Weekday", labels = weekDayLabel)
    }
    else{
      a<-ggplot(data=p_all_data, aes(factor(year(ymd(newDates))), rides, fill=factor(year(ymd(newDates))))) +
        geom_bar(stat="identity") + labs(x = "Year", y = "Rides", title = paste(p_station,"Rides per Year"), fill="Years") +
        theme(axis.text=element_text(size=6), legend.position = "none")
      
      b<-ggplot(data=uic2021, aes(newDates, rides) ) +
        geom_bar(stat="identity") + 
        labs(x = paste("Days in", p_year), y = "Rides", title = paste(p_station,"Rides per Day in",p_year)) +
        theme(legend.position = "none")
      
      c<-ggplot(data=uic2021, aes(factor(month(ymd(newDates))), as.numeric(rides), 
        fill=factor(month(ymd(newDates))))) +
        geom_bar(stat="identity") + 
        labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="Month") +
        scale_x_discrete(limit = factor(monthLimit), labels=monthLabel) +
        scale_fill_discrete(name = "Month", labels = monthLabel)+
        theme(legend.position = "none")
      
      d<-ggplot(data=uic2021, aes(factor(wday(newDates)), rides, fill=factor(wday(newDates)))) + 
        geom_bar(stat="identity") + 
        labs(x = paste("Weekdays in", p_year), y = "Rides", title = paste(p_station,"Rides per Weekday in", p_year), fill="Weekday") +
        scale_x_discrete(limit = factor(weekDayLimit), labels=weekDayLabel) +
        scale_fill_discrete(name = "Weekday", labels = weekDayLabel) +
        theme(legend.position = "none")
      grid.arrange(a,b,c,d,nrow=2)
    }
    
  })
  
  output$left_plot <- renderPlot({
    p_station <- input$select_left
    p_year <- input$select_left_year
    p_data <- leftYearReactive()
    p_year_select <- input$select_left_graph
    p_all_data = uic
    
    if(p_station == "UIC-Halsted"){
      p_all_data = uic
    }
    else if(p_station == "O'Hare Airport"){
      p_all_data = ohare
    }
    else{
      p_all_data = uic
    }
    
    uic2021 <- p_data
    monthLimit = c(1,2,3,4,5,6,7,8,9,10,11,12)
    monthLabel <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    monthColor <- c("red","red","red","red","red","red","red","red","red","red","red","red")
    weekDayLimit = c(1,2,3,4,5,6,7)
    weekDayLabel <- c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
    
    #UIC by year
    if(p_year_select == "All Years"){
      ggplot(data=p_all_data, aes(factor(year(ymd(newDates))), rides, fill=factor(year(ymd(newDates))))) +
        geom_bar(stat="identity") + labs(x = "Year", y = "Rides", title = paste(p_station,"Rides per Year"), fill="Years") +
        theme(axis.text=element_text(size=6))
    }
    else if(p_year_select == "Each Day"){
      ggplot(data=uic2021, aes(newDates, rides) ) +
        geom_bar(stat="identity") + labs(x = paste("Days in", p_year), y = "Rides", title = paste(p_station,"Rides per Day in",p_year))
    }
    else if(p_year_select == "Each Month"){
      ggplot(data=uic2021, aes(factor(month(ymd(newDates))), as.numeric(rides), 
                               fill=factor(month(ymd(newDates))))) +
        geom_bar(stat="identity") + 
        labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="Month") +
        scale_x_discrete(limit = factor(monthLimit), labels=monthLabel) +
        scale_fill_discrete(name = "Month", labels = monthLabel)
    }
    else if(p_year_select == "Each Day of Week"){
      ggplot(data=uic2021, aes(factor(wday(newDates)), rides, fill=factor(wday(newDates)))) + 
        geom_bar(stat="identity") + 
        labs(x = paste("Weekdays in", p_year), y = "Rides", title = paste(p_station,"Rides per Weekday in", p_year), fill="Weekday") +
        scale_x_discrete(limit = factor(weekDayLimit), labels=weekDayLabel) +
        scale_fill_discrete(name = "Weekday", labels = weekDayLabel)
    }
    else{
      a<-ggplot(data=p_all_data, aes(factor(year(ymd(newDates))), rides, fill=factor(year(ymd(newDates))))) +
        geom_bar(stat="identity") + labs(x = "Year", y = "Rides", title = paste(p_station,"Rides per Year"), fill="Years") +
        theme(axis.text=element_text(size=6), legend.position = "none")
      
      b<-ggplot(data=uic2021, aes(newDates, rides) ) +
        geom_bar(stat="identity") + 
        labs(x = paste("Days in", p_year), y = "Rides", title = paste(p_station,"Rides per Day in",p_year)) +
        theme(legend.position = "none")
      
      c<-ggplot(data=uic2021, aes(factor(month(ymd(newDates))), as.numeric(rides), 
                                  fill=factor(month(ymd(newDates))))) +
        geom_bar(stat="identity") + 
        labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="Month") +
        scale_x_discrete(limit = factor(monthLimit), labels=monthLabel) +
        scale_fill_discrete(name = "Month", labels = monthLabel)+
        theme(legend.position = "none")
      
      d<-ggplot(data=uic2021, aes(factor(wday(newDates)), rides, fill=factor(wday(newDates)))) + 
        geom_bar(stat="identity") + 
        labs(x = paste("Weekdays in", p_year), y = "Rides", title = paste(p_station,"Rides per Weekday in", p_year), fill="Weekday") +
        scale_x_discrete(limit = factor(weekDayLimit), labels=weekDayLabel) +
        scale_fill_discrete(name = "Weekday", labels = weekDayLabel) +
        theme(legend.position = "none")
      grid.arrange(a,b,c,d,nrow=2)
    }
    
  })
}

shinyApp(ui = ui, server = server)
