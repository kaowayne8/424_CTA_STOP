# CTA stations data
# This app takes CTA station data, from UIC, O'Hare airport, and 
# and gives visualizations on riders within each year, month, day, day of the week
# Author: Wayne Kao

#TODO:
# Test on Shiny app


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
cermak <- read.table(file="cermak.tsv", quote="", sep="\t", header=TRUE)


stations <- c("UIC-Halsted", "O'Hare Airport", "54th/Cermak")
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
                                 plotOutput("left_plot", height = 750)
                          ),
                          #O'Hare (right) chart
                          column(5,
                                 plotOutput("right_plot", height = 750)
                          ),
                          column(1)
                        ),
                      
                        fluidRow( style="padding-top: 2%; padding-bottom: 18%",
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
                      fluidPage(
                          fluidRow(style="font-size: 40px; padding-bottom: 15%",
                            h1("CTA Rides Data"),
                            h4("Author: Wayne Kao"),
                            h4("Dataset: https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
                            div("The data was taken from the city of chicago page. This app was written to compare the amount of riders from 2001-2021
                                from three stations: UIC-Halsted, O'Hare Airport, and 54th/Cermak. For UIC-Halsted, the coloring of the graph coresponds
                                more to the UIC school year and timings of the year versus O'Hare and 54th/Cermak looks at more towards overall year
                                based on the season. The data goes up to November 2021 so December of 2021 is missing in this dataset. You are able
                                to switch graphs between looking at all the riders at a particular station with the following criteria:
                                all years from 2001-2021, or all riders categorized by days, months, day of the week with a particular year.
                                You are also given an option to view all graphs in a table like structure.")
                          )                     
                      )
              ),
             tags$style(type="text/css", 
                        '.navbar{
                font-size: 20px;
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
      dataset <- cermak
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
      dataset <- cermak
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
      p_all_data = cermak
    }
    
    uic2021 <- p_data
    monthLimit = c(1,2,3,4,5,6,7,8,9,10,11,12)
    monthLabel <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    monthColor <- c("red","red","red","red","red","red","red","red","red","red","red","red")
    weekDayLimit = c(1,2,3,4,5,6,7)
    weekDayLabel <- c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
    seasons_label <- c("Spring", "Summer", "Fall", "Winter")
    seasons_color <- c("gold2","palegreen3","sienna1","blue")
    
    #UIC by year
    if(p_year_select == "All Years"){
      a<-ggplot(data=p_all_data, aes(factor(year(ymd(newDates))), rides, fill=factor(year(ymd(newDates))))) +
        geom_bar(stat="identity") + labs(x = "Year", y = "Rides", title = paste(p_station,"Rides per Year"), fill="Years") +
        theme(axis.text=element_text(size=6))
      a
    }
    else if(p_year_select == "Each Day"){
      b<-ggplot(data=uic2021, aes(newDates, rides, fill=season) ) +
        geom_bar(stat="identity") + 
        labs(x = paste("Days in", p_year), y = "Rides", title = paste(p_station,"Rides per Day in",p_year))+
        scale_fill_manual(labels=seasons_label, values=seasons_color)
      b
    }
    else if(p_year_select == "Each Month"){
      fill_var <- NULL
      if(p_station == "UIC-Halsted"){
        fill_var <- uic2021$school
      }
      else if(p_station == "O'Hare Airport"){
        fill_var <- uic2021$season
      }
      else{
        fill_var <- uic2021$season
      }
      c<-ggplot(data=uic2021, aes(factor(month(ymd(newDates))), as.numeric(rides), 
                                  fill=fill_var)) +
        geom_bar(stat="identity") + 
        scale_x_discrete(limit = factor(monthLimit), labels=monthLabel) 
      #Adjust colors for UIC-Halsted 
      if(p_station == "UIC-Halsted"){
        c <- c + labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="School")+
          scale_fill_manual(labels=c("In School", "On Break"), values=c("seagreen", "indianred1"))
      }
      else {
        c <- c + labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="Season")+
          scale_fill_manual(labels=seasons_label, values=seasons_color)
      }
      c
    }
    else if(p_year_select == "Each Day of Week"){
      d<-ggplot(data=uic2021, aes(factor(wday(newDates)), rides, fill=factor(wday(newDates)))) + 
        geom_bar(stat="identity") + 
        labs(x = paste("Weekdays in", p_year), y = "Rides", title = paste(p_station,"Rides per Weekday in", p_year), fill="Weekday") +
        scale_x_discrete(limit = factor(weekDayLimit), labels=weekDayLabel) +
        scale_fill_discrete(name = "Weekday", labels = weekDayLabel)
      if(p_station == "UIC-Halsted"){
        d <- d + scale_fill_manual(labels=weekDayLabel, values=c("indianred1", "seagreen", "seagreen", "seagreen", "seagreen", "seagreen", "indianred1")) 
      }
      d
    }
    else{
      fill_var <- NULL
      
      a<-ggplot(data=p_all_data, aes(factor(year(ymd(newDates))), rides, fill=factor(year(ymd(newDates))))) +
        geom_bar(stat="identity") + labs(x = "Year", y = "Rides", title = paste(p_station,"Rides per Year"), fill="Years") +
        theme(axis.text=element_text(size=6), legend.position = "none")
      
      b<-ggplot(data=uic2021, aes(newDates, rides, fill=season) ) +
        geom_bar(stat="identity") + 
        labs(x = paste("Days in", p_year), y = "Rides", title = paste(p_station,"Rides per Day in",p_year))+
        scale_fill_manual(labels=seasons_label, values=seasons_color) +
        theme(legend.position = "none")
      
      c<-ggplot(data=uic2021, aes(factor(month(ymd(newDates))), as.numeric(rides), 
                                  fill=fill_var)) +
        geom_bar(stat="identity") + 
        scale_x_discrete(limit = factor(monthLimit), labels=monthLabel) +
        theme(legend.position = "none")
      
      d<-ggplot(data=uic2021, aes(factor(wday(newDates)), rides, fill=factor(wday(newDates)))) + 
        geom_bar(stat="identity") + 
        labs(x = paste("Weekdays in", p_year), y = "Rides", title = paste(p_station,"Rides per Weekday in", p_year), fill="Weekday") +
        scale_x_discrete(limit = factor(weekDayLimit), labels=weekDayLabel) +
        scale_fill_discrete(name = "Weekday", labels = weekDayLabel) +
        theme(legend.position = "none")
      if(p_station == "UIC-Halsted"){
        fill_var <- uic2021$school
        d <- d + scale_fill_manual(labels=weekDayLabel, values=c("indianred1", "seagreen", "seagreen", "seagreen", "seagreen", "seagreen", "indianred1")) 
        c <- c + labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="School")+
          scale_fill_manual(labels=c("In School", "On Break"), values=c("seagreen", "indianred1"))
        
      }
      else{
        c <- c + labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="Season")+
          scale_fill_manual(labels=seasons_label, values=seasons_color)
        fill_var <- uic2021$season
        
        
        
      }
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
      p_all_data = cermak
    }
    
    uic2021 <- p_data
    monthLimit = c(1,2,3,4,5,6,7,8,9,10,11,12)
    monthLabel <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    monthColor <- c("red","red","red","red","red","red","red","red","red","red","red","red")
    weekDayLimit = c(1,2,3,4,5,6,7)
    weekDayLabel <- c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
    seasons_label <- c("Spring", "Summer", "Fall", "Winter")
    seasons_color <- c("gold2","palegreen3","sienna1","blue")
    
    #UIC by year
    if(p_year_select == "All Years"){
      a<-ggplot(data=p_all_data, aes(factor(year(ymd(newDates))), rides, fill=factor(year(ymd(newDates))))) +
        geom_bar(stat="identity") + labs(x = "Year", y = "Rides", title = paste(p_station,"Rides per Year"), fill="Years") +
        theme(axis.text=element_text(size=6))
      a
    }
    else if(p_year_select == "Each Day"){
      b<-ggplot(data=uic2021, aes(newDates, rides, fill=season) ) +
        geom_bar(stat="identity") + 
        labs(x = paste("Days in", p_year), y = "Rides", title = paste(p_station,"Rides per Day in",p_year))+
        scale_fill_manual(labels=seasons_label, values=seasons_color)
      b
    }
    else if(p_year_select == "Each Month"){
      fill_var <- NULL
      if(p_station == "UIC-Halsted"){
        fill_var <- uic2021$school
      }
      else if(p_station == "O'Hare Airport"){
        fill_var <- uic2021$season
      }
      else{
        fill_var <- uic2021$season
      }
      c<-ggplot(data=uic2021, aes(factor(month(ymd(newDates))), as.numeric(rides), 
                                  fill=fill_var)) +
        geom_bar(stat="identity") + 
        scale_x_discrete(limit = factor(monthLimit), labels=monthLabel) 
      #Adjust colors for UIC-Halsted 
      if(p_station == "UIC-Halsted"){
        c <- c + labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="School")+
          scale_fill_manual(labels=c("In School", "On Break"), values=c("seagreen", "indianred1"))
      }
      else {
        c <- c + labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="Season")+
          scale_fill_manual(labels=seasons_label, values=seasons_color)
      }
      c
    }
    else if(p_year_select == "Each Day of Week"){
      d<-ggplot(data=uic2021, aes(factor(wday(newDates)), rides, fill=factor(wday(newDates)))) + 
        geom_bar(stat="identity") + 
        labs(x = paste("Weekdays in", p_year), y = "Rides", title = paste(p_station,"Rides per Weekday in", p_year), fill="Weekday") +
        scale_x_discrete(limit = factor(weekDayLimit), labels=weekDayLabel) +
        scale_fill_discrete(name = "Weekday", labels = weekDayLabel)
      if(p_station == "UIC-Halsted"){
        d <- d + scale_fill_manual(labels=weekDayLabel, values=c("indianred1", "seagreen", "seagreen", "seagreen", "seagreen", "seagreen", "indianred1")) 
      }
      d
    }
    else{
      fill_var <- NULL
      
      a<-ggplot(data=p_all_data, aes(factor(year(ymd(newDates))), rides, fill=factor(year(ymd(newDates))))) +
        geom_bar(stat="identity") + labs(x = "Year", y = "Rides", title = paste(p_station,"Rides per Year"), fill="Years") +
        theme(axis.text=element_text(size=6), legend.position = "none")
      
      b<-ggplot(data=uic2021, aes(newDates, rides, fill=season) ) +
        geom_bar(stat="identity") + 
        labs(x = paste("Days in", p_year), y = "Rides", title = paste(p_station,"Rides per Day in",p_year))+
        scale_fill_manual(labels=seasons_label, values=seasons_color) +
        theme(legend.position = "none")
      
      c<-ggplot(data=uic2021, aes(factor(month(ymd(newDates))), as.numeric(rides), 
                                  fill=fill_var)) +
        geom_bar(stat="identity") + 
        scale_x_discrete(limit = factor(monthLimit), labels=monthLabel) +
        theme(legend.position = "none")
      
      d<-ggplot(data=uic2021, aes(factor(wday(newDates)), rides, fill=factor(wday(newDates)))) + 
        geom_bar(stat="identity") + 
        labs(x = paste("Weekdays in", p_year), y = "Rides", title = paste(p_station,"Rides per Weekday in", p_year), fill="Weekday") +
        scale_x_discrete(limit = factor(weekDayLimit), labels=weekDayLabel) +
        scale_fill_discrete(name = "Weekday", labels = weekDayLabel) +
        theme(legend.position = "none")
      if(p_station == "UIC-Halsted"){
        fill_var <- uic2021$school
        d <- d + scale_fill_manual(labels=weekDayLabel, values=c("indianred1", "seagreen", "seagreen", "seagreen", "seagreen", "seagreen", "indianred1")) 
        c <- c + labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="School")+
          scale_fill_manual(labels=c("In School", "On Break"), values=c("seagreen", "indianred1"))
        
      }
      else{
        c <- c + labs(x = paste("Months in",p_year), y = "Rides", title = paste(p_station,"Rides per Month in", p_year), fill="Season")+
          scale_fill_manual(labels=seasons_label, values=seasons_color)
        fill_var <- uic2021$season
        
        
        
      }
      grid.arrange(a,b,c,d,nrow=2)
    }
    
  })
}

shinyApp(ui = ui, server = server)
