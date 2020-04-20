# Load required packages
library(shiny)
library('flexdashboard')
library('geosphere')
library("shinyalert")
library('lubridate')
library('dplyr')
library(ECharts2Shiny)
library(timevis)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(tidyverse)
library(plotly)

# Google API key that allows to extract Maps
geo_key = {
  "nerate your own"
}
register_google(key = geo_key)


loc4 <- read.csv("loc4.csv", stringsAsFactors = FALSE)
date_min <- min(loc4$start_datetime)
date_max <- max(loc4$end_datetime)


#Storing Dallas map layout
loc4 <- separate(loc4, start_datetime, c("startdate", "startdatetime"), sep = " ")
loc4 <- separate(loc4, end_datetime, c("enddate", "enddatetime"), sep = " ")

loc4$startdate <- as.Date(loc4$startdate, origin="1970-01-01")
loc4$enddate <- as.Date(loc4$enddate, origin="1970-01-01")

loc6 <- subset(loc4, save_ratio >0, select = c(round_flon:Freq))


#UI function begins here.---------------------------
ui <- dashboardPage(
  
  #try adding navigation bar that scrolls like in a markdown acts as a dropdown
  #navbarPage("Hey!"),
  skin = "green",
  
  dashboardHeader(title = "BALC Project - Travel history", titleWidth = 1350),
  dashboardSidebar(width = "80px"),
  dashboardBody(
   
    h3("TRAVEL HISTORY: Summary"),
    actionButton("OUTPUT", label = "Result", width = 250),
    
    tags$b(style = "font-size: 20px;"),
    fixedRow(
      dateRangeInput("dates", 
                     "Select data range:",
                     start = date_min, 
                     end = date_max,
                     min = date_min,
                     max = date_max),
      textOutput("DateRange1"),
      textOutput("DateRange2"),
                 
        ),
    
    

    
    h3("Histograms:"),
    
    plotOutput("histogram1", width = "60%", height = "300px"),
  

fixedRow(
          column(1, offset = 0,  sliderInput("duration_range", "Duration Slider", 
                                             value = c(0,50), #mins
                                             min =   0, #mins
                                             max = 300, #mins
                                             step = 1, width = "1100px", animate = TRUE),width = 7,height = 125, status = "info"),
      ),

plotOutput("histogram2", width = "60%", height = "300px"),




fixedRow(
  column(1, offset = 0,  sliderInput("distance_range", "Distance",
                                     value = c(0,5), #Kms
                                     min =  0, #Kms
                                     max = 310, #Kms
                                     step = 1, width = "1100px", animate = TRUE),width = 7,height = 125, status = "info"),
       
      ),

plotOutput("histogram3", width = "60%", height = "300px"),

h3("Travel Profile"),


fixedRow(
  infoBoxOutput("Box1", width = 3),
  infoBoxOutput("Box2", width = 3),
  infoBoxOutput("Box3", width = 3),
  infoBoxOutput("Box4", width = 3)
),

h3("Distance Summary:"),


    fixedRow(
             box(
                infoBoxOutput("Stat0", width = 3),
                infoBoxOutput("Stat1", width = 3),
                infoBoxOutput("Stat2", width = 3),
                infoBoxOutput("Stat3", width = 3), width = 250,height = 125, status = "primary",solidHeader = T
                )
            ),

h3("Duration Summary: BEFORE buying a car"),


    fixedRow(
              box(
                infoBoxOutput("Stat3.1", width = 3),
                infoBoxOutput("Stat4", width = 3),
                infoBoxOutput("Stat5", width = 3),
                infoBoxOutput("Stat6", width = 3), width = 250,height = 125, status = "info",solidHeader = T
                 )
            ),
h3("Duration Summary: Time saved by buying a car"),
fixedRow(
  box(
    infoBoxOutput("Stat7", width = 3),
    infoBoxOutput("Stat8", width = 3),
    infoBoxOutput("Stat9", width = 3),
    infoBoxOutput("Stat10", width = 3), width = 250,height = 125, status = "info",solidHeader = T
  )
),
    fixedRow(
            column(1, offset = 1, plotlyOutput("gauge1")),
           # useShinyalert(),
           # column(2, offset = 3, actionButton("actionbutton", label = "ALOHA", width = 200)),
        #     helpText("When you click the button above, you should see",
        #              "the output below update to reflect the value you",
        #              "entered at the top:"),
           # column(2, p("Click the button to update the value displayed in the main panel.")),
            column(2,offset = 2, plotlyOutput("gauge"))
      
           ),


 sliderInput("zoom_range", "Map Zoom:",
             1, 16, 7, step = 1),

h3("The Map Plot - Dallas"),

plotOutput("map_plot"),



useShinyalert(),  # Set up shinyalert

  textOutput("From_date"),
  textOutput("Mean_Hist"),
  textOutput("To_date"),
  textOutput("Difference")

  #timevisOutput("timeline")
)
)

#Server function begins here ---------------------------

server <- function(input, output, session) {
  
  DateRange1 <- reactive(as.Date(input$dates[1], origin = "1970-01-01"))
  DateRange2 <- reactive(as.Date(input$dates[2], origin = "1970-01-01"))
    
  observeEvent(input$OUTPUT, {
   # loc5 <- subset(loc4, startdate >= DateRange1() & enddate <= DateRange1(), select = round_flon:Freq)
    loc5 <- subset(loc4, 
                   startdate >= DateRange1() & enddate <= DateRange2() & save_ratio>=0 & transit_time >= (input$duration_range[1]*60) & transit_time <= (input$duration_range[2]*60) & transit_dist >= (input$distance_range[1]*1000) & transit_dist <= (input$distance_range[2]*1000) , 
                   select = round_flon:Freq)

  tdist <- sum(loc5$transit_dist, na.rm = TRUE) #Total Distance
  ttime <- sum(loc5$transit_time, na.rm = TRUE) #Transit time
  dtime <- sum(loc5$drive_time, na.rm = TRUE) #Drive time
  journ <- nrow(loc5) # No of Journeys

  total_journey <- journ
  total_transit_speed <- (tdist/ttime)*(3.60)
  total_drive_speed <- (tdist/dtime)*(3.60)
  #total_save_ratio <- (ttime-dtime)/ttime
  
  # updateSliderInput(session, "duration_range", value = c(round(min(loc5$transit_time)*0.1666667,2),round(max(loc5$transit_time)*0.1666667,2)),
  #                 min =   round(min(loc5$transit_time)*0.1666667,2), max = round(max(loc5$transit_time)*0.1666667,2), step = 5)
  # 
  # updateSliderInput(session, "distance_range", value = c(round(min(loc5$transit_dist)*0.001,2),round(max(loc5$transit_dist)*0.001,2)),
  #                  min =  round(min(loc5$transit_dist)*0.001,2), max = round(max(loc5$transit_dist)*0.001,2), step = 1)

 diff_cal_day <- abs(as.numeric((DateRange1() - DateRange2())))

 
  # try adding value box later
  output$Box1 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "NUMBER OF JOURNEYS"), value = total_journey, 
            color = "red", icon= icon("stopwatch"), 
            width = 4, fill = FALSE)
    })
  
  #total_transit_speed?
  output$Box2 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", 
                           "AVG TRANSIT SPEED"),
            value =  round(total_transit_speed,2), subtitle = "(kmph)", 
            color = "green", icon= icon("tachometer-alt"), 
            width = 4, fill = FALSE)
  })
  
  #total_drive_speed?
  output$Box3 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "AVG DRIVE SPEED"),value =  round(total_drive_speed,2), subtitle = "(kmph)", 
            color = "yellow", icon= icon("tachometer-alt"), 
            width = 4, fill = FALSE)
  })
  
  #total_save_ratio?
  output$Box4 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "TOTAL SAVE RATIO"),value =  round(mean(loc5$save_ratio, na.rm = TRUE),2), subtitle = " ", 
            color = "blue", icon= icon("balance-scale"), 
            width = 4, fill = FALSE)
  })
  

  
  avg_dist_perday <- (tdist/diff_cal_day)*0.001
  avg_dist_perweek <- avg_dist_perday*7
  avg_dist_permonth <- avg_dist_perday*30
  # 
  avg_time_perday <- (sum(loc5$transit_time)/diff_cal_day)*0.000277778
  avg_time_perweek <- avg_time_perday*7
  avg_time_month <- avg_time_perday*30
  
  #avg_Total_distance?
  
  output$Stat0 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "TOTAL DISTANCE"),value =  abs(round(sum(loc5$transit_dist)*0.001,2)), subtitle = "(km)", 
            color = "green", icon= icon("road"),
            width = 4, fill = FALSE)
  })
  
  #avg_dist_perDay?

  output$Stat1 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "AVG. DAILY"),value =  abs(round(avg_dist_perday,2)), subtitle = "(km)", 
            color = "blue", icon= icon("road"),
            width = 4, fill = FALSE)
  })
  
  #avg_dist_perWeek?
  output$Stat2 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "AVG. WEEKLY"),value =  abs(round(avg_dist_perweek,2)), subtitle = "(km)", 
            color = "blue", icon= icon("road"),
            width = 4, fill = FALSE)
  })
  
  #avg_dist_perMonth?
  output$Stat3 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "AVG. MONTHLY"),value =  abs(round(avg_dist_permonth,2)), subtitle = "(km)", 
            color = "blue", icon= icon("road"),
            width = 4, fill = FALSE)
  })
  
  #TOTAL Duration?
  output$Stat3.1 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "TOTAL DURATION"),value =  abs(round(sum(loc5$transit_time)/3600,2)), subtitle = "(Hours)", 
            color = "green", icon = icon("clock"),
            width = 4, fill = FALSE)
  })
  
  #avg_TIME_perDay?
  output$Stat4 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "AVG. DAILY"),value =  abs(round(avg_time_perday,2)), subtitle = "(Hours)", 
            color = "light-blue", icon = icon("clock"),
            width = 4, fill = FALSE)
  })
  
  #avg_TIME_perWeek?
  output$Stat5 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "AVG. WEEKLY"),value =  abs(round(avg_time_perweek,2)), subtitle = "(Hours)", 
            color = "light-blue", icon= icon("clock"), 
            width = 4, fill = FALSE)
  })
  
  #avg_TIME_perMonth?
  output$Stat6 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "AVG. MONTHLY"),value =  abs(round(avg_time_month,2)), subtitle = "(Hours)", 
            color = "light-blue", icon= icon("clock"), 
            width = 4, fill = FALSE)
  })
  #------------------------------------------------
  #Save ratio multiplied to duration variables
  #TOTAL Duration?
  
  duration_save_ratio_factor <- round(mean(loc5$save_ratio, na.rm = TRUE),2)
  
  output$Stat7 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "TOTAL DURATION: "),value =  abs(round((sum(loc5$transit_time)*duration_save_ratio_factor)/3600,2)), subtitle = "(Hours)", 
            color = "green", 
            width = 4, fill = FALSE)
  })
  
  #avg_TIME_perDay?
  output$Stat8 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "AVG. DAILY"),value =  abs(round(avg_time_perday*duration_save_ratio_factor,2)), subtitle = "(Hours)", 
            color = "aqua", icon= icon("clock"), 
            width = 4, fill = FALSE)
  })
  
  #avg_TIME_perWeek?
  output$Stat9 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "AVG. WEEKLY"),value =  abs(round(avg_time_perweek*duration_save_ratio_factor,2)), subtitle = "(Hours)", 
            color = "aqua", icon= icon("clock"), 
            width = 4, fill = FALSE)
  })
  
  #avg_TIME_perMonth?
  output$Stat10 <- renderValueBox ({
    infoBox(title = tags$b(style = "font-size: 16px;", "AVG. MONTHLY"),value =  abs(round(avg_time_month*duration_save_ratio_factor,2)), subtitle = "(Hours)", 
            color = "aqua", icon= icon("clock"), 
            width = 4, fill = FALSE)
  })
 # loc5$Freq >= input$freq_range[1] & loc5$Freq <= input$freq_range[2])$save_ratio
  #the histograms_1
  output$histogram1 <- renderPlot({
    hist(loc5$transit_time*0.0166666666667,
         breaks=20,
         xlab = "Time (in mins)",
         main = "Time Duration Distribution")
  })
  
  output$histogram2 <- renderPlot({
    hist(loc5$transit_dist*0.001,
         breaks=20,
         xlab = "Distance (in km)",
         main = "Distance Distribution")
  })
  
  
  
  mean_hist <- mean(loc5$save_ratio, na.rm = TRUE)
  output$histogram3 <- renderPlot({
    hist(loc5$save_ratio,
         breaks=20,
         xlab = paste("Average percentage time saved: ", round(mean_hist,2)),
         main = "Save Ratio")
  })
  
  
  output$mean_hist <- renderText(mean_hist)

  output$gauge = renderPlotly({
    p <- plot_ly(
      domain = 100,
      value = total_drive_speed,
      width = 250,
      height = 200,
      fillcolor = "red",
     # frame = c(0:total_drive_speed),
      title = list(text = "With car kmph"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = total_transit_speed),
      gauge = list(
        axis =list(range = list(NULL, 25)),
        steps = list(
          list(range = c(0, 19), color = "lightgray"),
          list(range = c(20, 25), color = "lightgray")),
        threshold = list(
          line = list(color = "lightgray", width = 4),
          thickness = 0.75,
          value = 100))) %>%
      layout(margin = list(l=25,r=25))
  })  

  # output$gauge2 = renderPlotly({
  #   p <- plot_ly(
  #     value = total_transit_speed-total_drive_speed,
  #     title = list(text = "Increase speed in car by"),
  #   )
  # })
  # 
  # 

  output$gauge1 = renderPlotly({
    p <- plot_ly(
      domain = 100,
      value = total_transit_speed,
      width = 250,
      height = 200,
      colors = "#000000",
      title = list(text = "Without car kmph"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = total_drive_speed),
      gauge = list(
        axis =list(range = list(NULL, 25)),
        steps = list(
          list(range = c(0, 19), color = "lightgray"),
          list(range = c(20, 25), color = "lightgray")),
        threshold = list(
          line = list(color = "lightgray", width = 4),
          thickness = 0.75,
          value = 100))) %>%
      layout(margin = list(l=25,r=25))
  })
  
  output$map_plot <- renderPlot({
    dallas <- get_map(location = c(lon = mean(loc5$from_lon), lat = mean(loc5$from_lat)), zoom = input$zoom_range,
                      maptype = "roadmap", source = "google")
    ggmap(dallas) + geom_segment(data = loc5, 
                                   aes(x=from_lon, y=from_lat, xend=to_lon, yend=to_lat, size=Freq, color = save_ratio))
  }, height = 650, width = 1400)
  

  # ntext <- eventReactive(input$actionbutton, {
  #   title <- total_transit_speed
  # })
  
  speed_difference <- total_drive_speed - total_transit_speed
  
  #output$nText <- renderPrint(shinyalert(title = ntext(), text = "This is to show annoying popups are possible here!", type = "success", confirmButtonText = "OK"))

    
    # ifelse((total_transit_speed>total_drive_speed), 
    # shinyalert(title = "aheh"  , text = "This is to show annoying popups are possible here!" %+% round(speed_difference(),2), type = "success", confirmButtonText = "OK"),            
    # shinyalert(title = "aheh"  , text = "This is to show annoying popups are possible here!" %+% round(speed_difference(),2), type = "success", confirmButtonText = "OK"))
    # 
    # 
  })
  
}

shinyApp(ui, server)
