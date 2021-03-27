library(httpuv)
library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
#library(quantreg)
#library(shinythemes)
#library(colourpicker)
#library(basicTrendline)
library(shinydashboard)
#library("flexdashboard")
library(png)
library(jpeg)
#library(directlabels)
library(scales)
library(shinycssloaders)
library(shinyWidgets)
library(gridExtra)
library(grid)
library(gridGraphics)
library(devtools)
library(lubridate)

list.of.packages <- c("httpuv", "shiny", "ggplot2", "tidyverse", "dplyr", "shinydashboard", "png", "jpeg", "scales", "shinycssloaders", "shinyWidgets", "gridExtra", "grid", "gridGraphics", "devtools", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

Sys.setenv(LANG = "en")
#Sys.setlocale("LC_ALL","English")

#tiFile <- read.csv("p02ti.csv", stringsAsFactors = FALSE)
#scoresFile <- read.csv("p02scores.csv", stringsAsFactors = FALSE, sep="\t")
#adhFile <- read.csv("p02adh.csv", stringsAsFactors = FALSE, sep="\t")
#pdFile <- read.csv("p02pd.csv", stringsAsFactors = FALSE, sep="\t")

scores_directory="data/ball_game_acc"
ti_directory="data/TIP"
adh_directory="data/medication_adherence"
pd_directory="data/health"
drawings1_directory="data/Drawing_data_for_R.csv"
merged_directoy="data/screenshots/JPG/merged"
spiral_pd_directoy="data/screenshots/JPG/spiral/pd"
square_pd_directoy="data/screenshots/JPG/square/pd"
photos_directoy="data/photos"

#if (interactive()) {

ui <- dashboardPage(
  #skin = "red",
  dashboardHeader(title = "STOP"),
  #dashboardHeader(title = "STOP", titleWidth = 350),
  
  dashboardSidebar(
    #width = 350,
    
    sidebarMenu(id = "sidebar",
                #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                #                           label = "Type patient number"),
                #textInput("search"),
                #textOutput("text2"),
                #selectInput("list_of_patients", "Patient number:",
                #           c("Select", 1:13)),
                selectInput("list_of_patients", "Patient number:",
                            c("Select", 1:13), size = 13, selectize=FALSE),
                
                #sidebarMenuOutput("sidebar_search_content"),
                #sidebarUserPanel(name="a", subtitle = NULL, image = NULL),
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Medication", icon = icon("th"), tabName = "medication"),
                menuItem("Symptoms", icon = icon("th"), tabName = "symptoms"),
                menuItem("Drawings Error", icon = icon("th"), tabName = "drawings1"),
                menuItem("Drawings", icon = icon("th"), tabName = "drawings2"),
                menuItem("Tremor Intensity Parameter", icon = icon("th"), tabName = "games_tip"),
                menuItem("Game scores", icon = icon("th"), tabName = "game_scores")
    )
  ),
  dashboardBody(
    
    #titlePanel("Game date and TIP"),
    fluidRow(
      #column(1, textOutput("text99")),
      column(3, textOutput("text")),
      column(5, textOutput("text990")),
      #column(2, textOutput("text2")),
      column(4, dateRangeInput(
        inputId = "daterange1",
        label = NULL,
        start = as.Date('2018-7-16'), # The initial start date. Either a Date object, or a string in yyyy-mm-dd format
        end = as.Date('2018-7-22'), # The initial end date. Either a Date object, or a string in yyyy-mm-dd format 
        min = as.Date('2018-5-5'), # The minimum allowed date. Either a Date object, or a string in yyyy-mm-dd format.
        max = Sys.Date(), # The maximum allowed date. Either a Date object, or a string in yyyy-mm-dd format.
        format = "yyyy/mm/dd",  # The format of the date to display in the browser. try "mm/dd/yy"  
        separator = "to" # String to display between the start and end input boxes. try "to"
      ))
      #column(4, checkboxInput("input1Id", label = "Order by highest TIP")),
      #column(4, colourInput("col9", "Select colour", "red"))
    ),
    #if (input$sidebar == "dashboard") {
    conditionalPanel(
      condition = "input.sidebar == 'dashboard' && (input.list_of_patients == 'Select'||input.list_of_patients == 0)",
      fluidRow(
        box(title = "Medication", status = "primary", solidHeader = TRUE, height = 308,
            collapsible = TRUE, withSpinner(htmlOutput("text1"))),
        box(title = "Symptoms", status = "primary", solidHeader = TRUE, height = 308,
            collapsible = TRUE, withSpinner(htmlOutput("text2")))
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot3"))),
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot4")))
      ),
      fluidRow(
        #column(6, box(plotOutput("plot1"))),
        #column(6, box(plotOutput("plot2")))
        tabBox(title = "Drawings Error", id = "tabset", height = 308,
               tabPanel("Spiral", withSpinner(htmlOutput("text3"))),
               tabPanel("Square", withSpinner(htmlOutput("text33")))),
        #status = "primary", solidHeader = TRUE, collapsible = TRUE, 
        tabBox(
          title = "Drawings",
          id = "tabset1", height = 308,
          #tabPanel("Spiral", plotOutput("plot4", height = 245)),
          tabPanel("Spiral", withSpinner(htmlOutput("text4"))),
          #tabPanel("Square", plotOutput("plot400", height = 245)),
          tabPanel("Square", withSpinner(htmlOutput("text44")))
        )
        #box(title = "Drawings 2", status = "primary", solidHeader = TRUE,
        #   collapsible = TRUE, plotOutput("plot4", height = 245))
      ),
      fluidRow(
        #column(6, box(plotOutput("plot1"))),
        #column(6, box(plotOutput("plot2")))
        box(title = "Tremor Intensity Parameter", status = "primary", solidHeader = TRUE, height = 308,
            collapsible = TRUE, withSpinner(htmlOutput("text5"))),
        box(title = "Game scores", status = "primary", solidHeader = TRUE, height = 308,
            collapsible = TRUE, withSpinner(htmlOutput("text6")))
      )
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'dashboard' && input.list_of_patients != 'Select' && input.list_of_patients != 0",
      fluidRow(
        box(title = "Medication", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, withSpinner(plotOutput("plot1", height = 245))),
        box(title = "Symptoms", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, withSpinner(plotOutput("plot2", height = 245)))
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot3"))),
        #column(6, box(title = "Histogram", status = "primary", solidHeader = TRUE,
        #             collapsible = TRUE, plotOutput("plot4")))
      ),
      fluidRow(
        #column(6, box(plotOutput("plot1"))),
        #column(6, box(plotOutput("plot2")))
        tabBox(title = "Drawings Error", id = "tabset",
               tabPanel("Spiral", withSpinner(plotOutput("plot3", height = 245))),
               tabPanel("Square", withSpinner(plotOutput("plot33", height = 245)))),
        #status = "primary", solidHeader = TRUE, collapsible = TRUE, 
        tabBox(
          title = "Drawings",
          id = "tabset1",
          #tabPanel("Spiral", plotOutput("plot4", height = 245)),
          tabPanel("Spiral", withSpinner(plotOutput("plot4", height = 245))),
          #tabPanel("Square", plotOutput("plot400", height = 245)),
          tabPanel("Square", withSpinner(plotOutput("plot44", height = 245)))
        )
        #box(title = "Drawings 2", status = "primary", solidHeader = TRUE,
        #   collapsible = TRUE, plotOutput("plot4", height = 245))
      ),
      fluidRow(
        #column(6, box(plotOutput("plot1"))),
        #column(6, box(plotOutput("plot2")))
        box(title = "Tremor Intensity Parameter", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, withSpinner(plotOutput("plot5", height = 245))),
        box(title = "Game scores", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, withSpinner(plotOutput("plot6", height = 245)))
      )
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'medication' && (input.list_of_patients == 'Select'||input.list_of_patients == 0)",
      fluidRow(
        column(12, box(width=12, title = "Medication", status = "primary", solidHeader = TRUE, height = 603,
                       collapsible = TRUE, withSpinner(htmlOutput("text10")))
        ))),
    conditionalPanel(
      condition = "input.sidebar == 'medication' && input.list_of_patients != 'Select' && input.list_of_patients != 0",
      fluidRow(
        column(12, box(width=12, title = "Medication", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE, withSpinner(plotOutput("plot10", height = 540)))
        ))),
    
    conditionalPanel(
      condition = "input.sidebar == 'symptoms' && (input.list_of_patients == 'Select'||input.list_of_patients == 0)",
      fluidRow(
        column(12, box(width=12, title = "Symptoms", status = "primary", solidHeader = TRUE, height = 603,
                       collapsible = TRUE, withSpinner(htmlOutput("text20")))
        ))),
    conditionalPanel(
      condition = "input.sidebar == 'symptoms' && input.list_of_patients != 'Select' && input.list_of_patients != 0",
      fluidRow(
        column(12, box(width=12, title = "Symptoms", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE, withSpinner(plotOutput("plot20", height = 540)))
        ))),
    
    conditionalPanel(
      condition = "input.sidebar == 'drawings1' && (input.list_of_patients == 'Select'||input.list_of_patients == 0)",
      fluidRow(
        column(12,
               tabBox(width=NULL,
                      title = "Drawings Error",
                      id = "tabset2", height = 603,
                      tabPanel("Spiral", withSpinner(htmlOutput("text30"))),
                      tabPanel("Square", withSpinner(htmlOutput("text330")))
               )
        ))),
    conditionalPanel(
      condition = "input.sidebar == 'drawings1' && input.list_of_patients != 'Select' && input.list_of_patients != 0",
      fluidRow(
        column(12,
               tabBox(width=NULL,
                      title = "Drawings Error",
                      id = "tabset2",
                      tabPanel("Spiral", withSpinner(plotOutput("plot30", height = 540))),
                      tabPanel("Square", withSpinner(plotOutput("plot330", height = 540)))
               )
        ))),
    
    conditionalPanel(
      condition = "input.sidebar == 'drawings2' && (input.list_of_patients == 'Select'||input.list_of_patients == 0)",
      fluidRow(
        column(12,
               tabBox(width = NULL,
                      title = "Drawings",
                      id = "tabset2", height = 603,
                      #tabPanel("Spiral", plotOutput("plot40000", height = 540)),
                      tabPanel("Spiral", withSpinner(htmlOutput("text40"))),
                      #tabPanel("Square", plotOutput("plot4000000", height = 540)),
                      tabPanel("Square", withSpinner(htmlOutput("text440")))
               )
        )
        
        #column(12, box(width=12, title = "Drawings 1", status = "primary", solidHeader = TRUE,
        #              collapsible = TRUE, plotOutput("plot40", height = 540))
      )),
    conditionalPanel(
      condition = "input.sidebar == 'drawings2' && input.list_of_patients != 'Select' && input.list_of_patients != 0",
      fluidRow(
        column(12,
               tabBox(width = NULL,
                      title = "Drawings",
                      id = "tabset2",
                      #tabPanel("Spiral", plotOutput("plot40000", height = 540)),
                      tabPanel("Spiral", withSpinner(plotOutput("plot40", height = 1130))),
                      #tabPanel("Square", plotOutput("plot4000000", height = 540)),
                      tabPanel("Square", withSpinner(plotOutput("plot440", height = 1130)))
               )
        )
        
        #column(12, box(width=12, title = "Drawings 1", status = "primary", solidHeader = TRUE,
        #              collapsible = TRUE, plotOutput("plot40", height = 540))
      )),
    
    conditionalPanel(
      condition = "input.sidebar == 'games_tip' && (input.list_of_patients == 'Select'||input.list_of_patients == 0)",
      fluidRow(
        column(12, box(width=12, title = "Tremor Intensity Parameter", status = "primary", solidHeader = TRUE, height = 603,
                       collapsible = TRUE, withSpinner(htmlOutput("text50")))
        ))),
    conditionalPanel(
      condition = "input.sidebar == 'games_tip' && input.list_of_patients != 'Select' && input.list_of_patients != 0",
      fluidRow(
        column(12, box(width=12, title = "Tremor Intensity Parameter", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE, withSpinner(plotOutput("plot50", height = 540)))
        ))),
    
    conditionalPanel(
      condition = "input.sidebar == 'game_scores' && (input.list_of_patients == 'Select'||input.list_of_patients == 0)",
      fluidRow(
        column(12, box(width=12, title = "Game scores", status = "primary", solidHeader = TRUE, height = 603,
                       collapsible = TRUE, withSpinner(htmlOutput("text60")))
        ))),
    conditionalPanel(
      condition = "input.sidebar == 'game_scores' && input.list_of_patients != 'Select' && input.list_of_patients != 0",
      fluidRow(
        column(12, box(width=12, title = "Game scores", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE, withSpinner(plotOutput("plot60", height = 540)))
        )))
    
  )
)


server <- function(input, output, session) {
  
  
  
  
  # 1. Trick file date creation update
  onStop(function() {
    # 1. File name
    p <- paste0(getwd(), "app.R")
    # 2. Update file 'date creation'
    Sys.setFileTime(p, Sys.time())
  }) # onStop
  
  #output$text2 <- renderText({
  #  a = input$list_of_patients
  # a
  #})
  
  
  output$text <- renderText({
    b=input$list_of_patients
    if (b=="Select"||b==0) {
      b=""
    }
    a=paste("Selected patient: ", b, sep=" ")
    #a=input$sidebar
    a
  })
  
  output$text990 <- renderText({
    b=input$list_of_patients
    if (b=="Select"||b==0) {
      active_time_for_patient=""
    } else {
      c=paste("p0", b, sep="")
      if (as.numeric(b)>9) {
        c=paste("p", b, sep="")
      }
      d=paste(c, "csv", sep = ".")
      a=paste(scores_directory, d, sep = "/")
      
      if (file.exists(a)) {
        scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
        first_date=as.Date(first(scoresFile$date_time))
        last_date=as.Date(last(scoresFile$date_time))
        active_time=paste(first_date, last_date, sep = " to ")
        active_time_for_patient=paste("Active period for patient:", active_time, sep=" ")
      } else {
        active_time_for_patient=""
      }
      
      
      #filtered <-
      #scoresFile %>%
      #filter(date_time >= input$daterange1[1],
      #        date_time <= input$daterange1[2])
      #filtered <- filtered[ ,c("score", "date_time")]
      #filtered <- unique(filtered)
      
      
      #dd <- filtered[ ,c("date_time")]
      #dd="active between 1 - 2"
      
      #active_time_for_patient=paste(active_time_for_patient, dd, sep=" ")
      
    }
    
    active_time_for_patient
    
  })
  
  #output$text1 <- renderImage({
   # width  <- session$clientData$output_text1_width
    #height <- session$clientData$output_text1_height
    #filename <- normalizePath(file.path('./data/photos',
     #                                   paste('med_adh', '.png', sep='')))
    #list(src = filename, width = width, height = height)
    #png()
  #}, deleteFile = FALSE)
  
  
  output$text1 <- renderText({
    paste("<h3> <b> The deviation of the medication intake time from the regimen 
            (before of after) in minutes. 15 minutes limit is shown as a red line 
            in the plot. </b>",
          "<br> <br>", "For extended review, press the âMedicationâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text2 <- renderText({
    paste("<h4> <b> The user evaluates the symptom level daily: </b>", "<br>",
          "- No symptoms, excellent day.", "<br>",
          "- Minimal symptoms with no concrete interface with my daily activities.", "<br>",
          "- Mild symptoms that slightly interfered with my daily activities.", "<br>",
          "- Moderate symptoms but they did not stop me from carrying out my daily activities.", "<br>",
          "- Severe symptoms that prevented me from carrying out one or more of my daily activities.",
          "<br> <br>", "For extended review, press the âSymptomsâ side tab.",
          "<br>", "Please select a patient.", "</h4>")
  })
  output$text3 <- renderText({
    paste("<h3> <b> The average error of the spiral drawings in pixels. The trend 
            is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the âDrawings Errorâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text33 <- renderText({
    paste("<h3> <b>The average error of the square drawings in pixels. 
            The trend is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the âDrawings Errorâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text4 <- renderText({
    paste("<h3> <b> The spiral drawings. </b>",
          "<br> <br>", "For extended review, press the âDrawingsâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text44 <- renderText({
    paste("<h3> <b> The square drawings. </b>",
          "<br> <br>", "For extended review, press the âDrawingsâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text5 <- renderText({
    paste("<h3> <b>Tremor Intensity Parameter of the each ball game session. 
            Tremor intensity parameter describes the tremor level during a game session. 
            The trend is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the âTremor Intensity Parameterâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text6 <- renderText({
    paste("<h3> <b>Ball game scores. The trend is shown as a blue line in the plot.</b>",
          "<br> <br>", "For extended review, press the âGame scoresâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  
  
  output$text10 <- renderText({
    paste("<h3> <b> The deviation of the medication intake time from the regimen 
            (before of after) in minutes. 15 minutes limit is shown as a red line 
            in the plot. </b>",
          "<br> <br>", "For extended review, press the âMedicationâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text20 <- renderText({
    paste("<h4> <b> The user evaluates the symptom level daily: </b>", "<br>",
          "- No symptoms, excellent day.", "<br>",
          "- Minimal symptoms with no concrete interface with my daily activities.", "<br>",
          "- Mild symptoms that slightly interfered with my daily activities.", "<br>",
          "- Moderate symptoms but they did not stop me from carrying out my daily activities.", "<br>",
          "- Severe symptoms that prevented me from carrying out one or more of my daily activities.",
          "<br> <br>", "For extended review, press the âSymptomsâ side tab.",
          "<br>", "Please select a patient.", "</h4>")
  })
  output$text30 <- renderText({
    paste("<h3> <b> The average error of the spiral drawings in pixels. The trend 
            is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the âDrawings Errorâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text330 <- renderText({
    paste("<h3> <b>The average error of the square drawings in pixels. 
            The trend is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the âDrawings Errorâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text40 <- renderText({
    paste("<h3> <b> The spiral drawings. </b>",
          "<br> <br>", "For extended review, press the âDrawingsâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text440 <- renderText({
    paste("<h3> <b> The square drawings. </b>",
          "<br> <br>", "For extended review, press the âDrawingsâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text50 <- renderText({
    paste("<h3> <b>Tremor Intensity Parameter of the each ball game session. 
            Tremor intensity parameter describes the tremor level during a game session. 
            The trend is shown as a blue line in the plot. </b>",
          "<br> <br>", "For extended review, press the âTremor Intensity Parameterâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  output$text60 <- renderText({
    paste("<h3> <b>Ball game scores. The trend is shown as a blue line in the plot.</b>",
          "<br> <br>", "For extended review, press the âGame scoresâ side tab.",
          "<br>", "Please select a patient.", "</h3>")
  })
  
  #output$sidebar_search_content <- renderPlot({
  # ggplot()
  #})
  
  
  
  output$plot1 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
      #photo=paste(photos_directoy, "med_adh.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(adh_directory, d, sep = "/")
    
    if (file.exists(a)) {
      adhFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    
    filtered <-
      adhFile %>%
      filter(intake_time >= input$daterange1[1],
             intake_time <= input$daterange1[2])
    filtered <- filtered[ ,c("diff_min", "intake_time")]
    filtered <- unique(filtered)
    
    a <- filtered[ ,c("intake_time")]
    b <- filtered[ ,c("diff_min")]
    mydf = data.frame(Date=a, Diff=b)
    
    date_error = as.numeric(input$daterange1[2]-as.Date(adhFile[c(1),"intake_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(adhFile))
    date_error = as.numeric(as.Date(adhFile[c(maximum),"intake_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    #days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    #if (days_count<1) {
    # stop(safeError("End date should be after start date."))
    
    #}
    
    #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    
    #mydf$day <- factor(strftime(mydf$Date,format="%a"),
     #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    
    #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    
    #mydf$week <- factor(strftime(mydf$Date,format="%V"))
    #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    
    #mydf$month <- strftime(mydf$Date,format="%b")
    #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    
    #mydf$year <- strftime(mydf$Date,format="%Y")
    #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    
    #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
    #days_count = count(mydf2)
    
    
    
    filtered2 <- adhFile
    filtered2 <- filtered2[ ,c("diff_min", "intake_time")]
    filtered2 <- unique(filtered2)
    #print(filtered2)
    
    #print(count(filtered2))
    first_point_intake = filtered2[c(1),2]
    
    count = count(filtered2)
    count = as.numeric(count)
    
    one_count_flag=0
    if (count<1) {
      stop(safeError("Empty data file for the patient."))
    }
    last_point_intake = filtered2[c(count),2]
    if (count==1) {
      count=2
      one_count_flag=1
    }
    
    half_point_intake = round(count/2)
    half_point_intake = as.numeric(half_point_intake)
    half_point_intake = filtered2[c(half_point_intake),2]
    #print(half_point_intake)
    
    if (one_count_flag==1) {
      half_point_intake_plus_one = round(count/2)
    } else {
      half_point_intake_plus_one = round(count/2) + 1
    }
    
    half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
    half_point_intake_plus_one = filtered2[c(half_point_intake_plus_one),2]
    #print(half_point_intake_plus_one)
    
    first_half <- 
      filtered2  %>%
      filter(intake_time >= first_point_intake,
             intake_time <= half_point_intake)
    
    #print(first_half)
    average1 = mean(first_half$diff_min)
    #print(average1)
    
    
    second_half <-
      filtered2  %>%
      filter(intake_time >= half_point_intake_plus_one,
             intake_time <= last_point_intake)
    
    #print(second_half)
    average2 = mean(second_half$diff_min)
    #print(average2)
    
    
    #if (input$input3Id == TRUE) {
    # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$Diff)])
    #}
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      #mydf$Date=as.Date(mydf$Date)
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
      #day_and_date=paste(mydf$day, mydf$ddate, sep = "\n")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = "\n")
    }
    
    
    if (days_count < 8) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      }
      first_date=first(as.Date(mydf$Date))
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_day=last(as.Date(mydf$Date))+ lubridate::days(1)
      last_day=paste(last_day, "00:00:00", sep = " ")
      last_day=as.POSIXct(last_day)
      
      ggplot(mydf, aes(x=Date, y=Diff))+
        scale_x_datetime(breaks = "1 day", minor_breaks = "1 days", labels = date_format("%a\n%d\n%b"),name="Date range (Day)")+
        #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
        #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
        theme_minimal() +
        #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a\n%d\n%b"),name="Day")+
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Day)") + ylab("Time deviation (minutes)") +
        expand_limits(x=c(first_date,last_day))+
        #scale_y_continuous(breaks = seq(0, 10000, 5)) +
        #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(0.5,"cm"))) +
        #annotate("text", x = last(mydf$day), y = (as.numeric(average2)+20), label = "Trend line of time adherence")+
        geom_hline(yintercept=15, color = "Red", size = 1)
        #geom_text(aes(last(day),15,label = "15 Minutes", vjust = "top", hjust="right"), size=3)
        #geom_text(aes(last(day_and_date),15,label = "15 Minutes", vjust = 1))
    }
    
    else if (days_count < 29) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(7)
      }
      first_date=cut(first(as.Date(mydf$Date)), "week")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_week=last(as.Date(mydf$Date))+ lubridate::days(7)
      last_week=paste(last_week, "00:00:00", sep = " ")
      last_week=as.POSIXct(last_week)
      
      ggplot(mydf, aes(x=Date, y=Diff)) +
        scale_x_datetime(breaks = "1 week",minor_breaks = "1 weeks",labels = date_format("%W"),name="Date range (Week)")+
        #scale_x_date(labels=date_format("%a"))+
        #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a\n%d"),name="")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
        geom_point() + xlab("Date range (Week)") + ylab("Time deviation (minutes)") +
        expand_limits(x=c(first_date,last_week))+
        #scale_y_continuous(breaks = seq(0, 10000, 5)) +
        #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(0.5,"cm")))+
        geom_hline(yintercept=15, color = "Red", size = 1)
        #geom_text(aes(last(week),15,label = "15 Minutes", vjust = 1))
    }
    
    else if (days_count > 28 & days_count < 366 ) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate:::months.numeric(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "month")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_month=last(as.Date(mydf$Date))%m+% lubridate:::months.numeric(1)
      last_month=paste(last_month, "00:00:00", sep = " ")
      last_month=as.POSIXct(last_month)
      
      ggplot(mydf, aes(x=Date, y=Diff)) +
        scale_x_datetime(breaks = "1 month",minor_breaks = "1 months",labels = date_format("%b"),name="Date range (Month)")+
        #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
        geom_point() + xlab("Date range (Month)") + ylab("Time deviation (minutes)")+
        expand_limits(x=c(first_date,last_month))+
        #scale_y_continuous(breaks = seq(0, 10000, 5)) +
        #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(0.5,"cm")))+
        geom_hline(yintercept=15, color = "Red", size = 1)
        #geom_text(aes(last(month),15,label = "15 Minutes", vjust = 1))
    }
    
    else if (days_count > 365) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate::years(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "year")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_year=last(as.Date(mydf$Date))%m+% lubridate::years(1)
      last_year=paste(last_year, "00:00:00", sep = " ")
      last_year=as.POSIXct(last_year)
      
      ggplot(mydf, aes(x=Date, y=Diff)) +
        #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a\n%d"),name="")+
        scale_x_datetime(breaks = "1 year",minor_breaks = "1 years",labels = date_format("%Y"),name="Date range (Year)")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
        geom_point() + xlab("Date range (Year)") + ylab("Time deviation (minutes)") +
        expand_limits(x=c(first_date,last_year))+
        #scale_y_continuous(breaks = seq(0, 10000, 5)) +
        #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(0.5,"cm")))+
        geom_hline(yintercept=15, color = "Red", size = 1)
        #geom_text(aes(last(year),15,label = "15 Minutes", vjust = 1))
    }
    }
    
  })
  
  output$plot10 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "med_adh.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(adh_directory, d, sep = "/")
    
    if (file.exists(a)) {
      adhFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    filtered <-
      adhFile %>%
      filter(intake_time >= input$daterange1[1],
             intake_time <= input$daterange1[2])
    filtered <- filtered[ ,c("diff_min", "intake_time")]
    filtered <- unique(filtered)
    
    a <- filtered[ ,c("intake_time")]
    b <- filtered[ ,c("diff_min")]
    mydf = data.frame(Date=a, Diff=b)
    
    date_error = as.numeric(input$daterange1[2]-as.Date(adhFile[c(1),"intake_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(adhFile))
    date_error = as.numeric(as.Date(adhFile[c(maximum),"intake_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    
    #mydf$day <- factor(strftime(mydf$Date,format="%a"),
     #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    
    #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    
    #mydf$week <- factor(strftime(mydf$Date,format="%V"))
    #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    
    #mydf$month <- strftime(mydf$Date,format="%b")
    #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    
    #mydf$year <- strftime(mydf$Date,format="%Y")
    #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    
    #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
    #days_count = count(mydf2)
    
    
    
    filtered2 <- adhFile
    filtered2 <- filtered2[ ,c("diff_min", "intake_time")]
    filtered2 <- unique(filtered2)
    #print(filtered2)
    
    #print(count(filtered2))
    first_point_intake = filtered2[c(1),2]
    
    count = count(filtered2)
    count = as.numeric(count)
    
    one_count_flag=0
    if (count<1) {
      stop(safeError("Empty data file for the patient."))
    }
    last_point_intake = filtered2[c(count),2]
    if (count==1) {
      count=2
      one_count_flag=1
    }
    
    half_point_intake = round(count/2)
    half_point_intake = as.numeric(half_point_intake)
    half_point_intake = filtered2[c(half_point_intake),2]
    #print(half_point_intake)
    
    if (one_count_flag==1) {
      half_point_intake_plus_one = round(count/2)
    } else {
      half_point_intake_plus_one = round(count/2) + 1
    }
    
    half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
    half_point_intake_plus_one = filtered2[c(half_point_intake_plus_one),2]
    #print(half_point_intake_plus_one)
    
    first_half <- 
      filtered2  %>%
      filter(intake_time >= first_point_intake,
             intake_time <= half_point_intake)
    
    #print(first_half)
    average1 = mean(first_half$diff_min)
    #print(average1)
    
    
    second_half <-
      filtered2  %>%
      filter(intake_time >= half_point_intake_plus_one,
             intake_time <= last_point_intake)
    
    #print(second_half)
    average2 = mean(second_half$diff_min)
    #print(average2)
    
    
    #if (input$input3Id == TRUE) {
    # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$Diff)])
    #}
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      #mydf$Date=as.Date(mydf$Date)
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
      mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      #day_and_date=paste(mydf$day, mydf$ddate, sep = " ")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = " ")
    }
    scaling=data.frame(asdf=unique(as.Date(mydf$Date)))
    scale_count=count(scaling)
    Switch2=F
    Switch=F
    if (scale_count>45) {
      Switch2=T
    }
    if (scale_count>7 & scale_count<46) {
      Switch=T
    }
    #print(scale_count)
    
    first_date=first(as.Date(mydf$Date))
    first_date=paste(first_date, "00:00:00", sep = " ")
    first_date=as.POSIXct(first_date)
    
    last_date=last(as.Date(mydf$Date))+ lubridate::days(1)
    last_date=paste(last_date, "00:00:00", sep = " ")
    last_date=as.POSIXct(last_date)
    #print(mydf)
    ggplot(mydf, aes(x=Date, y=Diff))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
      #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
      scale_x_datetime(breaks = "1 day", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")+
      #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
      theme_minimal() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"))+
      {if(Switch)theme(axis.text.x = element_text(angle = 45, hjust = 1))}+
      {if(Switch2)theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))}+
      #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
      geom_point() + xlab("Date range") + ylab("Time deviation (minutes)") +
      expand_limits(x=c(first_date,last_date))+
      #scale_y_continuous(breaks = seq(0, 10000, 5)) +
      #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm"))) +
      geom_hline(yintercept=15, color = "Red", size = 1)
      #geom_text(aes(last(day_and_date),15,label = "15 Minutes", vjust = 1))
    
    }
  })
  
  output$plot2 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "symptoms.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(pd_directory, d, sep = "/")
    
    if (file.exists(a)) {
      pdFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    
    filtered <-
      pdFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[ ,c("pd_value", "date_time")]
    filtered <- unique(filtered)
    
    a <- filtered[ ,c("date_time")]
    b <- filtered[ ,c("pd_value")]
    mydf = data.frame(Date=a, PD=b)
    
    date_error = as.numeric(input$daterange1[2]-as.Date(pdFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(pdFile))
    date_error = as.numeric(as.Date(pdFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    
    ###mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    
    ###mydf$day <- factor(strftime(mydf$Date,format="%a"),
    ###                levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    
    ###mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    
    ###mydf$week <- factor(strftime(mydf$Date,format="%V"))
    ###mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    
    ###mydf$month <- strftime(mydf$Date,format="%b")
    ###mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    
    ###mydf$year <- strftime(mydf$Date,format="%Y")
    ###mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    
    #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
    #days_count = count(mydf2)
    
    
    
    
    filtered2 <- pdFile
    filtered2 <- filtered2[ ,c("pd_value", "date_time")]
    filtered2 <- unique(filtered2)
    #print(filtered2)
    
    #print(count(filtered2))
    first_point_intake = filtered2[c(1),2]
    
    count = count(filtered2)
    count = as.numeric(count)
    one_count_flag=0
    if (count<1) {
      stop(safeError("Empty data file for the patient."))
    }
    last_point_intake = filtered2[c(count),2]
    if (count==1) {
      count=2
      one_count_flag=1
    }
    
    half_point_intake = round(count/2)
    half_point_intake = as.numeric(half_point_intake)
    half_point_intake = filtered2[c(half_point_intake),2]
    #print(half_point_intake)
    
    if (one_count_flag==1) {
      half_point_intake_plus_one = round(count/2)
    } else {
      half_point_intake_plus_one = round(count/2) + 1
    }
    
    half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
    half_point_intake_plus_one = filtered2[c(half_point_intake_plus_one),2]
    #print(half_point_intake_plus_one)
    
    first_half <- 
      filtered2  %>%
      filter(date_time >= first_point_intake,
             date_time <= half_point_intake)
    
    #print(first_half)
    average1 = mean(first_half$pd_value)
    #print(average1)
    
    
    second_half <-
      filtered2  %>%
      filter(date_time >= half_point_intake_plus_one,
             date_time <= last_point_intake)
    
    #print(second_half)
    average2 = mean(second_half$pd_value)
    #print(average2)
    #print(mydf)
    
    #if (input$input4Id == TRUE) {
    # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$PD)])
    #}
    #cc=as.numeric(count(mydf))
    #print(cc)
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    ###mydf$`Frequency` <- c(1)
    ###mydf$`Frequency ` <- c(1)
    ###mydf$`Frequency  ` <- c(1)
    ###mydf$`Frequency   ` <- c(1)
    
    #print(mydf)
    
    ###mydf_day_count = data.frame(PD = mydf$PD, day=mydf$day)
    ###mydf_day_count$count <- c(1)
    ###mydf_day_count$flag <- c(0)
    ###last=as.numeric(count(mydf_day_count))
    ###if (last>1) {
    ###last_minus_one=as.numeric(count(mydf_day_count))-1
    ###for (i in 1:last_minus_one) {
    ###for (j in (i+1):last) {
    ###   if (as.numeric(mydf_day_count$day[i])==as.numeric(mydf_day_count$day[j]) && as.numeric(mydf_day_count$PD[i])==as.numeric(mydf_day_count$PD[j]) && as.numeric((mydf_day_count$flag[i])==0) && as.numeric((mydf_day_count$flag[j])==0)) {
    ###     mydf$`Frequency`[i]=mydf$`Frequency`[i]+1
    ###     mydf$`Frequency`[j]=mydf$`Frequency`[j]-1
    ###     mydf_day_count$flag[j]=mydf_day_count$flag[j]+1
    ###   }
    ### }
    ###}
    ###}
    
    
    
    ###mydf_week_count = data.frame(PD = mydf$PD, week=mydf$week)
    ###mydf_week_count$count <- c(1)
    ###mydf_week_count$flag <- c(0)
    ###last=as.numeric(count(mydf_week_count))
    ###if (last>1) {
    ###last_minus_one=as.numeric(count(mydf_week_count))-1
    ###for (i in 1:last_minus_one) {
    ###  for (j in (i+1):last) {
    ###   if (as.numeric(mydf_week_count$week[i])==as.numeric(mydf_week_count$week[j]) && as.numeric(mydf_week_count$PD[i])==as.numeric(mydf_week_count$PD[j]) && as.numeric((mydf_week_count$flag[i])==0) && as.numeric((mydf_week_count$flag[j])==0)) {
    ###     mydf$`Frequency `[i]=mydf$`Frequency `[i]+1
    ###     mydf$`Frequency `[j]=mydf$`Frequency `[j]-1
    ###     mydf_week_count$flag[j]=mydf_week_count$flag[j]+1
    ###   }
    ### }
    ###}
    ###}
    
    
    
    
    ###mydf_month_count = data.frame(PD = mydf$PD, month=mydf$month)
    ###mydf_month_count$count <- c(1)
    ###mydf_month_count$flag <- c(0)
    ###last=as.numeric(count(mydf_month_count))
    ###if (last>1) {
    ###last_minus_one=as.numeric(count(mydf_month_count))-1
    ###for (i in 1:last_minus_one) {
    ### for (j in (i+1):last) {
    ###   if (as.numeric(mydf_month_count$month[i])==as.numeric(mydf_month_count$month[j]) && as.numeric(mydf_month_count$PD[i])==as.numeric(mydf_month_count$PD[j]) && as.numeric((mydf_month_count$flag[i])==0) && as.numeric((mydf_month_count$flag[j])==0)) {
    ###     mydf$`Frequency  `[i]=mydf$`Frequency  `[i]+1
    ###     mydf$`Frequency  `[j]=mydf$`Frequency  `[j]-1
    ###     mydf_month_count$flag[j]=mydf_month_count$flag[j]+1
    ###   }
    ### }
    ###}
    ###}
    
    ###mydf_year_count = data.frame(PD = mydf$PD, year=mydf$year)
    ###mydf_year_count$count <- c(1)
    ###mydf_year_count$flag <- c(0)
    ###last=as.numeric(count(mydf_year_count))
    ###if (last>1) {
    ###last_minus_one=as.numeric(count(mydf_year_count))-1
    ###for (i in 1:last_minus_one) {
    ### for (j in (i+1):last) {
    ###   if (as.numeric(mydf_year_count$year[i])==as.numeric(mydf_year_count$year[j]) && as.numeric(mydf_year_count$PD[i])==as.numeric(mydf_year_count$PD[j]) && as.numeric((mydf_year_count$flag[i])==0) && as.numeric((mydf_year_count$flag[j])==0)) {
    ###     mydf$`Frequency   `[i]=mydf$`Frequency   `[i]+1
    ###     mydf$`Frequency   `[j]=mydf$`Frequency   `[j]-1
    ###     mydf_year_count$flag[j]=mydf_year_count$flag[j]+1
    ###   }
    ### }
    ###}
    ###}
    
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      #mydf$Date=as.Date(mydf$Date)
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%d %H:%M:%S")
      #day_and_date=paste(mydf$day, mydf$ddate, sep = "\n")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = "\n")
    }
    
    #print(mydf)
    if (days_count < 8) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      }
      first_date=first(as.Date(mydf$Date))
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_day=last(as.Date(mydf$Date))+ lubridate::days(1)
      last_day=paste(last_day, "00:00:00", sep = " ")
      last_day=as.POSIXct(last_day)
      
      #mydf=mydf[mydf$`Frequency`!=0, ]
      #, color=`Frequency` in aes()
      ggplot(mydf, aes(x=Date, y=PD))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
        scale_x_datetime(breaks = "1 day", minor_breaks = "1 days", labels = date_format("%a\n%d\n%b"),name="Date range (Day)")+
        #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a\n%d\n%b"),name="Day")+
        #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Day)") + ylab("Score") +
        #scale_y_continuous(breaks = seq(0, 4, 1)) +
        scale_y_continuous(breaks = c(0,1,2,3,4), labels=c("0" = "No Symptoms", "1" = "Light Symptoms","2" = "Mild Symptoms","3" = "Strong Symptoms","4" = "Severe Symptoms") ) +
        expand_limits(y=c(0,4))+
        #annotate("text", x = last(mydf$day), y = (as.numeric(average2)), label = "Trend line of ratings")+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_day, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of ratings", vjust = "top", hjust="right"), size=3)
    }
    
    else if (days_count < 29) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(7)
      }
      first_date=cut(first(as.Date(mydf$Date)), "week")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_week=last(as.Date(mydf$Date))+ lubridate::days(7)
      last_week=paste(last_week, "00:00:00", sep = " ")
      last_week=as.POSIXct(last_week)
      
      #mydf=mydf[mydf$`Frequency `!=0, ]
      #, color=`Frequency ` in aes()
      ggplot(mydf, aes(x=Date, y=PD)) +
        scale_x_datetime(breaks = "1 week",minor_breaks = "1 weeks",labels = date_format("%W"),name="Date range (Week)")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Week)") + ylab("Score") +
        #scale_y_continuous(breaks = seq(0, 4, 1)) +
        scale_y_continuous(breaks = c(0,1,2,3,4),labels=c("0" = "No Symptoms", "1" = "Light Symptoms","2" = "Mild Symptoms","3" = "Strong Symptoms","4" = "Severe Symptoms") ) +
        expand_limits(y=c(0,4))+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_week, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(week),as.numeric(average2),label = "Trend line of ratings", vjust = "top", hjust="right"), size=3)
    }
    
    else if (days_count > 28 & days_count < 366 ) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate:::months.numeric(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "month")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_month=last(as.Date(mydf$Date))%m+% lubridate:::months.numeric(1)
      last_month=paste(last_month, "00:00:00", sep = " ")
      last_month=as.POSIXct(last_month)
      
      #mydf=mydf[mydf$`Frequency  `!=0, ]
      #, color=`Frequency  ` in aes()
      ggplot(mydf, aes(x=Date, y=PD)) +
        scale_x_datetime(breaks = "1 month",minor_breaks = "1 months",labels = date_format("%b"),name="Date range (Month)")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Month)") + ylab("Score")+
        #scale_y_continuous(breaks = seq(0, 4, 1)) +
        scale_y_continuous(breaks = c(0,1,2,3,4),labels=c("0" = "No Symptoms", "1" = "Light Symptoms","2" = "Mild Symptoms","3" = "Strong Symptoms","4" = "Severe Symptoms") ) +
        expand_limits(y=c(0,4))+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_month, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(month),as.numeric(average2),label = "Trend line of ratings", vjust = "top", hjust="right"), size=3)
    }
    
    else if (days_count > 365) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate::years(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "year")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_year=last(as.Date(mydf$Date))%m+% lubridate::years(1)
      last_year=paste(last_year, "00:00:00", sep = " ")
      last_year=as.POSIXct(last_year)
      
      #mydf=mydf[mydf$`Frequency   `!=0, ]
      #, color=`Frequency   ` in aes()
      ggplot(mydf, aes(x=Date, y=PD)) +
        scale_x_datetime(breaks = "1 year",minor_breaks = "1 years",labels = date_format("%Y"),name="Date range (Year)")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Year)") + ylab("Score") +
        #scale_y_continuous(breaks = seq(0, 4, 1)) +
        scale_y_continuous(breaks = c(0,1,2,3,4),labels=c("0" = "No Symptoms", "1" = "Light Symptoms","2" = "Mild Symptoms","3" = "Strong Symptoms","4" = "Severe Symptoms") ) +
        expand_limits(y=c(0,4))+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_year, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(year),as.numeric(average2),label = "Trend line of ratings", vjust = "top", hjust="right"), size=3)
    }
    }
  })
  
  output$plot20 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "symptoms.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(pd_directory, d, sep = "/")
    
    if (file.exists(a)) {
      pdFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    
    filtered <-
      pdFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[ ,c("pd_value", "date_time")]
    filtered <- unique(filtered)
    
    a <- filtered[ ,c("date_time")]
    b <- filtered[ ,c("pd_value")]
    mydf = data.frame(Date=a, PD=b)
    
    date_error = as.numeric(input$daterange1[2]-as.Date(pdFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(pdFile))
    date_error = as.numeric(as.Date(pdFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    
    #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    
    #mydf$day <- factor(strftime(mydf$Date,format="%a"),
     #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    
    #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    
    #mydf$week <- factor(strftime(mydf$Date,format="%V"))
    #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    
    #mydf$month <- strftime(mydf$Date,format="%b")
    #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    
    #mydf$year <- strftime(mydf$Date,format="%Y")
    #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
    #days_count = count(mydf2)
    
    
    
    
    filtered2 <- pdFile
    filtered2 <- filtered2[ ,c("pd_value", "date_time")]
    filtered2 <- unique(filtered2)
    #print(filtered2)
    
    #print(count(filtered2))
    first_point_intake = filtered2[c(1),2]
    
    count = count(filtered2)
    count = as.numeric(count)
    one_count_flag=0
    if (count<1) {
      stop(safeError("Empty data file for the patient."))
    }
    last_point_intake = filtered2[c(count),2]
    if (count==1) {
      count=2
      one_count_flag=1
    }
    
    half_point_intake = round(count/2)
    half_point_intake = as.numeric(half_point_intake)
    half_point_intake = filtered2[c(half_point_intake),2]
    #print(half_point_intake)
    
    if (one_count_flag==1) {
      half_point_intake_plus_one = round(count/2)
    } else {
      half_point_intake_plus_one = round(count/2) + 1
    }
    
    half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
    half_point_intake_plus_one = filtered2[c(half_point_intake_plus_one),2]
    #print(half_point_intake_plus_one)
    
    first_half <- 
      filtered2  %>%
      filter(date_time >= first_point_intake,
             date_time <= half_point_intake)
    
    #print(first_half)
    average1 = mean(first_half$pd_value)
    #print(average1)
    
    
    second_half <-
      filtered2  %>%
      filter(date_time >= half_point_intake_plus_one,
             date_time <= last_point_intake)
    
    #print(second_half)
    average2 = mean(second_half$pd_value)
    #print(average2)
    #print(mydf)
    
    #if (input$input4Id == TRUE) {
    # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$PD)])
    #}
    #cc=as.numeric(count(mydf))
    #print(cc)
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    ###mydf$`Frequency` <- c(1)
    #mydf$`Frequency ` <- c(1)
    #mydf$`Frequency  ` <- c(1)
    #mydf$`Frequency   ` <- c(1)
    
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      #mydf$Date=as.Date(mydf$Date)
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%d %H:%M:%S")
      mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      #day_and_date=paste(mydf$day, mydf$ddate, sep = " ")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = " ")
    }
    
    ###mydf_Date_count = data.frame(PD = mydf$PD, Date=mydf$Date)
    ###mydf_Date_count$count <- c(1)
    ###mydf_Date_count$flag <- c(0)
    ###last=as.numeric(count(mydf_Date_count))
    ###if (last>1) {
    ###last_minus_one=as.numeric(count(mydf_Date_count))-1
    ###for (i in 1:last_minus_one) {
    ### for (j in (i+1):last) {
    ###   if (mydf_Date_count$Date[i]==mydf_Date_count$Date[j] && mydf_Date_count$PD[i]==mydf_Date_count$PD[j] && (mydf_Date_count$flag[i])==0 && (mydf_Date_count$flag[j])==0) {
    ###     mydf$`Frequency`[i]=mydf$`Frequency`[i]+1
    ###     mydf$`Frequency`[j]=mydf$`Frequency`[j]-1
    ###     mydf_Date_count$flag[j]=mydf_Date_count$flag[j]+1
    ###   }
    ### }
    ###}
    ###}
    #print(mydf)
    scaling=data.frame(asdf=unique(as.Date(mydf$Date)))
    scale_count=count(scaling)
    Switch2=F
    Switch=F
    if (scale_count>45) {
      Switch2=T
    }
    if (scale_count>7 & scale_count<46) {
      Switch=T
    }
    
    
    first_date=first(as.Date(mydf$Date))
    first_date=paste(first_date, "00:00:00", sep = " ")
    first_date=as.POSIXct(first_date)
    
    last_date=last(as.Date(mydf$Date))+ lubridate::days(1)
    last_date=paste(last_date, "00:00:00", sep = " ")
    last_date=as.POSIXct(last_date)
    
    
    #mydf=mydf[mydf$`Frequency`!=0, ]
    #, color=`Frequency` in aes()
    ggplot(mydf, aes(x=Date, y=PD))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
      #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
      #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
      scale_x_datetime(breaks = "1 day", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")+
      theme_minimal() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"))+
      {if(Switch)theme(axis.text.x = element_text(angle = 45, hjust = 1))}+
      {if(Switch2)theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))}+
      geom_point() + xlab("Date range") + ylab("Score") +
      #scale_y_continuous(breaks = seq(0, 4, 1)) +
      scale_y_continuous(breaks = c(0,1,2,3,4), labels=c("0" = "No Symptoms", "1" = "Light Symptoms","2" = "Mild Symptoms","3" = "Strong Symptoms","4" = "Severe Symptoms") ) +
      expand_limits(y=c(0,4))+
      #annotate("text", x = last(mydf$day), y = (as.numeric(average2)), label = "Trend line of ratings")+
      geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_date, yend = as.numeric(average2)), size = 0.5, color="Blue")
      #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of ratings", vjust = "top", hjust="right"), size=3)
  
    }
  })
  
  
  
  
  output$plot3 <- renderPlot({
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "drawings_error.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(scores_directory, d, sep = "/")
    
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    a=drawings1_directory
    
    if (file.exists(a)) {
      errorFile <- read.csv(a, stringsAsFactors = FALSE, sep=";")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    errorFileDivided <-
      errorFile %>%
      filter(game_type == " spiral")
    
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    filtered <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[c("date_time")]
    filtered <- unique(filtered)
    
    a <- filtered[("date_time")]
    dfa=data.frame(aa=a)
    #print(count(dfa))
    
    b <- errorFileDivided[c("error")]
    dfb=data.frame(bb=b)
    #print(count(dfb))
    
    min_count=min(count(a), count(b))
    #print(min_count)
    
    #dfa = dfa[sample(nrow(dfa), min_count), ]
    #dfa = head(dfa, min_count)
    dfa=dfa[1:min_count,]
    #print(dfa)
    
    
    #dfb = dfb[sample(nrow(dfb), min_count), ]
    #dfb = head(dfb, min_count)
    dfb=dfb[1:min_count,]
    #print(dfb)
    
    mydf = data.frame(Date=dfa, error=dfb)
    #print(mydf)
    
    #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    #mydf$day <- factor(strftime(mydf$Date,format="%a"),
     #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    #mydf$week <- factor(strftime(mydf$Date,format="%V"))
    #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    #mydf$month <- strftime(mydf$Date,format="%b")
    #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    #mydf$year <- strftime(mydf$Date,format="%Y")
    #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    
    #print(mydf)
    filtered2 <- errorFileDivided
    filtered2 <- filtered2[ ,c("error", "time")]
    
    count = count(filtered2)
    count = as.numeric(count)
    
    one_count_flag=0
    if (count<1) {
      stop(safeError("Empty data file for the patient."))
    }
    if (count==1) {
      count=2
      one_count_flag=1
    }
    
    half_point_intake = round(count/2)
    half_point_intake = as.numeric(half_point_intake)
    #print(half_point_intake)
    
    if (one_count_flag==1) {
      half_point_intake_plus_one = round(count/2)
      count=1
    } else {
      half_point_intake_plus_one = round(count/2) + 1
    }
    
    
    half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
    
    first_half <- 
      filtered2  %>%
      slice(1:half_point_intake)
    
    #print(first_half)
    average1 = mean(first_half$error)
    #print(average1)
    
    
    second_half <-
      filtered2  %>%
      slice(half_point_intake_plus_one:count)
    
    #print(second_half)
    average2 = mean(second_half$error)
    #print(average2)
    
    
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      #mydf$Date=as.Date(mydf$Date)
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
      #day_and_date=paste(mydf$day, mydf$ddate, sep = "\n")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = "\n")
    }
    if (days_count < 8) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      }
      first_date=first(as.Date(mydf$Date))
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_day=last(as.Date(mydf$Date))+ lubridate::days(1)
      last_day=paste(last_day, "00:00:00", sep = " ")
      last_day=as.POSIXct(last_day)
      
      ggplot(mydf, aes(x=Date, y=error))+ #trendline(x1,y1,model="line2P",summary=TRUE)
        scale_x_datetime(breaks = "1 day", minor_breaks = "1 days", labels = date_format("%a\n%d\n%b"),name="Date range (Day)")+
        #geom_count()
        #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a\n%d\n%b"),name="Day")+
        #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Day)") + ylab("Error in pixels")+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_day, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of error", vjust = "top", hjust="right"), size=3)
      #scale_y_continuous(breaks = seq(0, 10000, 5)) +
      #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend =
      #as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm"))) +
      #geom_hline(yintercept=15, color = "Red", size = 1)
    }
    else if (days_count < 29) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(7)
      }
      first_date=cut(first(as.Date(mydf$Date)), "week")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_week=last(as.Date(mydf$Date))+ lubridate::days(7)
      last_week=paste(last_week, "00:00:00", sep = " ")
      last_week=as.POSIXct(last_week)
      
      ggplot(mydf, aes(x=Date, y=error)) +
        scale_x_datetime(breaks = "1 week",minor_breaks = "1 weeks",labels = date_format("%W"),name="Date range (Week)")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Week)") + ylab("Error in pixels")+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_week, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(week),as.numeric(average2),label = "Trend line of error", vjust = "top", hjust="right"), size=3)
      #scale_y_continuous(breaks = seq(0, 10000, 5)) +
      #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend =
      #                  as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm")))+
      #geom_hline(yintercept=15, color = "Red", size = 1)
    }
    else if (days_count > 28 & days_count < 366 ) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate:::months.numeric(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "month")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_month=last(as.Date(mydf$Date))%m+% lubridate:::months.numeric(1)
      last_month=paste(last_month, "00:00:00", sep = " ")
      last_month=as.POSIXct(last_month)
      
      ggplot(mydf, aes(x=Date, y=error)) +
        scale_x_datetime(breaks = "1 month",minor_breaks = "1 months",labels = date_format("%b"),name="Date range (Month)")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Month)") + ylab("Error in pixels")+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_month, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(month),as.numeric(average2),label = "Trend line of error", vjust = "top", hjust="right"), size=3)
      #scale_y_continuous(breaks = seq(0, 10000, 5)) +
      #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend =
      #                  as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm")))+
      #geom_hline(yintercept=15, color = "Red", size = 1)
    }
    else if (days_count > 365) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate::years(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "year")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_year=last(as.Date(mydf$Date))%m+% lubridate::years(1)
      last_year=paste(last_year, "00:00:00", sep = " ")
      last_year=as.POSIXct(last_year)
      
      ggplot(mydf, aes(x=Date, y=error)) +
        scale_x_datetime(breaks = "1 year",minor_breaks = "1 years",labels = date_format("%Y"),name="Date range (Year)")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Year)") + ylab("Error in pixels")+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_year, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(year),as.numeric(average2),label = "Trend line of error", vjust = "top", hjust="right"), size=3)
      #scale_y_continuous(breaks = seq(0, 10000, 5)) +
      #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend =
      #                  as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm")))+
      #geom_hline(yintercept=15, color = "Red", size = 1)
    }
    }
  })
  
  output$plot33 <- renderPlot({
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "drawings_error.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(scores_directory, d, sep = "/")
    
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    a=drawings1_directory
    
    if (file.exists(a)) {
      errorFile <- read.csv(a, stringsAsFactors = FALSE, sep=";")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    errorFileDivided <-
      errorFile %>%
      filter(game_type == " square")
    
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    filtered <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[c("date_time")]
    filtered <- unique(filtered)
    
    a <- filtered[("date_time")]
    dfa=data.frame(aa=a)
    #print(count(dfa))
    
    b <- errorFileDivided[c("error")]
    dfb=data.frame(bb=b)
    #print(count(dfb))
    
    min_count=min(count(a), count(b))
    #print(min_count)
    
    #dfa = dfa[sample(nrow(dfa), min_count), ]
    #dfa = head(dfa, min_count)
    dfa=dfa[1:min_count,]
    #print(dfa)
    
    
    #dfb = dfb[sample(nrow(dfb), min_count), ]
    #dfb = head(dfb, min_count)
    dfb=dfb[1:min_count,]
    #print(dfb)
    
    mydf = data.frame(Date=dfa, error=dfb)
    #print(mydf)
    
    #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    #mydf$day <- factor(strftime(mydf$Date,format="%a"),
     #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    #mydf$week <- factor(strftime(mydf$Date,format="%V"))
    #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    #mydf$month <- strftime(mydf$Date,format="%b")
    #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    #mydf$year <- strftime(mydf$Date,format="%Y")
    #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    
    #print(mydf)
    filtered2 <- errorFileDivided
    filtered2 <- filtered2[ ,c("error", "time")]
    
    count = count(filtered2)
    count = as.numeric(count)
    
    
    one_count_flag=0
    if (count<1) {
      stop(safeError("Empty data file for the patient."))
    }
    if (count==1) {
      count=2
      one_count_flag=1
    }
    
    half_point_intake = round(count/2)
    half_point_intake = as.numeric(half_point_intake)
    #print(half_point_intake)
    
    if (one_count_flag==1) {
      half_point_intake_plus_one = round(count/2)
      count=1
    } else {
      half_point_intake_plus_one = round(count/2) + 1
    }
    
    half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
    
    first_half <- 
      filtered2  %>%
      slice(1:half_point_intake)
    
    #print(first_half)
    average1 = mean(first_half$error)
    #print(average1)
    
    
    second_half <-
      filtered2  %>%
      slice(half_point_intake_plus_one:count)
    
    #print(second_half)
    average2 = mean(second_half$error)
    #print(average2)
    
    
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      #mydf$Date=as.Date(mydf$Date)
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
      #day_and_date=paste(mydf$day, mydf$ddate, sep = "\n")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = "\n")
    }
    if (days_count < 8) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      }
      first_date=first(as.Date(mydf$Date))
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_day=last(as.Date(mydf$Date))+ lubridate::days(1)
      last_day=paste(last_day, "00:00:00", sep = " ")
      last_day=as.POSIXct(last_day)
      
      ggplot(mydf, aes(x=Date, y=error))+ #trendline(x1,y1,model="line2P",summary=TRUE)
        scale_x_datetime(breaks = "1 day", minor_breaks = "1 days", labels = date_format("%a\n%d\n%b"),name="Date range (Day)")+
        #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a\n%d\n%b"),name="Day")+
        #geom_count()
        #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Day)") + ylab("Error in pixels")+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_day, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of error", vjust = "top", hjust="right"), size=3)
      #scale_y_continuous(breaks = seq(0, 10000, 5)) +
      #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend =
      #as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm"))) +
      #geom_hline(yintercept=15, color = "Red", size = 1)
    }
    else if (days_count < 29) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(7)
      }
      first_date=cut(first(as.Date(mydf$Date)), "week")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_week=last(as.Date(mydf$Date))+ lubridate::days(7)
      last_week=paste(last_week, "00:00:00", sep = " ")
      last_week=as.POSIXct(last_week)
      
      ggplot(mydf, aes(x=Date, y=error)) +
        scale_x_datetime(breaks = "1 week",minor_breaks = "1 weeks",labels = date_format("%W"),name="Date range (Week)")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Week)") + ylab("Error in pixels")+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_week, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(week),as.numeric(average2),label = "Trend line of error", vjust = "top", hjust="right"), size=3)
      #scale_y_continuous(breaks = seq(0, 10000, 5)) +
      #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend =
      #                  as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm")))+
      #geom_hline(yintercept=15, color = "Red", size = 1)
    }
    else if (days_count > 28 & days_count < 366 ) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate:::months.numeric(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "month")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_month=last(as.Date(mydf$Date))%m+% lubridate:::months.numeric(1)
      last_month=paste(last_month, "00:00:00", sep = " ")
      last_month=as.POSIXct(last_month)
      
      ggplot(mydf, aes(x=Date, y=error)) +
        scale_x_datetime(breaks = "1 month",minor_breaks = "1 months",labels = date_format("%b"),name="Date range (Month)")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Month)") + ylab("Error in pixels")+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_month, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(month),as.numeric(average2),label = "Trend line of error", vjust = "top", hjust="right"), size=3)
      #scale_y_continuous(breaks = seq(0, 10000, 5)) +
      #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend =
      #                  as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm")))+
      #geom_hline(yintercept=15, color = "Red", size = 1)
    }
    else if (days_count > 365) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate::years(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "year")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_year=last(as.Date(mydf$Date))%m+% lubridate::years(1)
      last_year=paste(last_year, "00:00:00", sep = " ")
      last_year=as.POSIXct(last_year)
      
      ggplot(mydf, aes(x=Date, y=error)) +
        scale_x_datetime(breaks = "1 year",minor_breaks = "1 years",labels = date_format("%Y"),name="Date range (Year)")+
        theme_minimal() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Year)") + ylab("Error in pixels")+
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_year, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(year),as.numeric(average2),label = "Trend line of error", vjust = "top", hjust="right"), size=3)
      #scale_y_continuous(breaks = seq(0, 10000, 5)) +
      #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend =
      #                  as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm")))+
      #geom_hline(yintercept=15, color = "Red", size = 1)
    }
    }
  })
  
  output$plot30 <- renderPlot({
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "drawings_error.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(scores_directory, d, sep = "/")
    
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    a=drawings1_directory
    
    if (file.exists(a)) {
      errorFile <- read.csv(a, stringsAsFactors = FALSE, sep=";")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    errorFileDivided <-
      errorFile %>%
      filter(game_type == " spiral")
    
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    filtered <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[c("date_time")]
    filtered <- unique(filtered)
    
    a <- filtered[("date_time")]
    dfa=data.frame(aa=a)
    #print(count(dfa))
    
    b <- errorFileDivided[c("error")]
    dfb=data.frame(bb=b)
    #print(count(dfb))
    
    min_count=min(count(a), count(b))
    #print(min_count)
    
    #dfa = dfa[sample(nrow(dfa), min_count), ]
    #dfa = head(dfa, min_count)
    dfa=dfa[1:min_count,]
    #print(dfa)
    
    
    #dfb = dfb[sample(nrow(dfb), min_count), ]
    #dfb = head(dfb, min_count)
    dfb=dfb[1:min_count,]
    #print(dfb)
    
    mydf = data.frame(Date=dfa, error=dfb)
    #print(mydf)
    
    #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    #mydf$day <- factor(strftime(mydf$Date,format="%a"),
     #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    #mydf$week <- factor(strftime(mydf$Date,format="%V"))
    #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    #mydf$month <- strftime(mydf$Date,format="%b")
    #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    #mydf$year <- strftime(mydf$Date,format="%Y")
    #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    
    #print(mydf)
    filtered2 <- errorFileDivided
    filtered2 <- filtered2[ ,c("error", "time")]
    
    count = count(filtered2)
    count = as.numeric(count)
    
    one_count_flag=0
    if (count<1) {
      stop(safeError("Empty data file for the patient."))
    }
    if (count==1) {
      count=2
      one_count_flag=1
    }
    
    half_point_intake = round(count/2)
    half_point_intake = as.numeric(half_point_intake)
    #print(half_point_intake)
    
    if (one_count_flag==1) {
      half_point_intake_plus_one = round(count/2)
      count=1
    } else {
      half_point_intake_plus_one = round(count/2) + 1
    }
    
    half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
    
    first_half <- 
      filtered2  %>%
      slice(1:half_point_intake)
    
    #print(first_half)
    average1 = mean(first_half$error)
    #print(average1)
    
    
    second_half <-
      filtered2  %>%
      slice(half_point_intake_plus_one:count)
    
    #print(second_half)
    average2 = mean(second_half$error)
    #print(average2)
    
    
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      #mydf$Date=as.Date(mydf$Date)
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
      mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      #day_and_date=paste(mydf$day, mydf$ddate, sep = " ")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = " ")
    }
    scaling=data.frame(asdf=unique(as.Date(mydf$Date)))
    scale_count=count(scaling)
    Switch2=F
    Switch=F
    if (scale_count>45) {
      Switch2=T
    }
    if (scale_count>7 & scale_count<46) {
      Switch=T
    }
    
    
    first_date=first(as.Date(mydf$Date))
    first_date=paste(first_date, "00:00:00", sep = " ")
    first_date=as.POSIXct(first_date)
    
    last_date=last(as.Date(mydf$Date))+ lubridate::days(1)
    last_date=paste(last_date, "00:00:00", sep = " ")
    last_date=as.POSIXct(last_date)
    
    ggplot(mydf, aes(x=Date, y=error))+ #trendline(x1,y1,model="line2P",summary=TRUE)
      #geom_count()
      #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
      scale_x_datetime(breaks = "1 day", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")+
      theme_minimal() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"))+
      {if(Switch)theme(axis.text.x = element_text(angle = 45, hjust = 1))}+
      {if(Switch2)theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))}+
      scale_x_datetime(breaks = "1 day", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")+
      #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
      geom_point() + xlab("Date range") + ylab("Error in pixels")+
      geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_date, yend = as.numeric(average2)), size = 0.5, color="Blue")
      #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of error", vjust = "top", hjust="right"), size=3)
    #scale_y_continuous(breaks = seq(0, 10000, 5)) +
    #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend =
    #as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm"))) +
    #geom_hline(yintercept=15, color = "Red", size = 1)
    
    }
  })
  
  output$plot330 <- renderPlot({
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "drawings_error.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(scores_directory, d, sep = "/")
    
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    a=drawings1_directory
    
    if (file.exists(a)) {
      errorFile <- read.csv(a, stringsAsFactors = FALSE, sep=";")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    errorFileDivided <-
      errorFile %>%
      filter(game_type == " square")
    
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    filtered <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[c("date_time")]
    filtered <- unique(filtered)
    
    a <- filtered[("date_time")]
    dfa=data.frame(aa=a)
    #print(count(dfa))
    
    b <- errorFileDivided[c("error")]
    dfb=data.frame(bb=b)
    #print(count(dfb))
    
    min_count=min(count(a), count(b))
    #print(min_count)
    
    #dfa = dfa[sample(nrow(dfa), min_count), ]
    #dfa = head(dfa, min_count)
    dfa=dfa[1:min_count,]
    #print(dfa)
    
    
    #dfb = dfb[sample(nrow(dfb), min_count), ]
    #dfb = head(dfb, min_count)
    dfb=dfb[1:min_count,]
    #print(dfb)
    
    mydf = data.frame(Date=dfa, error=dfb)
    #print(mydf)
    
    #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    #mydf$day <- factor(strftime(mydf$Date,format="%a"),
     #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    #mydf$week <- factor(strftime(mydf$Date,format="%V"))
    #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    #mydf$month <- strftime(mydf$Date,format="%b")
    #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    #mydf$year <- strftime(mydf$Date,format="%Y")
    #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    
    #print(mydf)
    filtered2 <- errorFileDivided
    filtered2 <- filtered2[ ,c("error", "time")]
    
    count = count(filtered2)
    count = as.numeric(count)
    
    one_count_flag=0
    if (count<1) {
      stop(safeError("Empty data file for the patient."))
    }
    if (count==1) {
      count=2
      one_count_flag=1
    }
    
    half_point_intake = round(count/2)
    half_point_intake = as.numeric(half_point_intake)
    #print(half_point_intake)
    
    if (one_count_flag==1) {
      half_point_intake_plus_one = round(count/2)
      count=1
    } else {
      half_point_intake_plus_one = round(count/2) + 1
    }
    
    half_point_intake_plus_one = as.numeric(half_point_intake_plus_one)
    
    first_half <- 
      filtered2  %>%
      slice(1:half_point_intake)
    
    #print(first_half)
    average1 = mean(first_half$error)
    #print(average1)
    
    
    second_half <-
      filtered2  %>%
      slice(half_point_intake_plus_one:count)
    
    #print(second_half)
    average2 = mean(second_half$error)
    #print(average2)
    
    
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
      mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      #mydf$Date=as.Date(mydf$Date)
      #day_and_date=paste(mydf$day, mydf$ddate, sep = " ")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = " ")
    }
    scaling=data.frame(asdf=unique(as.Date(mydf$Date)))
    scale_count=count(scaling)
    Switch2=F
    Switch=F
    if (scale_count>45) {
      Switch2=T
    }
    if (scale_count>7 & scale_count<46) {
      Switch=T
    }
    
    
    first_date=first(as.Date(mydf$Date))
    first_date=paste(first_date, "00:00:00", sep = " ")
    first_date=as.POSIXct(first_date)
    
    last_date=last(as.Date(mydf$Date))+ lubridate::days(1)
    last_date=paste(last_date, "00:00:00", sep = " ")
    last_date=as.POSIXct(last_date)
    
    ggplot(mydf, aes(x=Date, y=error))+ #trendline(x1,y1,model="line2P",summary=TRUE)
      #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
      scale_x_datetime(breaks = "1 day", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")+
      theme_minimal() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"))+
      {if(Switch)theme(axis.text.x = element_text(angle = 45, hjust = 1))}+
      {if(Switch2)theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))}+
      #geom_count()
      #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
      geom_point() + xlab("Date range") + ylab("Error in pixels")+
      geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_date, yend = as.numeric(average2)), size = 0.5, color="Blue")
      #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of error", vjust = "top", hjust="right"), size=3)
    #scale_y_continuous(breaks = seq(0, 10000, 5)) +
    #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend =
    #as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm"))) +
    #geom_hline(yintercept=15, color = "Red", size = 1)
    
    }
  })
  
  output$plot4 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "drawings.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(scores_directory, d, sep = "/")
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    filtered <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[c("date_time")]
    filtered <- unique(filtered)
    
    if (as.numeric(count(filtered))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    #filename = list()
    #for (i in 1:6) {
     # filename[i] <- normalizePath(file.path(spiral_pd_directoy, 
      #  paste0("pd",i,"_spiral_1", '.jpg')))
    #}
    
    #filename <- normalizePath(file.path(merged_directoy, paste0("spiral-pd", ".jpg", sep = "")))
    #filename <- filename[file.exists(filename)]
    #jpgs = lapply(filename, readJPEG)
    #asGrobs = lapply(jpgs, rasterGrob)
    #p <- grid.arrange(grobs=asGrobs, nrow = 2, ncol = 3)
    
    
    files <- list.files(path = spiral_pd_directoy, pattern = "*.jpg", full.names = TRUE)
    part_files = files[1:4]
    jpgs = lapply(part_files, readJPEG)
    asGrobs = lapply(jpgs, grid::rasterGrob)
    p <- grid.arrange(grobs=asGrobs, ncol = 4)
    
    #photo=paste(merged_directoy, "spiral-pd.jpg", sep="/")
    #pic = readJPEG(photo)
    #plot.new()
    #return(grid::grid.raster(pic))
    }
  })
  
  
  output$plot44 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "drawings.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(scores_directory, d, sep = "/")
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    filtered <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[c("date_time")]
    filtered <- unique(filtered)
    
    if (as.numeric(count(filtered))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    files <- list.files(path = square_pd_directoy, pattern = "*.jpg", full.names = TRUE)
    part_files = files[1:4]
    jpgs = lapply(part_files, readJPEG)
    asGrobs = lapply(jpgs, grid::rasterGrob)
    p <- grid.arrange(grobs=asGrobs, ncol = 4)
    #photo=paste(merged_directoy, "square-pd.jpg", sep="/")
    #pic = readJPEG(photo)
    #plot.new()
    #return(grid::grid.raster(pic))
    }
  })
  
  
  output$plot40 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "drawings.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(scores_directory, d, sep = "/")
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    filtered <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[c("date_time")]
    filtered <- unique(filtered)
    
    if (as.numeric(count(filtered))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    files <- list.files(path = spiral_pd_directoy, pattern = "*.jpg", full.names = TRUE)
    jpgs = lapply(files, readJPEG)
    asGrobs = lapply(jpgs, grid::rasterGrob)
    p <- grid.arrange(grobs=asGrobs, nrow = 4)
    
    #photo=paste(merged_directoy, "spiral-pd.jpg", sep="/")
    #pic = readJPEG(photo)
    #plot.new()
    #return(grid::grid.raster(pic))
    }
  })
  
  output$plot440 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "drawings.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(scores_directory, d, sep = "/")
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    filtered <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[c("date_time")]
    filtered <- unique(filtered)
    
    if (as.numeric(count(filtered))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    files <- list.files(path = square_pd_directoy, pattern = "*.jpg", full.names = TRUE)
    jpgs = lapply(files, readJPEG)
    asGrobs = lapply(jpgs, grid::rasterGrob)
    p <- grid.arrange(grobs=asGrobs, nrow = 4)
    #photo=paste(merged_directoy, "square-pd.jpg", sep="/")
    #pic = readJPEG(photo)
    #plot.new()
    #return(grid::grid.raster(pic))
    }
  })
  
  
  
  output$plot5 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "games_tip.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(ti_directory, d, sep = "/")
    
    if (file.exists(a)) {
      tiFile <- read.csv(a, stringsAsFactors = FALSE)
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    a=paste(scores_directory, d, sep = "/")
    
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    
    filtered1 <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered1 <- filtered1[ ,c("game_id", "date_time")]
    filtered1 <- unique(filtered1)
    
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    #print(date_error)
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    filtered <-
      tiFile %>%
      filter(game_id >= min (filtered1[1][1]),
             game_id <= max (filtered1[1][1]))
    filtered <- filtered[ ,c("game_id", "TI")]
    filtered <- unique(filtered)
    
    
    a <- filtered1[ ,c("date_time")]
    b <- filtered[ ,c("TI")]
    c <- filtered1[ ,c("game_id")]
    mydf = data.frame(Date=a, TI=b, game_id=c)
    
    aa <- unique(scoresFile[ ,c("date_time")])
    bb <- unique(tiFile[ ,c("TI")])
    cc <- unique(scoresFile[ ,c("game_id")])
    mydf_original = data.frame(Date=aa, TI=bb, game_id=cc)
    
    
    
    
    #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    
    #mydf$day <- factor(strftime(mydf$Date,format="%a"),
     #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    
    #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    
    #mydf$week <- factor(strftime(mydf$Date,format="%V"))
    #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    
    #mydf$month <- strftime(mydf$Date,format="%b")
    #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    
    #mydf$year <- strftime(mydf$Date,format="%Y")
    #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    
    #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
    #days_count = count(mydf2)
    
    
    
    filtered2 <- mydf_original
    #filtered2 <- filtered2[ ,c("score", "game_id")]
    filtered2 <- unique(filtered2)
    #print(filtered2)
    
    #print(count(filtered2))
    half_point = round((min(filtered2$game_id)+max(filtered2$game_id))/2)
    #print(half_point)
    
    
    first_half <-
      filtered2 %>%
      filter(game_id >= 1,
             game_id <= as.numeric(half_point))
    
    #print(first_half)
    average1 = mean(first_half$TI)
    #print(average1)
    
    
    second_half <-
      filtered2 %>%
      filter(game_id >= as.numeric(half_point+1),
             game_id <= count(filtered2))
    
    #print(second_half)
    average2 = mean(second_half$TI)
    #print(average2)
    
    
    #if (input$input1Id == TRUE) {
    # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$TI)])
    #}
    
    
    #TIstep = max(filtered[2])/5
    #print(TIstep)
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      #mydf$Date=as.Date(mydf$Date)
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
      #day_and_date=paste(mydf$day, mydf$ddate, sep = "\n")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = "\n")
    }
    if (days_count < 8) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      }
      first_date=first(as.Date(mydf$Date))
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_day=last(as.Date(mydf$Date))+ lubridate::days(1)
      last_day=paste(last_day, "00:00:00", sep = " ")
      last_day=as.POSIXct(last_day)
      
      ggplot(mydf, aes(x=Date, y=TI))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
        #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
        #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a\n%d\n%b"),name="Day")+
        scale_x_datetime(breaks = "1 day", minor_breaks = "1 days", labels = date_format("%a\n%d\n%b"),name="Date range (Day)")+
        theme_bw() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Day)") + ylab("Tremor Intensity Parameter") +
        #scale_y_continuous(breaks = seq(0, 100, 1)) +
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_day, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of TIP", vjust = "top", hjust="right"), size=3)
      #geom_point(col = "red", size=3)
    }
    
    else if (days_count < 29) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(7)
      }
      first_date=cut(first(as.Date(mydf$Date)), "week")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_week=last(as.Date(mydf$Date))+ lubridate::days(7)
      last_week=paste(last_week, "00:00:00", sep = " ")
      last_week=as.POSIXct(last_week)
      
      ggplot(mydf, aes(x=Date, y=TI)) +
        scale_x_datetime(breaks = "1 week",minor_breaks = "1 weeks",labels = date_format("%W"),name="Date range (Week)")+
        theme_bw() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Week)") + ylab("Tremor Intensity Parameter") +
        #scale_y_continuous(breaks = seq(0, 10000, 1)) +
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_week, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(week),as.numeric(average2),label = "Trend line of TIP", vjust = "top", hjust="right"), size=3)
    }
    
    else if (days_count > 28 & days_count < 366 ) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate:::months.numeric(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "month")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_month=last(as.Date(mydf$Date))%m+% lubridate:::months.numeric(1)
      last_month=paste(last_month, "00:00:00", sep = " ")
      last_month=as.POSIXct(last_month)
      
      ggplot(mydf, aes(x=Date, y=TI)) +
        scale_x_datetime(breaks = "1 month",minor_breaks = "1 months",labels = date_format("%b"),name="Date range (Month)")+
        theme_bw() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Month)") + ylab("Tremor Intensity Parameter")+
        #scale_y_continuous(breaks = seq(0, 10000, 1)) +
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_month, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(month),as.numeric(average2),label = "Trend line of TIP", vjust = "top", hjust="right"), size=3)
    }
    
    else if (days_count > 365) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate::years(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "year")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_year=last(as.Date(mydf$Date))%m+% lubridate::years(1)
      last_year=paste(last_year, "00:00:00", sep = " ")
      last_year=as.POSIXct(last_year)
      
      ggplot(mydf, aes(x=Date, y=TI)) +
        scale_x_datetime(breaks = "1 year",minor_breaks = "1 years",labels = date_format("%Y"),name="Date range (Year)")+
        theme_bw() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Year)") + ylab("Tremor Intensity Parameter") +
        #scale_y_continuous(breaks = seq(0, 10000, 1)) +
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_year, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(year),as.numeric(average2),label = "Trend line of TIP", vjust = "top", hjust="right"), size=3)
    }
    
    #}
    }
  })
  
  
  output$plot50 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "games_tip.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(ti_directory, d, sep = "/")
    
    if (file.exists(a)) {
      tiFile <- read.csv(a, stringsAsFactors = FALSE)
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    a=paste(scores_directory, d, sep = "/")
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    
    filtered1 <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered1 <- filtered1[ ,c("game_id", "date_time")]
    filtered1 <- unique(filtered1)
    
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    filtered <-
      tiFile %>%
      filter(game_id >= min (filtered1[1][1]),
             game_id <= max (filtered1[1][1]))
    filtered <- filtered[ ,c("game_id", "TI")]
    filtered <- unique(filtered)
    
    
    a <- filtered1[ ,c("date_time")]
    b <- filtered[ ,c("TI")]
    c <- filtered1[ ,c("game_id")]
    mydf = data.frame(Date=a, TI=b, game_id=c)
    
    aa <- unique(scoresFile[ ,c("date_time")])
    bb <- unique(tiFile[ ,c("TI")])
    cc <- unique(scoresFile[ ,c("game_id")])
    mydf_original = data.frame(Date=aa, TI=bb, game_id=cc)
    
    #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    
    #mydf$day <- factor(strftime(mydf$Date,format="%a"),
     #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    
    #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    
    #mydf$week <- factor(strftime(mydf$Date,format="%V"))
    #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    
    #mydf$month <- strftime(mydf$Date,format="%b")
    #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    
    #mydf$year <- strftime(mydf$Date,format="%Y")
    #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    
    #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
    #days_count = count(mydf2)
    
    
    
    filtered2 <- mydf_original
    #filtered2 <- filtered2[ ,c("score", "game_id")]
    filtered2 <- unique(filtered2)
    #print(filtered2)
    
    #print(count(filtered2))
    half_point = round((min(filtered2$game_id)+max(filtered2$game_id))/2)
    #print(half_point)
    
    
    first_half <-
      filtered2 %>%
      filter(game_id >= 1,
             game_id <= as.numeric(half_point))
    
    #print(first_half)
    average1 = mean(first_half$TI)
    #print(average1)
    
    
    second_half <-
      filtered2 %>%
      filter(game_id >= as.numeric(half_point+1),
             game_id <= count(filtered2))
    
    #print(second_half)
    average2 = mean(second_half$TI)
    #print(average2)
    
    
    #if (input$input1Id == TRUE) {
    # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$TI)])
    #}
    
    
    #TIstep = max(filtered[2])/5
    #print(TIstep)
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      #mydf$Date=as.Date(mydf$Date)
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
      mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      #day_and_date=paste(mydf$day, mydf$ddate, sep = " ")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = " ")
    }
    scaling=data.frame(asdf=unique(as.Date(mydf$Date)))
    scale_count=count(scaling)
    Switch2=F
    Switch=F
    if (scale_count>45) {
      Switch2=T
    }
    if (scale_count>7 & scale_count<46) {
      Switch=T
    }
    
    first_date=first(as.Date(mydf$Date))
    first_date=paste(first_date, "00:00:00", sep = " ")
    first_date=as.POSIXct(first_date)
    
    last_date=last(as.Date(mydf$Date))+ lubridate::days(1)
    last_date=paste(last_date, "00:00:00", sep = " ")
    last_date=as.POSIXct(last_date)
    
    ggplot(mydf, aes(x=Date, y=TI))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
      #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
      scale_x_datetime(breaks = "1 day", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")+
      theme_bw() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"))+
      {if(Switch)theme(axis.text.x = element_text(angle = 45, hjust = 1))}+
      {if(Switch2)theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))}+
      geom_point() + xlab("Date range") + ylab("Tremor Intensity Parameter") +
      #scale_y_continuous(breaks = seq(0, 100, 1)) +
      geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_date, yend = as.numeric(average2)), size = 0.5, color="Blue")
      #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of TIP", vjust = "top", hjust="right"), size=3)
    #geom_point(col = "red", size=3)
    
    
    
    #}
    }
  })
  
  
  
  output$plot6 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "game_scores.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(scores_directory, d, sep = "/")
    
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    
    filtered <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[ ,c("score", "date_time")]
    filtered <- unique(filtered)
    
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    a <- filtered[ ,c("date_time")]
    b <- filtered[ ,c("score")]
    mydf = data.frame(Date=a, Scores=b)
    
    #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    
    #mydf$day <- factor(strftime(mydf$Date,format="%a"),
     #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    
    #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    
    #mydf$week <- factor(strftime(mydf$Date,format="%V"))
    #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    
    #mydf$month <- strftime(mydf$Date,format="%b")
    #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    
    #mydf$year <- strftime(mydf$Date,format="%Y")
    #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    
    #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
    #days_count = count(mydf2)
    
    
    
    filtered2 <- scoresFile
    filtered2 <- filtered2[ ,c("score", "game_id")]
    filtered2 <- unique(filtered2)
    
    #print(count(filtered2))
    half_point = round((min(filtered2$game_id)+max(filtered2$game_id))/2)
    #print(half_point)
    
    
    first_half <-
      filtered2 %>%
      filter(game_id >= 1,
             game_id <= as.numeric(half_point))
    
    #print(first_half)
    average1 = mean(first_half$score)
    #print(average1)
    
    
    second_half <-
      filtered2 %>%
      filter(game_id >= as.numeric(half_point+1),
             game_id <= count(filtered2))
    
    #print(second_half)
    average2 = mean(second_half$score)
    #print(average2)
    
    
    #if (input$input2Id == TRUE) {
    # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$Scores)])
    #}
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      #mydf$Date=as.Date(mydf$Date)
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
      #mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      #mydf$Date[i]=mydf$Date[i]%m+% lubridate:::months.numeric(1)
      #day_and_date=paste(mydf$day, mydf$ddate, sep = "\n")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = "\n")
    }
    
    
    if (days_count < 8) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      }
      first_date=first(as.Date(mydf$Date))
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_day=last(as.Date(mydf$Date))+ lubridate::days(1)
      last_day=paste(last_day, "00:00:00", sep = " ")
      last_day=as.POSIXct(last_day)
      #print(mydf)
      ggplot(mydf, aes(x=Date, y=Scores))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
        scale_x_datetime(breaks = "1 day", minor_breaks = "1 days", labels = date_format("%a\n%d\n%b"),name="Date range (Day)")+
        #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
        theme_light() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
        geom_point() + xlab("Date range (Day)") + ylab("Game Score (0-100)") +
        #scale_y_continuous(breaks = seq(0, 100, 2)) +
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_day, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of scores", vjust = "top", hjust="right"), size=3)
    }
    
    else if (days_count < 29) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]+ lubridate::days(7)
      }
      first_date=cut(first(as.Date(mydf$Date)), "week")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_week=last(as.Date(mydf$Date))+ lubridate::days(7)
      last_week=paste(last_week, "00:00:00", sep = " ")
      last_week=as.POSIXct(last_week)
      ggplot(mydf, aes(x=Date, y=Scores)) +
        scale_x_datetime(breaks = "1 week",minor_breaks = "1 weeks",labels = date_format("%W"),name="Date range (Week)")+
        theme_light() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Week)") + ylab("Game Score (0-100)") +
        #scale_y_continuous(breaks = seq(0, 100, 2)) +
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_week, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(week),as.numeric(average2),label = "Trend line of scores", vjust = "top", hjust="right"), size=3)
    }
    #expand = c(0, 0), put in scalexdatetime
    else if (days_count > 28 & days_count < 366 ) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate:::months.numeric(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "month")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_month=last(as.Date(mydf$Date))%m+% lubridate:::months.numeric(1)
      last_month=paste(last_month, "00:00:00", sep = " ")
      last_month=as.POSIXct(last_month)
      #print(mydf)
      ggplot(mydf, aes(x=Date, y=Scores)) +
        scale_x_datetime(breaks = "1 month",minor_breaks = "1 months",labels = date_format("%b"),name="Date range (Month)")+
        theme_light() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
        geom_point() + xlab("Date range (Month)") + ylab("Game Score (0-100)")+
        #scale_y_continuous(breaks = seq(0, 100, 2)) +
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_month, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(month),as.numeric(average2),label = "Trend line of scores", vjust = "top", hjust="right"), size=3)
    }
    
    else if (days_count > 365) {
      for (i in 1:last) {
        mydf$Date[i]=mydf$Date[i]%m+% lubridate::years(1)
      }
      first_date=cut(first(as.Date(mydf$Date)), "year")
      first_date=paste(first_date, "00:00:00", sep = " ")
      first_date=as.POSIXct(first_date)
      
      last_year=last(as.Date(mydf$Date))%m+% lubridate::years(1)
      last_year=paste(last_year, "00:00:00", sep = " ")
      last_year=as.POSIXct(last_year)
      ggplot(mydf, aes(x=Date, y=Scores)) +
        scale_x_datetime(breaks = "1 year",minor_breaks = "1 years",labels = date_format("%Y"),name="Date range (Year)")+
        theme_light() +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=12,face="bold"))+
        geom_point() + xlab("Date range (Year)") + ylab("Game Score (0-100)") +
        #scale_y_continuous(breaks = seq(0, 100, 2)) +
        geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_year, yend = as.numeric(average2)), size = 0.5, color="Blue")
        #geom_text(aes(last(year),as.numeric(average2),label = "Trend line of scores", vjust = "top", hjust="right"), size=3)
    }
    }
  })
  
  output$plot60 <- renderPlot({
    
    days_count = as.numeric(input$daterange1[2] - input$daterange1[1] + 1)
    if (days_count<1) {
      stop(safeError("End date should be after start date."))
      
    }
    
    b=input$list_of_patients
    #if (b=="Select"||b==0) {
     # photo=paste(photos_directoy, "game_scores.png", sep="/")
      #pic = readPNG(photo)
      #plot.new()
      #return(grid::grid.raster(pic))
    #}
    if (b!="Select"&&b!=0)
    {
    c=paste("p0", b, sep="")
    if (as.numeric(b)>9) {
      c=paste("p", b, sep="")
    }
    d=paste(c, "csv", sep = ".")
    a=paste(scores_directory, d, sep = "/")
    
    if (file.exists(a)) {
      scoresFile <- read.csv(a, stringsAsFactors = FALSE, sep="\t")
    }
    else
      stop(safeError("Missing data file for the patient."))
    
    filtered <-
      scoresFile %>%
      filter(date_time >= input$daterange1[1],
             date_time <= input$daterange1[2])
    filtered <- filtered[ ,c("score", "date_time")]
    filtered <- unique(filtered)
    
    date_error = as.numeric(input$daterange1[2]-as.Date(scoresFile[c(1),"date_time"]))+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    maximum=as.numeric(count(scoresFile))
    date_error = as.numeric(as.Date(scoresFile[c(maximum),"date_time"])-input$daterange1[1])+1
    if (date_error<1) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    a <- filtered[ ,c("date_time")]
    b <- filtered[ ,c("score")]
    mydf = data.frame(Date=a, Scores=b)
    #print(mydf)
    
    #mydf$ddate <- factor(strftime(mydf$Date,format="%d"))
    
    #mydf$day <- factor(strftime(mydf$Date,format="%a"),
     #                  levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
    
    #mydf$day <- factor(mydf$day,levels=unique(mydf$day))
    
    #mydf$week <- factor(strftime(mydf$Date,format="%V"))
    #mydf$week <- factor(mydf$week,levels=unique(mydf$week))
    
    #mydf$month <- strftime(mydf$Date,format="%b")
    #mydf$month <- factor(mydf$month,levels=unique(mydf$month))
    
    #mydf$year <- strftime(mydf$Date,format="%Y")
    #mydf$year <- factor(mydf$year,levels=unique(mydf$year))
    
    #mydf$time <- strptime(mydf$Date,format="%Y-%m-%dT%H:%M")
    #mydf$time <- strftime(mydf$time,format="%H:%M")
    #mydf$time <- factor(mydf$time,levels=unique(mydf$time))
    #z <- strptime(mydf$Date, "%d%b%Y")
    #aqw=Sys.time()
    #aqw=strftime(aqw, format ="%T")
    #print(aqw)
    
    #mydf2 = mydf[!duplicated(mydf[ , c("ddate")]),]
    #days_count = count(mydf2)
    
    
    
    filtered2 <- scoresFile
    filtered2 <- filtered2[ ,c("score", "game_id")]
    filtered2 <- unique(filtered2)
    
    #print(count(filtered2))
    half_point = round((min(filtered2$game_id)+max(filtered2$game_id))/2)
    #print(half_point)
    
    
    first_half <-
      filtered2 %>%
      filter(game_id >= 1,
             game_id <= as.numeric(half_point))
    
    #print(first_half)
    average1 = mean(first_half$score)
    #print(average1)
    
    
    second_half <-
      filtered2 %>%
      filter(game_id >= as.numeric(half_point+1),
             game_id <= count(filtered2))
    
    #print(second_half)
    average2 = mean(second_half$score)
    #print(average2)
    
    
    #if (input$input2Id == TRUE) {
    # mydf$Date <- factor(mydf$Date, levels = mydf$Date[order(mydf$Scores)])
    #}
    if (as.numeric(count(mydf))==0) {
      stop(safeError("No data for the patient in this time period."))
    }
    
    last=as.numeric(count(mydf))
    for (i in 1:last) {
      #mydf$Date=as.Date(mydf$Date)
      #mydf$Date=as.POSIXlt(mydf$Date, format="%Y-%m-%dT%H:%M")
      mydf$Date=as.POSIXct(mydf$Date, format="%Y-%m-%dT%H:%M:%S")
      mydf$Date[i]=mydf$Date[i]+ lubridate::days(1)
      #day_and_date=paste(mydf$day, mydf$ddate, sep = " ")
      #mydf$day_and_date=paste(day_and_date, mydf$month, sep = " ")
      #mydf$day_and_date=paste(mydf$day_and_date, mydf$time, sep = " ")
    }
    #print(mydf)
    scaling=data.frame(asdf=unique(as.Date(mydf$Date)))
    scale_count=count(scaling)
    Switch2=F
    Switch=F
    if (scale_count>45) {
      Switch2=T
    }
    if (scale_count>7 & scale_count<46) {
      Switch=T
    }
    
    
    first_date=first(as.Date(mydf$Date))
    first_date=paste(first_date, "00:00:00", sep = " ")
    first_date=as.POSIXct(first_date)
    
    last_date=last(as.Date(mydf$Date))+ lubridate::days(1)
    last_date=paste(last_date, "00:00:00", sep = " ")
    last_date=as.POSIXct(last_date)
    
    
    ggplot(mydf, aes(x=Date, y=Scores))+ #trendline(x1,y1,model="line2P",summary=TRUE) #geom_count()
      #scale_x_datetime(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
      scale_x_datetime(breaks = "1 day", minor_breaks = "1 days",labels=date_format("%a %d %b"),name="Date range")+
      #scale_x_date(breaks = "1 day",minor_breaks = "1 days",labels = date_format("%a %d %b"),name="Date range")+
      #geom_smooth(data = mydf, aes(x=game_id, y=Scores), method = lm, se = FALSE)
      theme_light() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=12,face="bold"))+
      {if(Switch)theme(axis.text.x = element_text(angle = 45, hjust = 1))}+
      {if(Switch2)theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))}+
      geom_point() + xlab("Date range") + ylab("Game Score (0-100)")+
      #scale_y_continuous(breaks = seq(0, 100, 2)) +
      #geom_segment(aes(x = 0, y = as.numeric(average1), xend = Inf, yend = as.numeric(average2)), size = 1, color="Blue", arrow = arrow(length = unit(1,"cm")))+
      geom_segment(aes(x = first_date, y = as.numeric(average1), xend = last_date, yend = as.numeric(average2)), size = 0.5, color="Blue")
      #geom_text(aes(last(day_and_date),as.numeric(average2),label = "Trend line of scores", vjust = "top", hjust="right"), size=3)
    
    
    }
  })
  
  
}




shinyApp(ui = ui, server = server)
#}
