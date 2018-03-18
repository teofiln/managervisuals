# install.packages("package name") - if you don't have them
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)

header <- dashboardHeader(title="Manager.io::visualisations", titleWidth = 250)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("File input", tabName="inputs", icon = icon("dashboard"),
           radioButtons("sep", "Choose separator", choices=c("tab", "comma"), selected="comma", inline=TRUE),
           fileInput('file', 'Choose a CSV file to upload', accept = '.csv' )),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
           badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  fluidRow(
    box(plotlyOutput("plot", height = "1000px"))
    #box(dataTableOutput("DT"))
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  getSeparator <- reactive({
    Sep <- ifelse(input$sep=="tab", "\t", ",")
    return(Sep)
  })
  
  getFile <- reactive({
    validate(
      need(input$file != "" , "Please select a data set")
    )
    inFile <- input$file
    return(inFile)
    
  })
  
  
  loadData <- reactive({
    csvfile <- read.delim(getFile()$datapath, sep=getSeparator(), stringsAsFactors = FALSE) 
    return(csvfile)
  })
  
  makePlot <- reactive({
    csvfile <- loadData()
    pp <- ggplot(csvfile, aes(x=Account, y=Amount/1000, fill=Group)) +
      geom_bar(stat="identity", position = "dodge", width=.6) +
      #scale_y_continuous(trans="log", breaks=10^{1:5}) +
      scale_fill_discrete(name="")+
      xlab("") + ylab("") +
      coord_flip()
    
    pp <- plotly_build(pp)
    return(pp)
  })
  
  # render the plot
  output$plot <- renderPlotly({
    makePlot()
  })
   
  # render the table
  #output$DT <- renderDataTable({
  #  loadData()
  #})
  
}

shinyApp(ui, server)