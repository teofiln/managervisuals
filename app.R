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
    uiOutput("checks"),
    box(plotlyOutput("plot")),
    box(dataTableOutput("DT"))
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
    csvfile <- read_delim(getFile()$datapath, delim=getSeparator()) 
    return(csvfile)
  })

  # reactive UI
    output$checks <- renderUI({
      DD <- loadData()
      box(width=3,
        checkboxGroupInput(inputId = 'choices', 
                           label = 'Select account(s)', 
                           choices = unique(DD$Account), 
                           selected = unique(DD$Account)[1:3])    
      )
  })
  
  dataSub <- reactive({
    D <- loadData() %>% filter(Account %in% input$choices)
    return(D)
  })
    
  makePlot <- reactive({
    pp <- ggplot(dataSub(), aes(x=Date, y=Amount, fill=Group)) +
      geom_bar(stat="identity", position = "stack", width=.6) +
      #geom_point(size=3, pch=21, alpha=.6) +
      scale_y_continuous(trans="log", breaks=10^{1:10}) +
      scale_fill_brewer(name="", palette = "Set1") +
      xlab("") + ylab("") +
      facet_wrap("Account", labeller = label_wrap_gen()) +
      #coord_flip() + 
      theme(legend.position = "top")
    
    pp <- ggplotly(pp)
    return(pp)
  })
  
  # render the plot
  output$plot <- renderPlotly({
    makePlot()
  })
   
  # render the table
  output$DT <- renderDataTable({
    dataSub()
  })
  
}

shinyApp(ui, server)