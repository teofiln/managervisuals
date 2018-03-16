library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)

header <- dashboardHeader(title="Manager.io::visualisations", titleWidth = 250)

sidebar <- dashboardSidebar(
  sidebarMenu(
    radioButtons("sep", "Choose separator", choices=c("tab", "comma"), selected="tab", inline=TRUE),
    fileInput('file', 'Choose a CSV file to upload', accept = '.csv' ),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
    # render data table
  dataTableOutput("DT")
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
  
  cash_summary <- function(df.raw){
    df.raw <- read.delim(getFile()$datapath, sep=getSeparator(), stringsAsFactors = FALSE) 
    df.raw$Date <- gsub("X", "", colnames(df.raw)[3])
    # replace the name of column 3, which was the date, with 'amount'
    colnames(df.raw)[3] <- "Amount"
    # replace te name of column 2 with 'account'
    colnames(df.raw)[2] <- "Account"
    # make sure numbers are formated with . for decimal
    df.raw$Amount <- gsub("\\.", "", df.raw$Amount)
    df.raw$Amount <- gsub(",", ".", df.raw$Amount)
    # make sure ammounts are numeric
    df.raw$Amount <- as.numeric(df.raw$Amount)
    #remove accounts with amount 0
    df.raw <- filter(df.raw, Amount != 0)
    
    # the operation just subsets with the range from the total number of rows -4 
    # to the total number of rows to take just the last four rows
    df.raw_sums <- df.raw[(nrow(df.raw)-3):nrow(df.raw),]
    
    # reverse this subsetting with the minus sign
    df.raw <- df.raw[-((nrow(df.raw)-3):nrow(df.raw)),]
    
    #make outflows negative (in case we need that)
    df.raw <- df.raw %>%
      mutate(Direction = case_when(Group == "Inflows" ~ Amount,
                                   Group == "Outflows" ~ -Amount))
    return(df.raw)
  }
  
  
  
  
  loadData <- reactive({
    cash_summary(csvfile)
  })
  
  
  # make reactive table based on input csv
  output$DT <- renderDataTable({
    loadData()
  })
  
}

shinyApp(ui, server)