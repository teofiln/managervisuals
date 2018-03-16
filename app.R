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
           radioButtons("sep", "Choose separator", choices=c("tab", "comma"), selected="tab", inline=TRUE),
           fileInput('file', 'Choose a CSV file to upload', accept = '.csv' )),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
           badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  fluidRow(
    box(plotlyOutput("plot", height = "1000px")),
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
  
  makePlot <- reactive({
    
    pp <- ggplot(cash_summary(csvfile), aes(x=Account, y=Amount/1000, fill=Group)) +
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
  output$DT <- renderDataTable({
    loadData()
  })
  
}

shinyApp(ui, server)