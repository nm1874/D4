#Starter file for any Shiny dashboard app
#This should replace the default app.r that displays Old Faithful data
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(rsconnect)

#The user interface
header <- dashboardHeader(title = "D4 According to Biggs")
sidebar <- dashboardSidebar(
  width = 100,
  actionButton("btninit", "Initialize"),
  actionButton("btni","Apply i"),
  actionButton("btnr","Apply r"),
  actionButton("btns","Apply s"),
  actionButton("btnt","Apply t"),
  actionButton("btnw","Apply w"),
  actionButton("btnx","Apply x"),
  actionButton("btny","Apply y"),
  actionButton("btnz","Apply z")
)
body <- dashboardBody(
  fluidRow(
    column(
      width = 12,
      plotOutput("configs", height =200)
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("Square", height = 300)
    ),
    column(
      width = 6,
      dataTableOutput("multable")
    )
  )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("d4calc.R")

#Variables that are shared among server functions
D4DF <- D4.makeDataFrame()
config <- "ABCD"

#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  #Initialization
  output$configs <- renderPlot(D4.showConfigs(D4DF))
  output$Square <- renderPlot(D4.showSquare(config))
  tbl <-outer(D4DF$name,D4DF$name,vD4.multiply,DF=D4DF)
  colnames(tbl) <- D4DF$name
  rownames(tbl) <- D4DF$name 
  #Use options to suppress the fancy controls
  output$multable <- renderDataTable(tbl, options = list(dom = "t"))
  #Functions that respond to events in the input
  observeEvent(input$btninit,{
    config <<- "ABCD"
    output$Square <- renderPlot(D4.showSquare(config))
  })
  
  observeEvent(input$btni,{
    config <<- D4.apply("i",config)
    output$Square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnr,{
    config <<- D4.apply("r",config)
    output$Square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btns,{
    config <<- D4.apply("s",config)
    output$Square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnt,{
    config <<- D4.apply("t",config)
    output$Square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnw,{
    config <<- D4.apply("w",config)
    output$Square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnx,{
    config <<- D4.apply("x",config)
    output$Square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btny,{
    config <<- D4.apply("y",config)
    output$Square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnz,{
    config <<- D4.apply("z",config)
    output$Square <- renderPlot(D4.showSquare(config))
  })
}

#Run the app
shinyApp(ui = ui, server = server)
