####Read in Libraries####
library(shiny)
library(shinydashboard)


####Source Functions####
source("Functions.R")
user <- "Basil"
opposing_user <- "Geoff"
week = 10
season = 2019
available_players <- get_available_players(user, opposing_user, week, season, ppr = T)


####User Interface####
ui <- pageWithSidebar(
  ##App Title
  headerPanel("Weekly Picks"),
  
  ##Sidebar
  sidebarPanel(
    selectInput("positions", "Positions:", 
                unique(available_players$position))
  ),
  
  ##Main Panel
  mainPanel(
    h3(textOutput("caption")),
    dataTableOutput("table")
  )
)


####Server####
server <- function(input, output){
  headerText <- reactive({
    paste0("Available ", input$positions, "s for ", user, " - Week ", week)
  })
  
  output$caption <- renderText({
    headerText()
  })
  
  output$table <- renderDataTable({
    filter(available_players, position == input$positions)
  })
}

shinyApp(ui, server)
