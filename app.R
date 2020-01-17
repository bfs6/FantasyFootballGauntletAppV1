####Read in Libraries####
library(shiny)
library(shinydashboard)
library(DT)


####Source Functions####
source("Functions.R")
user <- "Basil"
opposing_user <- "Geoff"
week = 10
season = 2019
available_players <- get_available_players(user, opposing_user, week, season, ppr = T)


####User Interface####
ui <- fluidPage(
  ##App Title
  headerPanel(paste0(user, "'s ", "Weekly Picks")),
  
  ##Sidebar
  fluidRow(
    column(3,
           selectInput("positions", "Positions:", 
                       unique(available_players$position))
    )
  ),
  
  fluidRow(
    column(3,
           h3(textOutput("pickCaption")),
           dataTableOutput("pickData"), 
           offset = 0
    ),
    column(8, 
           h3(textOutput("caption")),
           dataTableOutput("table"),
           offset = 1
    )
  )
  
)


####Server####
server <- function(input, output){
  #fullDF <- reactive({available_players %>% filter(position == input$positions)})
  
  headerText <- reactive({
    paste0("Available ", input$positions, "s", " - Week ", week)
  })
  
  pickText <- reactive({
    paste("Week", week, "Picks", sep = " ")
  })
  
  output$caption <- renderText({
    headerText()
  })
  
  output$pickCaption <- renderText({
    pickText()
  })
  
  reset <- reactiveValues(sel = "")
  output$table <- DT::renderDataTable({
    input$table_rows_selected
    fullDF <- reactive({datatable(available_players %>% filter(position == input$positions), selection = list(mode = 'multiple', selected = reset$sel))})
    fullDF()
  })
  
  observe({
    if(input$positions %in% c("WR", "RB")){
      if(length(input$table_rows_selected) > 4){
        reset$sel <- setdiff(input$table_rows_selected, input$table_row_last_clicked)
      }else{
        reset$sel <- input$table_rows_selected
      }
    }else if(input$positions %in% c("TE", "QB", "K", "DEF")){
      if(length(input$table_rows_selected) > 3){
        reset$sel <- setdiff(input$table_rows_selected, input$table_row_last_clicked)
      }else{
        reset$sel <- input$table_rows_selected
      }
    }
  })
  
  output$pickData <- DT::renderDataTable({
    shiny::validate(need(length(input$table_rows_selected) > 0, "Select Players"))
    rows_sel <- input$table_rows_selected
    df_full <- available_players %>% filter(position == input$positions)
    datatable(df_full[rows_sel, c("name", "position", "teamAbbr", "weekProjectedPts", "opponent_used")], rownames = NULL)
  })
  
}

shinyApp(ui, server)
