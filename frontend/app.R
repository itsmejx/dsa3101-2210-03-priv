#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(plotly)
library(httr)
library(jsonlite)
library(RColorBrewer)
library(dplyr)
library(chron)
library(DT)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
library(timevis)
options(spinner.color="#FF6272")

Sys.setenv(TZ = 'Etc/GMT+0')

source('helper_functions.R')

#data input
# hdb <- readRDS('hdb_locs.rds')
flask_url <- "http://flask:5000/"

data = readLines("aisleTraffic/updatedFakeData.json")
aisleData = read.csv("aisleTraffic/aisleProduce.csv")
fake_json = lapply(data, fromJSON)
data = data.frame(id = numeric(0), camera = numeric(0), time = character(0))
for (i in 1:length(fake_json)) {
  idx = i
  occ = fake_json[[i]]$occurrence
  temp  = data.frame(id = idx, camera = occ[1],time = occ[2])
  data = rbind(data, temp)
}
##

data = data %>% mutate(date = sapply(strsplit(data$time, " "), "[[", 1),
                       time = chron(times. = sapply(strsplit(data$time, " "), "[[", 2))) %>% 
  mutate(camera = as.factor(camera))
data$camera = factor(data$camera,levels=c("1","2","3","4","5","6","7","8","9","10"))

header <-
  dashboardHeader( title = HTML("BoothY"),
                   disable= FALSE,
                   titleWidth=200,
                   dropdownMenuCustom( type = 'message',
                                       customSentence = customSentence,
                              messageItem(
                                from = "cwj.darren@u.nus.edu",
                                message="",
                                icon = icon("envelope"),
                                href="mailto:cwj.darren@u.nus.edu"
                              ),
                              icon = icon('comment')))

siderbar <-
  dashboardSidebar(
    width=200,
    sidebarMenu(
      id='sidebar',
      style="position: relative; overflow: visible",
      menuItem( "Daily Insights",tabName='dashboard',icon=icon('magnifying-glass-chart')),
      menuItem("Heat Map", tabName='heat_map',icon=icon('map')),
      menuItem("Traffic Manager", tabName="aisle_traffic",icon=icon('traffic-light')),
      div(id = "aisle_traffic_cr",
          conditionalPanel("input.sidebar === 'aisle_traffic'",
                           dateInput("dates_in",
                                     "Select date",
                                     value='2022-08-18'),
                           selectInput("choose_aisle",
                                       "Select aisle number",
                                       selected = "1",
                                       choices=c("1", "2", "3", "4", "5","6", "7", "8", "9", "10"),
                                       multiple=TRUE)
                           )
          )
    )
  )
                   
body <- dashboardBody(
  tags$head(
    tags$script("document.title = 'BoothY Dashboard'"),
    
    # Styles
    tags$style(HTML(".small-box {height:65px}")),
    tags$style(HTML(".fa {font-size: 35px;")),
    #tags$style(HTML(".glyphicon {font-size:33px;}")), # use glyphicon package
    tags$style(HTML(".fa-magnifying-glass-chart{font-size:20px;")),
    tags$style(HTML(".fa-map{font-size:20px;")),
    tags$style(HTML(".fa-traffic-light{font-size:20px;}")),
    tags$style(HTML('
                    .skin-blue .main-header .navbar {
                    background-color: #006272;
                    }
                    
                    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                    background-color: #006272;}')),
    tags$style(HTML("hr {border-top: 1px solid #000000;}")),
    tags$style(HTML(".shiny-output-error{visibility:hidden;}")),
    tags$style(HTML(".shiny-output-error:before{visibility:hidden;}")),
    
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu{width:10px;font-size:10px;padding:1px;margin:1px;}')),
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu > h4 {width:0px;font-size:0px;padding:0px;margin:0px;}')),
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu > p {width:0px;font-size:0px;padding:0px;margin:0px;}')),
  ),
  
  ##Dashboard body ------------------------------
  tabItems(
    ### Daily Insights
    tabItem( tabName = "dashboard",
             div(id = 'main_loading_msg',
                 h1('LOADING...',
                    style = "color:darkblue", align = "center"),
                 tags$hr()
                 ),
             h1(paste0("18 Aug 2022", "'s insights")),
             fluidRow(
               # valueBoxOutput(),#TO-DO: Weekly Customer's Count
               # valueBoxOutput(),#TO-DO: Most Popular Aisle
               # valueBoxOutput()#TO-DO: Least Popular Aisle
             ),
             h2(paste0("Most crowded")),
             fluidRow(
               # valueBoxOutput(),#TO-DO: Most crowded time
               # valueBoxOutput()#TO-DO: Least crowded time
             ),
             
             #more plots maybe?
             h2(paste0("Do we want to add a graph??")),
             fluidRow( column (width = 6, h4("graphA", align = "center")),
                       column (width = 6, h4("graphB", align = "center"))
                       ),
             ),
    
    ###Heat Map
    tabItem( tabName = "heat_map",
             div(id = 'main_loading_msg',
                 h1('LOADING...',
                    style = "color:darkblue", align = "center"),
                 tags$hr()
                 )
             ),
    
    ###Aisle Traffic
    tabItem( tabName = "aisle_traffic",
             h1(paste0("Traffic Manager")),
             div(
             # sidebarLayout(
               # sidebarPanel(
               #   dateInput("dates_in",
               #             "Select date",
               #             value='2022-08-18'),
               #   selectInput("choose_aisle",
               #               "Select aisle number",
               #               selected = "1",
               #               choices=c("1", "2", "3", "4", "5","6", "7", "8", "9", "10"),
               #               multiple=TRUE)
               #   ),
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel("Aisle Traffic (interactive)",
                            plotlyOutput("timePlot2")
                            ),
                   ),
                 
                   hr(),
                   h2("Aisle products"),

                   fluidRow(
                     column(12, strong("Choose an aisle above to see what is in store!"))
                   ),
                   hr(),
                   fluidRow(
                     column(2, "Aisle(s) chosen:", textOutput("data_display_text")),
                     column(10, dataTableOutput("data_display_left"), align="center")
                   )
               ),
               # )
             )
    )
    )
)


# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Traffic manager"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             dateInput("dates_in", 
#                            "Select date",
#                            value='2022-08-18'),
#             selectInput("choose_aisle",
#                         "Select aisle number", 
#                         selected = "1",
#                         choices=c("1", "2", "3", "4", "5",
#                                   "6", "7", "8", "9", "10"),
#                         multiple=TRUE)
#         ),
#         
#         # Show a plot of the generated distribution
#         mainPanel(
#           tabsetPanel(
#             tabPanel("Aisle Traffic",
#                      plotOutput("timePlot")
#             ),
#             tabPanel("Aisle Traffic (interactive)",
#                      plotlyOutput("timePlot2")
#             ),
#           )
#         )
#     ),
#     
#     hr(),  
#     h2("Aisle products"),
#     
#     fluidRow(
#       column(12, strong("Choose an aisle above to see what is in store!"))
#     ),
#     hr(),
#     fluidRow(
#       column(2, "Aisle(s) chosen:", textOutput("data_display_text")),
#       column(10, dataTableOutput("data_display_left"), align="center")
#       # column(5, dataTableOutput("data_display_right"), align="center")
#     )
# )
ui <- dashboardPage(header,siderbar,body)

# Define server logic required to draw a histogram
server <- function(input, output) {
    sub_df <- reactive({filter(data, data.table::between(date, input$dates_in, 
                                            input$dates_in), 
                               camera %in% input$choose_aisle)
      })
    
  output$timePlot <- renderPlot({
    sub_df2 <- group_by(sub_df(), camera, hour = chron::hours(time)) %>% 
      mutate(hour = chron(times. = paste(as.character(hour),":00:00"), format = "h:m:s")) %>% 
      summarise(ct = n(), .groups="drop") 
    ggplot(sub_df2, aes(x=hour, y=ct, col=camera)) + geom_point() + 
      geom_line() + scale_x_chron(format = "%H:%M") +
      labs(title="Traffic per aisle", y="number of people", x="Time", col="Aisles")
  })
  output$timePlot2 <- renderPlotly({
    sub_df2 <- group_by(sub_df(), camera, hour = chron::hours(time)) %>%
      mutate(hour = chron(times. = paste(as.character(hour),":00:00"), format = "h:m:s")) %>% 
      summarise(ct = n(), .groups="drop") 
    p <- ggplot(sub_df2, aes(x=hour, y=ct, col=camera)) + geom_point() + 
      geom_line() + scale_x_chron(format = "%H:%M") +
      labs(title="Traffic per aisle", y="number of people", x="Time", col="Aisles")
    ggplotly(p)
  })
  
  output$daily_count <- renderValueBox({
    daily_count_df <- data %>% 
      group_by(date) %>% 
      summarize(ct=n())
    valueBox("Daily Customer Count","number of customers yesterday",icon=icon('chart-line'))
  })
  
  main_filter <- reactive({filter(aisleData, aisle %in% input$choose_aisle)})
  
  output$data_display_text <- renderText({
    disp_text <- toString(input$choose_aisle)
  })
  
  # output$data_display_aisle <- renderTable({
  #   a = as.data.frame(input$choose_aisle)
  #   })
  
  output$data_display_left <- renderDataTable({
    left_table <- group_by(main_filter()) %>% 
      select(!c("side")) %>% 
      arrange(aisle, type)
    })
  
  # output$data_display_right <- renderDataTable({
  #   right_table <- group_by(main_filter()) %>% 
  #     filter(side == "right") %>% 
  #     select(!c("side")) %>% 
  #     arrange(aisle, type)
  # })
  
  # output$data_display <- renderTable({
  #   head(output_data(), n=5)
  # })
  
  # input_data <- reactive({filter(data, camera %in% input$choose_aisle)
  # })

  
  output_data <- reactive({
    tmp_list_in <- list(town = input_data()$town, storey = input_data()$storey)
    tmp_out <- POST(flask_url, path="predictions",  
                  body=toJSON(tmp_list_in), verbose(), 
                  content_type_json(), accept_json())
    # check status code
    # handle errors
    predictions <- content(tmp_out) %>% unlist %>% as.numeric()
    cbind(input_data(), predictions=predictions)
  })
  
  
  output$download <- downloadHandler(
    filename = function() {
      paste("predictions-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(output_data(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
