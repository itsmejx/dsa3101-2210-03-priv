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
library(tidyverse)
library(plotly)
library(httr)
library(jsonlite)
library(RColorBrewer)
library(dplyr)
library(chron)
library(DT)

Sys.setenv(TZ = 'Etc/GMT+0')

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


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Traffic manager"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("dates_in", 
                           "Select dates",
                           value='2022-08-18'),
            selectInput("choose_aisle",
                        "Select aisle number", 
                        selected = "1",
                        choices=c("1", "2", "3", "4", "5",
                                  "6", "7", "8", "9", "10"),
                        multiple=TRUE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Aisle Traffic",
                     plotOutput("timePlot")
            ),
            tabPanel("Aisle Traffic (plotly)",
                     plotlyOutput("timePlot2")
            ),
          )
        )
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
      # column(5, dataTableOutput("data_display_right"), align="center")
    )
)


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
