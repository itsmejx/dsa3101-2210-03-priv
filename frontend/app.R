#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(httr)
library(jsonlite)
library(RColorBrewer)
library(dplyr)


#data input
# hdb <- readRDS('hdb_locs.rds')
flask_url <- "http://flask:5000/"

data = readLines("aisleTraffic/fakeData.json")
fake_json = lapply(data, fromJSON)
data = data.frame(id = numeric(0), camera = numeric(0), time = character(0))
for (i in 1:length(fake_json)) {
  idx = i
  occ = fake_json[[i]]$occurrence
  temp  = data.frame(id = idx, camera = occ[1],time = occ[2])
  data = rbind(data, temp)
}
##

data = data %>% mutate(date = sapply(strsplit(data$time, "-T"), "[[", 1),
                time = strsplit(sapply(strsplit(data$time, "-T"), "[[", 2), ".000Z"))
  # mutate(id = as.factor(id))
# class(data$id)
# class(data)
# summary(data)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Traffic manager"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("dates_in", 
                           "Select start and end dates",
                           start='2020-01-01',
                           end='2022-02-28'),
            selectInput("choose_aisle",
                        "Select aisle number", 
                        choices=c("1", "2", "3", "4", "5",
                                  "6", "7", "8", "9", "10"),
                        multiple=TRUE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Time series",
                     plotOutput("timePlot")
            ),
            tabPanel("Time series (plotly)",
                     plotlyOutput("timePlot2")
            ),
          )
        )
    ),
    
    hr(),  
    h2("Batch Predictions"),
    
    fluidRow(
      column(5, "Input file should contain two columns:", 
             strong("town (character)"), "and" ,
             strong("storey (numeric)."),
             "There should also be a header row."
             ),
      column(4, fileInput("file1", "Upload csv file", accept=".csv")),
      column(3, downloadButton("download", "Download"))
    ),
    fluidRow(
      column(12, tableOutput("data_display"), align="center")
    )
)
?filter

# Define server logic required to draw a histogram
server <- function(input, output) {
    sub_df <- reactive({filter(data, between(date, input$dates_in[1], 
                                            input$dates_in[2]), 
                               camera %in% input$choose_aisle)
      })
    
  output$timePlot <- renderPlot({
    sub_df2 <- group_by(sub_df(), date, camera) %>% 
      summarise(ct = count(id), .groups="drop") 
    ggplot(sub_df2, aes(x=date, y=ct, col=camera)) + geom_point() + 
      geom_line() + 
      labs(title="Traffic per aisle", y="number people", x="Time", col="Aisles")
  })
  output$timePlot2 <- renderPlotly({
    sub_df2 <- group_by(sub_df(), date, aisle_choice) %>% 
      summarise(ct = count(id), .groups="drop") 
    p <- ggplot(sub_df2, aes(x=date, y=ct, col=camera)) + geom_point() + 
      geom_line() + 
      labs(title="Traffic per aisle", y="number people", x="Time", col="Aisles")
    ggplotly(p)
  })
  
  output$data_display <- renderTable({
    head(output_data(), n=5)
  })
  
  input_data <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    shiny::validate(need(ext == "csv", "Please upload a csv file"))
    read.csv(file$datapath, header=TRUE)
  })
  
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
