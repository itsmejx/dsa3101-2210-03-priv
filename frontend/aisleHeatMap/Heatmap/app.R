#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dygraphs)
library(rgdal)
library(flexdashboard)
library(tidyverse)
library(plotly)
library(jsonlite)
library(DT)
library(jsonlite)
library(xts)
library(dplyr)
library(tmap)

##add fake information here first
data = readLines("fakeData.json")
fake_json = lapply(data, fromJSON)
data = data.frame(id = numeric(0), camera = numeric(0), time = character(0))
empty_table = data.frame()
for (i in 1:length(fake_json)) {
  idx = i
  occ = fake_json[[i]]$occurrence
  temp  = data.frame(id = idx, camera = occ[1], time = occ[2])
  temp$time = as.POSIXct(strptime(temp$time, format = "%Y-%m-%d-T%H:%M:%S.000Z"))
  temp$date = as.Date(temp$time)
  temp$time = format(as.POSIXct(temp$time), format = "%H:%M:%S")
  data = rbind(data, temp)
}

## Add count TRY ONLY
data = cbind(data, count = sample(c(1:10), nrow(data), replace = TRUE))

dataxts <- NULL
aisles <- unique(data$camera)
dates <- unique(data$date)
datedata <- data[data$date == dates[2],]  ## chose 2 coz got more

for (l in 1:length(aisles)){
  daisles <- datedata[datedata$camera == l,]
  dd <- xts(daisles$count, order.by=as.POSIXct(daisles$time, format = "%H:%M:%S"))
  dataxts <- cbind(dataxts,dd) ## i think one time can only take one value
}
colnames(dataxts) <- sort(as.numeric(aisles))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("HeatMap of MAMAshop"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
