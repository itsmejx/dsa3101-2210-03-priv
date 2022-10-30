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
library(dplyr)

##add fake information here first
data = readLines("../UpdatedFakeData.json")
fake_json = lapply(data, fromJSON)
data = data.frame(id = numeric(0), camera = numeric(0), time = character(0))
empty_table = data.frame()
for (i in 1:length(fake_json)) {
  idx = i
  occ = fake_json[[i]]$occurrence
  temp  = data.frame(id = idx, camera = occ[1], time = occ[2])
  temp$time = as.POSIXct(strptime(temp$time, format = "%Y-%m-%d %H:%M:%S"))
  temp$date = as.Date(temp$time)
  temp$time = format(as.POSIXct(temp$time), format = "%H") ## Just extract hours
  data = rbind(data, temp)
}

colnames(data) = c("id", "aisle","time","date")

## Add count TRY ONLY
data = cbind(data, count = sample(c(1:10),nrow(data),replace=TRUE))

## REAL ADD CUSTOMER COUNT
#data = data %>% group_by(date,time, aisle) %>% count()


data <- data %>% 
  group_by(aisle, date, time) %>%
    summarise(count = sum(count)) %>%
      arrange(by_group=TRUE) %>%
         ungroup()

time_options <- c("12am", paste0(seq(1,11),"am"), paste0(seq(1,11),"pm"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Heatmap"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
          dateInput('date', 'Date', value = "2022-08-18"),
          sliderInput('time','Time (Hours)', value = 0, min = 0, max = 23, post = ":00")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("heatmap_plot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$heatmap_plot <- renderPlot({
      cur_date = input$date
      cur_time = input$time
        
      subset <- data %>% filter(date == cur_date & time == cur_time )
      if (nrow(subset) == 0) {
        col1 <- rep(c(1:10)) ## Ownself generate aisle values
        col2 <- rep(NA,10) ## customer counts
        plot_data <- as.data.frame(cbind(col1, col2))
        plot_data$col1 <- as.factor(plot_data$col1)
        colnames(plot_data) <- c("Aisles","Customer_Count")
      } else {
      subset <- subset %>% select(aisle, count)
      plot_col2 <- matrix(0,10)    ## Need introduce 0 values for aisles with no customers
      for (i in c(1:nrow(subset))) {
        cur_aisle = subset[[i, "aisle"]]
        plot_col2[cur_aisle] = subset[[i,"count"]]
      }
      
      aisle_col <- c(1:10)    ## X value --> aisle numbers
      plot_data <- as.data.frame(cbind(aisle_col, plot_col2))
      colnames(plot_data) <- c("Aisles","Customer_Count")
      }
      
      plot_data <- plot_data %>% mutate(Aisles = as.factor(Aisles), t = rep(1, nrow(plot_data))) ## t is just a dummy var for geom bar to have equal height
       
      ## Plotting
      
      ggplot(plot_data, aes(fill=Customer_Count,y=t,x=Aisles)) +
        geom_bar(position="fill", stat='identity') +
        scale_fill_gradient2(low = "#FFFF99" , mid = "#FF6600", high = "red", midpoint = mean(data$count), na.value = "#FFFF99") +
        theme(panel.background = element_blank(), axis.ticks = element_blank(),axis.text.y=element_blank()) +
        labs(title ="Heat Map of Customers in Grocery Store", y = "", color = "Customer Density")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
