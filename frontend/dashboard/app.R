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
# library(treemap)
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


## Data reading/ api querying

files <- list.files(path="../DB/DB", pattern="*.json", full.names=TRUE, recursive=FALSE)

data = data.frame(id = numeric(0), camera = numeric(0), time = character(0))
for (f in files) {
  temp = read_json(f)
  for (i in 1:length(temp$id)) {
    idx = temp$id[[i]]
    occ = temp$camera[[i]]
    dt = temp$datetime[[i]]
    subtemp  = data.frame(id = idx, camera = occ, time = dt)
    data = rbind(data, subtemp)
  }
}

aisleData = read.csv("../DB/aisleProduce.csv")

## Data processing
data = data %>% mutate(date = sapply(strsplit(data$time, " "), "[[", 1),
                       time = chron(times. = sapply(strsplit(data$time, " "), "[[", 2))) %>% 
  mutate(camera = as.factor(camera))
data$camera = factor(data$camera,levels=c("1","2","3","4","5","6","7","8","9","10"))




###UI
##Header
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


##Sidebar
siderbar <-
  dashboardSidebar(
    width=200,
    sidebarMenu(
      id='sidebar',
      style="position: relative; overflow: visible",
      menuItem("Daily Insights", tabName='dashboard',icon=icon('magnifying-glass-chart')),
      menuItem("Heat Map", tabName='heat_map',icon=icon('map')),
      menuItem("Traffic Manager", tabName="aisle_traffic",icon=icon('traffic-light')),
      div(id = "heat_map_cr",
          conditionalPanel("input.sidebar === 'heat_map'",
                           dateInput("date",
                                     "Select date",
                                     value='2022-11-09'),
                           sliderInput('time',
                                       'Time (Hours)',
                                       value = 9, min = 9, max = 23, post = ":00")
                           )
          ),
      div(id = "aisle_traffic_cr",
          conditionalPanel("input.sidebar === 'aisle_traffic'",
                           dateInput("dates_in",
                                     "Select date",
                                     value='2022-11-09'),
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
    tags$style(HTML(".fa-fire{font-size:33px}")),
    tags$style(HTML(".fa-chart-line{font-size:33px}")),
    tags$style(HTML(".fa-snowflake{font-size:33px}")),
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
                 h1(paste0("Today's Date: ",format(Sys.Date(),format="%d %B %Y (%A)")),
                    style = "color:darkblue", align = "center"),
                 tags$hr()
             ),
             h2(paste0(format(Sys.Date()-1,format="%d %b %Y"), "'s insights")),
             fluidRow(
               valueBoxOutput("daily_count"),
               valueBoxOutput("most_crowded"), #TO-DO: Most crowded time
               valueBoxOutput("least_crowded") #TO-DO: Least crowded time
             ),
             h2(paste0("This Week's Performance")),
             fluidRow(
               valueBoxOutput("weekly_count",width=6),
               valueBoxOutput("weekly_performance",width = 6)
             )
    ),
    
    ###Heat Map
    tabItem( tabName = "heat_map",
             h1(paste0("Heat Map")),
             div(
               plotlyOutput("heatmap_plot", width = "100%"),
             )
    ),
    
    ###Aisle Traffic
    tabItem( tabName = "aisle_traffic",
             h1(paste0("Traffic Manager")),
             div(
               plotlyOutput("timePlot"),
               hr(),
               h2("Aisle products"),
               fluidRow(
                 column(12, strong("Choose an aisle above to see what is in store!"))
               ),
               hr(),
               fluidRow(
                 column(2, "Aisle(s) chosen:", textOutput("data_display_text")),
                 column(10, dataTableOutput("data_display"), align="center")
               )
             )
    )
  )
)

ui <- dashboardPage(header,siderbar,body)

# Define server logic required to draw a histogram
server <- function(input, output) {
    sub_df <- reactive({filter(data, data.table::between(date, input$dates_in, 
                                            input$dates_in), 
                               camera %in% input$choose_aisle)
      })
    
    
    ##output for heatmap
    output$heatmap_plot <- renderPlotly({
      
      data_heat <- data %>% mutate(time = as.numeric(chron::hours(time))) %>% 
        group_by(date,time, camera) %>% mutate(count = n()) %>% ungroup()
      
      cur_date = input$date
      cur_time = input$time
      
      subset <- data_heat %>% filter(date == cur_date & time == cur_time )
      if (nrow(subset) == 0) {
        col1 <- rep(c(1:10)) ## Ownself generate aisle values
        col2 <- rep(0,10) ## customer counts
        plot_data <- as.data.frame(cbind(col1, col2))
        plot_data$col1 <- as.factor(plot_data$col1)
        colnames(plot_data) <- c("Aisles","Customer_Count")
        
        plot_data <- plot_data %>% mutate(Aisles = as.factor(Aisles), t = rep(1, nrow(plot_data))) ## t is just a dummy var for geom bar to have equal height
        
        a <- ggplot(plot_data, aes(fill=Customer_Count,y=t,x=Aisles)) +
          geom_bar(position="fill", stat='identity') +
          scale_fill_gradient(low = "#FFFF99" , high = "#FFFF99", na.value = "#FFFF99") +
          theme(panel.background = element_blank(), axis.ticks = element_blank(),axis.text.y=element_blank()) +
          labs(title ="Heat Map of Customers in Grocery Store", y = "", color = "Customer Density", fill = "Customer Count\n(Hourly)")
        
        
      } else {
        subset <- subset %>% select(camera, count)
        plot_col2 <- matrix(0,10)    ## Need introduce 0 values for aisles with no customers
        for (i in c(1:nrow(subset))) {
          cur_aisle = subset[[i, "camera"]]
          plot_col2[cur_aisle] = subset[[i,"count"]]
        }
        
        aisle_col <- c(1:10)    ## X value --> aisle numbers
        plot_data <- as.data.frame(cbind(aisle_col, plot_col2))
        colnames(plot_data) <- c("Aisles","Customer_Count")
      
      
      plot_data <- plot_data %>% mutate(Aisles = as.factor(Aisles), t = rep(1, nrow(plot_data))) ## t is just a dummy var for geom bar to have equal height
      
      a <- ggplot(plot_data, aes(fill=Customer_Count,y=t,x=Aisles)) +
        geom_bar(position="fill", stat='identity') +
        scale_fill_gradient2(low = "#FFFF99" , mid = "#FF6600", high = "red", midpoint = mean(data_heat$count), na.value = "#FFFF99") +
        theme(panel.background = element_blank(), axis.ticks = element_blank(),axis.text.y=element_blank()) +
        labs(title ="Heat Map of Customers in Grocery Store", y = "", color = "Customer Density", fill = "Customer Count\n(Hourly)")
      }
      ggplotly(a)
    })
    
  
  ##logic for aisleTraffic
  output$timePlot <- renderPlotly({
    sub_df2 <- group_by(sub_df(), camera, hour = chron::hours(time)) %>%
      mutate(hour = chron(times. = paste(as.character(hour),":00:00"), format = "h:m:s")) %>% 
      summarise(num_people = n(), .groups="drop") 
    p <- ggplot(sub_df2, aes(x=hour, y=num_people, col=camera)) + geom_point() + 
      geom_line() + scale_x_chron(format = "%H:%M") +
      labs(title="Traffic per Aisle", y="Number of People", x="Time", col="Aisles")
    ggplotly(p)
  })
  
  
  ##logic for mainPage
  output$daily_count <- renderValueBox({
    yest_date <- as.character(Sys.Date()-1)
    daily_count_df <- data %>% 
      filter(date==yest_date) %>% 
      distinct(id,date,.keep_all=TRUE) %>% 
      group_by(date) %>% 
      summarize(ct=n())
    valueBox(
      VB_style(daily_count_df$ct[1],"font-size:60%;"),
             "Customer Count",
             icon=icon('chart-line'),
             color="teal")
  })
  
  output$weekly_count <- renderValueBox({
    yest_date <- as.character(Sys.Date()-1)
    weekly_count_df <- data %>%
      filter((date>=as.character(Sys.Date()-8))&(date<=as.character(Sys.Date()-1))) %>% 
      distinct(id,date,.keep_all=TRUE) %>% 
      group_by(date) %>% 
      summarize(ct=n())
    valueBox(
      VB_style(sum(weekly_count_df$ct),"font-size:60%;"),
      "This Week's Customer Count",
      icon=icon('chart-line'),
      color="fuchsia",
      width=6)
  })
  
  output$weekly_performance <- renderValueBox({
    weekly_count_df <- data %>%
      filter((date>=as.character(Sys.Date()-8))&(date<=as.character(Sys.Date()-1))) %>% 
      distinct(id,date,.keep_all = TRUE) %>% 
      group_by(date) %>% 
      summarize(ct=n())
    curr_wk_count <- sum(weekly_count_df$ct)
    last_weeks_count <- 400
    percent_change <- round(((curr_wk_count - last_weeks_count)/(last_weeks_count)) * 100,digits=3)
    valueBox(
      VB_style(paste0(ifelse(percent_change>0,'+','-'),abs(percent_change),'%'),"font-size:60%;"),
      "Performance against last week",
      icon=icon('chart-line'),
      color=ifelse(percent_change>0,'green','red'),
      width=6
    )
  })
  
  ## change date string to fit data
  output$most_crowded <- renderValueBox({
    yest_date <- as.character(Sys.Date()-1)
    daily_crowd_df <- data %>%
      filter(date==yest_date) %>% 
      group_by(camera) %>% 
      summarize(ct=n()) %>% 
    slice(which.max(ct))
    most_crowded <- daily_crowd_df$camera
    valueBox(
      VB_style(paste0("Aisle"," ",most_crowded),"font-size:60%;"),
      "Most Popular Aisle",
      icon=icon('fire'),
      color="orange"
    )
  })
  
  ## change date string to fit data
  output$least_crowded <- renderValueBox({
    yest_date <- as.character(Sys.Date()-1)
    daily_crowd_df <- data %>% 
      filter(date==yest_date) %>%
      group_by(camera) %>% 
      summarize(ct=n()) %>% 
      slice(which.min(ct))
    least_crowded <- daily_crowd_df$camera
    valueBox(
      VB_style(paste0("Aisle"," ",least_crowded),"font-size:60%;"),
      "Least Popular Aisle",
      icon=icon('snowflake'),
      color="aqua"
    )
  })
  
  main_filter <- reactive({filter(aisleData, aisle %in% input$choose_aisle)})
  
  output$data_display_text <- renderText({
    disp_text <- toString(input$choose_aisle)
  })
  
  output$data_display <- renderDataTable({
    left_table <- group_by(main_filter()) %>% 
      select(!c("side")) %>% 
      arrange(aisle, type)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
