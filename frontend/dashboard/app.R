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
# flask_url <- "http://localhost:5000/"
# flask_url <- "http://127.0.0.1:5000/"
flask_url <- "http://flask:5000/"

aisleData = read.csv("../DB/aisleProduce.csv")

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
                                     value=as.character(Sys.Date()-1)),
                           sliderInput('time',
                                       'Time (Hours)',
                                       value = 9, min = 9, max = 23, post = ":00")
                           )
          ),
      div(id = "aisle_traffic_cr",
          conditionalPanel("input.sidebar === 'aisle_traffic'",
                           dateInput("dates_in",
                                     "Select date",
                                     value=as.character(Sys.Date()-1)),
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
    tags$style(HTML(".fa-magnifying-glass-chart{font-size:20px;}")),
    tags$style(HTML(".fa-map{font-size:20px;}")),
    tags$style(HTML(".fa-fire{font-size:33px;}")),
    tags$style(HTML(".fa-chart-line{font-size:33px;}")),
    tags$style(HTML(".fa-clock{font-size:33px;}")),
    tags$style(HTML(".fa-snowflake{font-size:33px;}")),
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
               valueBoxOutput("most_crowded"), 
               valueBoxOutput("least_crowded")
             ),
             fluidRow(
               valueBoxOutput("most_crowded_time",width=6),
               valueBoxOutput("least_crowded_time",width=6)
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
               plotlyOutput("heatmap_plot"),
               hr(),
               h2("Aisle Recommendations"),
               hr()),
             div(
               fluidRow(
                 valueBoxOutput("in_aisles"),
                 valueBoxOutput("btw_aisles")
               )

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
    
    ###API Query for stats page
    stats_wk1 = as.character(seq.Date(Sys.Date()-7, Sys.Date()-1, length.out = 7))
    stats_wk2 = as.character(seq.Date(Sys.Date()-14, Sys.Date()-8, length.out = 7))
    
    final_stats_1 = data.frame(id = numeric(0), camera = numeric(0), time = character(0))
    for (i in stats_wk1){
      temp1 = GET(flask_url, path="get_date", query = list(date = i))
      temp1 = content(temp1)
      for (i in 1:length(temp1$id)) {
        idx = temp1$id[[i]]
        occ = temp1$camera[[i]]
        dt = temp1$datetime[[i]]
        subtemp  = data.frame(id = idx, camera = occ, time = dt)
        final_stats_1 = rbind(final_stats_1, subtemp)
      }
    }
    final_stats_2 = data.frame(id = numeric(0), camera = numeric(0), time = character(0))
    for (i in stats_wk2){
      temp2 = GET(flask_url, path="get_date", query = list(date = i))
      temp2 = content(temp2)
      for (i in 1:length(temp2$id)) {
        idx = temp2$id[[i]]
        occ = temp2$camera[[i]]
        dt = temp2$datetime[[i]]
        subtemp  = data.frame(id = idx, camera = occ, time = dt)
        final_stats_2 = rbind(final_stats_2, subtemp)
      }
    }
    
    ##Data processing
    data = rbind(final_stats_1, final_stats_2)
    data = data %>% mutate(date = sapply(strsplit(data$time, " "), "[[", 1),
                           time = chron(times. = sapply(strsplit(data$time, " "), "[[", 2))) %>% 
      mutate(camera = as.factor(camera))
    data$camera = factor(data$camera,levels=c("1","2","3","4","5","6","7","8","9","10"))##for graphs
    
    data_heat <- data %>% mutate(time = as.numeric(chron::hours(time))) %>% 
      group_by(date,time, camera) %>% mutate(count = n()) %>% ungroup()
    
    
    ##logic for heatMap
    plotdata <- reactive({
     date = input$date
     time = input$time
     
     subset <- filter(data_heat, date == input$date, time == input$time)
             
     if (nrow(subset) == 0) {
       col1 <- rep(c(1:10)) ## Ownself generate aisle values
       col2 <- rep(0,10) ## customer counts
       plot_data <- as.data.frame(cbind(col1, col2))
       plot_data$col1 <- as.factor(plot_data$col1)
       colnames(plot_data) <- c("Aisles","Customer_Count")
       
       plot_data %>% mutate(Aisles = as.factor(Aisles), t = rep(1, nrow(plot_data))) ## t is just a dummy var for geom bar to have equal height
      
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
      
      plot_data %>% mutate(Aisles = as.factor(Aisles), t = rep(1, nrow(plot_data))) ## t is just a dummy var for geom bar to have equal height
    }
    })
    

    output$heatmap_plot <- renderPlotly({
      
      if (all(plotdata()$Customer_Count == 0)) {
        l <- ggplot(plotdata(), aes(fill=Customer_Count,y=t,x=Aisles)) +
          geom_bar(position="fill", stat='identity') +
          scale_fill_gradient(low = "#FFFF99" , high = "#FFFF99") +
          theme(panel.background = element_blank(), axis.ticks = element_blank(),axis.text.y=element_blank()) +
          labs(title ="Heat Map of Customers in Grocery Store", y = "", color = "Customer Density", fill = "Customer Count\n(Hourly)")
      } else {
        l <- ggplot(plotdata(), aes(fill=Customer_Count,y=t,x=Aisles)) +
          geom_bar(position="fill", stat='identity') +
          scale_fill_gradient2(low = "#FFFF99" , mid = "#FF6600", high = "red", midpoint = mean(data_heat$count)) +
          theme(panel.background = element_blank(), axis.ticks = element_blank(),axis.text.y=element_blank()) +
          labs(title ="Heat Map of Customers in Grocery Store", y = "", color = "Customer Density", fill = "Customer Count\n(Hourly)")
      }
      ggplotly(l, tooltip = c("Aisles","Customer_Count"))
      
    })
    
    
    output$in_aisles <- renderInfoBox({

      data_in_aisles <- plotdata() %>% slice_min(Customer_Count, n = 3, with_ties=FALSE) %>% arrange(Aisles)
      disp_str <- toString(data_in_aisles$Aisles)
      
      if (all(plotdata()$Customer_Count == 0)) {
        disp_str = "No Optimal Aisle"
      }
      
      infoBox("Within Aisles",disp_str, color = "purple", icon = icon('arrows-left-right-to-line'))
    })
    
    output$btw_aisles <- renderInfoBox({
      subset1 <- plotdata()
     best <- data.frame()
     done <- c()
     while (length(best)<=3) {
       subset <- subset1[!(subset1$Aisles %in% done), ]
       
       if (nrow(subset) == 0) {
         break
       }

       max <- max(subset$Customer_Count)
       mean <- mean(data_heat$count)
       if (max>mean) {
         max_aisle <- as.integer(slice_max(subset, subset$Customer_Count, n=1, with_ties = FALSE)$Aisles)
        if (max_aisle != as.integer(subset$Aisles[[1]])) {   #if at the front then cnt
           bef_max_aisle <- max_aisle - 1
           bef_max_aisle_count <- subset[subset$Aisles == bef_max_aisle, "Customer_Count"]
           if (bef_max_aisle_count < mean) {    ## If aisle before have low crowd
             best <- rbind(best, c(bef_max_aisle,max_aisle))
           }
        }
        if (max_aisle != as.integer(subset$Aisles[[nrow(subset)]])) { #if last then cant also
          aft_max_aisle <- max_aisle + 1
          aft_max_aisle_count <- subset[subset$Aisles == aft_max_aisle, "Customer_Count"]
          if (aft_max_aisle_count < mean) {   ## If aisle after have low crowd
            best <- rbind(best, c(max_aisle,aft_max_aisle))
          }
        }
       }
      else {
        break
      }
      done <- append(done, max_aisle)
     }
     if (nrow(best) == 0) {
       disp <- "No Optimal Aisles"
       }
     else {
       colnames(best) <- c("aisle1","aisle2")
       best <- best %>% rowwise() %>% mutate(toprint = paste("Between Aisles", toString(aisle1), "and", toString(aisle2)))
       disp = ""
       for (row in best$toprint) {
          disp <- paste0(disp,row, sep="<br/>")  
       }
     }
  
    infoBox("End of Aisles", HTML(disp), color = "red", icon=icon('arrows-down-to-line'))
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
  
  ##For aisleTraffic aisle products
  main_filter <- reactive({filter(aisleData, aisle %in% input$choose_aisle)})
  
  output$data_display_text <- renderText({
    disp_text <- toString(input$choose_aisle)
  })
  
  output$data_display <- renderDataTable({
    left_table <- group_by(main_filter()) %>% 
      select(!c("side")) %>% 
      arrange(aisle, type)
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
  ###weekly count
  output$weekly_count <- renderValueBox({
    yest_date <- as.character(Sys.Date()-1)
    weekly_count_df <- data %>% 
      filter((date>=as.character(Sys.Date()-7))&(date<=as.character(Sys.Date()-1))) %>% 
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
  
  ###Weekly performance
  output$weekly_performance <- renderValueBox({
    weekly_count_df <- data %>% 
      filter((date>=as.character(Sys.Date()-7))&(date<=as.character(Sys.Date()-1))) %>% 
      distinct(id,date,.keep_all = TRUE) %>% 
      group_by(date) %>% 
      summarize(ct=n())
    curr_wk_count <- sum(weekly_count_df$ct)
    
    last_week_count_df <- data %>% 
      filter((date>=as.character(Sys.Date()-14))&(date<=as.character(Sys.Date()-8))) %>% 
      distinct(id,date,.keep_all = TRUE) %>% 
      group_by(date) %>% 
      summarize(ct=n())
    last_weeks_count <- sum(last_week_count_df$ct)
    percent_change <- round(((curr_wk_count - last_weeks_count)/(last_weeks_count)) * 100,digits=3)
    valueBox(
      VB_style(paste0(ifelse(percent_change>0,'+','-'),abs(percent_change),'%'),"font-size:60%;"),
      "Performance against last week",
      icon=icon('chart-line'),
      color=ifelse(percent_change>0,'green','red'),
      width=6
    )
  })
  
  ###Most crowded
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
  
  
  ###Least crowded
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
  
  ###Most crowded time
  output$most_crowded_time <- renderValueBox({
    yest_date <- as.character(Sys.Date()-1)
    time_df <- data %>% 
      mutate(hour=format(strptime(time,"%H: %M: %S"),"%H")) %>% 
      filter(date==yest_date) %>% 
      group_by(hour) %>% 
      summarize(ct=n()) %>% 
      slice(which.max(ct))
    most_crowded_time <- format(strptime(time_df$hour,"%H"),"%H:%M")
    
    valueBox(
      VB_style(paste0(most_crowded_time, " till ",paste0(time_df$hour,":59")), "font-size:60%;"),
      "Most Popular Hour",
      icon=icon('clock'),
      color="maroon"
    )
  })
  
  ###Least crowded time
  output$least_crowded_time <- renderValueBox({
    yest_date <- as.character(Sys.Date()-1)
    time_df <- data %>% 
      mutate(hour=format(strptime(time,"%H: %M: %S"), "%H")) %>% 
      filter(date==yest_date) %>% 
      group_by(hour) %>% 
      summarize(ct=n()) %>% 
      slice(which.min(ct))
    least_crowded_time <- format(strptime(time_df$hour,"%H"),"%H:%M")
    
    valueBox(
       VB_style(paste0(least_crowded_time, " till ",paste0(time_df$hour,":59")),"font-size:60%;"),
       "Least Popular Hour",
       icon=icon('clock'),
       color="olive"
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
