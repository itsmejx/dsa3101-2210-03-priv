FROM rocker/shiny-verse
RUN sudo apt update && sudo apt install -y libxt-dev
RUN sudo apt -y upgrade
RUN sudo apt install -y vim nano
RUN sudo apt install -y libxml2 libxml2-dev libudunits2-dev
RUN install2.r tidyverse httr jsonlite chron RColorBrewer plotly flexdashboard DT shinydashboard ggplot2 lubridate stringr withr treemap shinyBS shinyjs WDI geosphere magrittr shinycssloaders timevis
COPY Template/index.html /srv/shiny-server/index.html
COPY Template/logo.png /srv/shiny-server/logo.png
COPY dashboard /srv/shiny-server/dashboard
COPY DB /srv/shiny-server/DB
