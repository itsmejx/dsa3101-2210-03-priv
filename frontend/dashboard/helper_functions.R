customSentence <- function(numItems, type){
  paste("Feedback & suggestions")
}

dropdownMenuCustom <- function(...,type=c("messages","notifications","tasks"),
                               badgeStatus="primary",icon=NULL,.list=NULL, customSentence = customSentence)
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...),.list)
  lapply(items,shinydashboard:::tagAssert,type="li")
  dropdownClass <- paste0("dropdown",type,"-menu")
  if(is.null(icon)) {
    icon <- switch(type,messages=shiny::icon("envelope"),
                   notifications=shiny::icon("warning"),tasks=shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)){
    badge <- NULL
  }
  else {
    badge <- tags$span(class = paste0("label label-", badgeStatus),
                       numItems)
  }
  tags$li(
    class = "dropdown",
    tags$ul(
      class="dropdown-menu",
      tags$li(
        class="header",
        customSentence(numItems,type)
      ),
      tags$li(
        tags$ul(class = "menu",items)
      )
    )
  )
}

VB_style <- function(msg='Hello', style="font-size:100%;"){
  tags$p(msg,style=style)
}

