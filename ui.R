# Loading required libraries
library(shiny)
library(lubridate)

  # Defining the value for year drop down selection box under "Statistics" tab
  yearList <- list(year(Sys.Date()))
  
  tryCatch ({
    dat <- read.csv("data.csv", header = TRUE)
    yearList <- as.list(unique(dat$YEAR))
  }, warning = function(war) {
  },  error = function(err) {
  }, finally = {
  })  

shinyUI(fluidPage(
    # Including required css files
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    # Defining custom HTML page to be used and the needed HTML components
    htmlTemplate(
      "./www/index.html",
      button_1 = actionButton("but1", "Vote", class="button button1"),
      button_2 = actionButton("but2", "Vote", class="button button1"),
      button_3 = actionButton("but3", "Vote", class="button button1"),
      button_4 = actionButton("but4", "Vote", class="button button1"),
      button_5 = actionButton("but5", "Vote", class="button button1"),
      button_6 = actionButton("but6", "Vote", class="button button1"),
      button_7 = actionButton("but7", "Vote", class="button button1"),
      button_8 = actionButton("but8", "Vote", class="button button1"),
      button_9 = actionButton("but9", "Vote", class="button button1"),
      button_10 = actionButton("but10", "Vote", class="button button1"),
      button_11 = actionButton("but11", "Vote", class="button button1"),
      button_12 = actionButton("but12", "Vote", class="button button1"),
      label_1 = tableOutput("label1"),
      label_2 = tableOutput("label2"),
      label_3 = tableOutput("label3"),
      label_4 = tableOutput("label4"),
      label_5 = tableOutput("label5"),
      label_6 = tableOutput("label6"),
      label_7 = tableOutput("label7"),
      label_8 = tableOutput("label8"),
      label_9 = tableOutput("label9"),
      label_10 = tableOutput("label10"),
      label_11 = tableOutput("label11"),
      label_12 = tableOutput("label12"),
      month_dropdown = selectInput("monthDropdown", NULL, choices = list("Jan - Jun" = 1, "Jul - Dec" = 2), selected = 1),
      year_dropdown = selectInput("yearDropdown", NULL, choices = yearList, selected = yearList[[1]]),
      button_13 = actionButton("but13", "Submit", class="button button1"),
      label_status = tableOutput("labelStatus"),
      plotGraph = plotOutput("distPlot")
    )
  )
)