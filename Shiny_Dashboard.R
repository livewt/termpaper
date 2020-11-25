library(shiny)
library(shinydashboard)
library(plotly)

source("Project.R")

# Create ui
ui =
  dashboardPage(
    dashboardHeader(
      title = "Financial Report",
      titleWidth = 250),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        menuItem("Dashboard",
                 tabName = "dashboard",
                 icon = icon("dashboard")),
        menuItem("About",
                 tabName = "about",
                 icon = icon("info-circle")))),
    dashboardBody(
      fluidRow(
        
      tabItems(
        tabItem(tabName = "dashboard",
                h4(
                  " I just put this visual here to test
                  if the source function worked."),
                roe,
                ),
        tabItem(tabName = "about",
                h4(
                   " Here, we can write some information
                   about the financial ratios or about how
                   the dashboard was made."),
                h5(
                  "We can also write our names."))))
    )
)

# Create server
server = 
  function(input,output){}

# Run app
shinyApp(ui,server)
