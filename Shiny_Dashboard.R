library(shiny)
library(shinydashboard)

# I lost all my work from yesterday so this file is almost
# identical to the one on the master branch :(

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
                  " This is the default starting page.
                  We will put some visualizations here.")),
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
