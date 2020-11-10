library(shiny)
library(shinydashboard)

FAR <- dashboardPage(
  dashboardHeader(title = "Financial Accounting Report"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItem(tabName = "dashboard",
            fluidRow())
  )
)

server <- function(input, output) {}

shinyApp(FAR, server)



#Split up

header <- dashboardHeader(title = "Financial Accounting Report",
                          titleWidth = 290)

sidebar <- dashboardSidebar(width = 290,
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))))

body <- dashboardBody(
  tabItem(tabName = "dashboard",
          fluidRow()))

FAR2 <- dashboardPage(header, sidebar, body)

server <- function(input, output) {}

shinyApp(FAR2, server)


#Source to ShinyDashboard:
#https://rstudio.github.io/shinydashboard/get_started.html