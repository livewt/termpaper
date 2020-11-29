library(shiny)
library(shinydashboard)
library(plotly)

# Here, we choose the Telenor file as we run the app

source("Project.R")

# create dataset
Art <- c('Salgsinntekt','Annen driftsinntekt',
            'Endring i beholdning av varer under tilvirkning og ferdig tilvirkede varer',
            'Endring i beholdning av egentilvirkede anleggsmidler',
            'Varekostnad', 'Lønnskostnad',
            'Avskrivning på varige driftsmidler og immaterielle eiendeler',
            'Nedksrivning av varige driftsmidler og immaterielle eiendeler',
            'Annen driftskostnad', 'Driftsresultat',
            'Inntekt på investering i datterselskap',
            'Inntekt på andre investeringer', 'Renteinntekt fra foretak i samme konsern',
            'Annen finansinntekt', 'Verdiendring av finansielle instrumenter',
            'Nedskrining av finansielle eiendeler', 'Rentekostnad til foretak i samme konsern',
            'Annen finanskostnad', 'Ordinært resultat før skattekostnad',
            'Skattekostnad på ordinært resultat', 'Ordinært resultat',
            'Ekstraordinære poster', 'Skattekostnad på ekstraordinære poster',
            'Årsresultat')
Resultatregnskap <- 0

art.data <- data.frame(Art, Resultatregnskap)

Eiendeler <- c('Anleggsmidler','Omløpsmidler')
Eiendeler.tall <- 0
`Egenkapital og Gjeld` <- c('Egenkapital', 'Gjeld')
EKGJ.tall <- 0

Balanse <- data.frame(Eiendeler, Eiendeler.tall, `Egenkapital og Gjeld`, EKGJ.tall)

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
        menuItem("Income Statement",
                 tabName = "incomestatement",
                 icon = icon("cash-register")),
        menuItem("Balance Statement",
                 tabName = "balancestatement",
                 icon = icon("cash-register")),
        menuItem("About",
                 tabName = "about",
                 icon = icon("info-circle")))),

    
    dashboardBody(
      fluidRow(
        tabItems(
          tabItem(tabName = "dashboard",
                  tabBox(
                    side = "left",
                    width = 6,
                    tabPanel(h6("Return on Assets"), roa),
                    tabPanel(h6("Return on Equity*"), roe,
                             h6("*Return on equity is calculated pre-tax"))),
                  valueBox(
                    round(Capital_turnover, digits = 2),
                    "Capital Turnover Rate",
                    icon = icon("hand-holding-usd"),
                    width = 3,
                    color = "purple"),
                  valueBox(
                    round(Inventory_turnover, digits = 2),
                    "Inventory Turnover Rate",
                    icon = icon("warehouse"),
                    width = 3,
                    color = "purple")),
        
          
          tabItem(tabName = "balancestatement",
                  h4(
                    "Balance statement"),
                    tabPanel("Balanse", DT::dataTableOutput("Balanse"))),
          tabItem(tabName = "incomestatement",
                  h4(
                    "Income statement"),
                  tabPanel("art.data", DT::dataTableOutput("art.data"))),
          tabItem(tabName = "about",
                  h4(
                    " Here, we can write some information
                   about the financial ratios or about how
                   the dashboard was made."),
                  h5(
                    "We can also write our names.")))
        
    )
  )
)

# Create server
server = 
  function(input,output){
    
    output$Balanse <- DT::renderDataTable({
      DT::datatable(Balanse)
    })
    output$art.data <- DT::renderDataTable({
      DT::datatable(art.data)
    })
    
  }

# Run app
shinyApp(ui,server)
