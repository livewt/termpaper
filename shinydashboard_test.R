library(shiny)
library(shinydashboard)
library(plotly)
library(data.table)

# Here, we choose the Telenor file as we run the app

source("Project.R")

# Order given dataset by StandardAccountID and sum ClosingDebitBalance
SumBySAID <- aggregate(main_df$ClosingDebitBalance,
                       by=list(StandardAccountID=main_df$StandardAccountID),
                       FUN=sum)
SumBySAID$StandardAccountID <- as.numeric(SumBySAID$StandardAccountID) # to compare later,
                                                                       # easier fix more than welcome

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

art.data <- data.frame(Art, Resultatregnskap, check.names = 'false')

# EIENDELER

Eiendeler <- c('Immaterielle eiendeler o.l','Tomter, bygninger og annen fast eiendom',
               'Transportmidler, inventar og maskiner o.l.', 'Finansielle anleggsmidler',
               'Varelager og forskudd til leverandører', 'Kortsiktige fordringer',
               'Merverdiavgift, opptjente offentlige tilskudd o.l.', 'Forskuddsbetalt kostnad, påløpt inntekt o.l.',
               'Kortsiktige finansinvesteringer', 'Bankinnskudd, kontanter og lignende')

StandardAccountID <- c(10,11,12,13,14,15,16,17,18,19)

`Eiendeler tall` <- 0 # placeholder

BalanseEiendeler <- data.frame(Eiendeler, StandardAccountID, `Eiendeler tall`, check.names = 'false')

# Get sum from SumBySAID and insert into BalanseEiendeler
setDT(BalanseEiendeler)[SumBySAID, `Eiendeler tall` := x, on = .(StandardAccountID)]

BalanseEiendeler$StandardAccountID <- NULL

# EGENKAPITAL OG GJELD

`Egenkapital og Gjeld` <- c('Egenkapital AS/ASA', 'Avsetning for forpliktelser',
                            'Annen langsiktig gjeld', 'Kortsiktige konvertible lån, obligasjonslån og gjeld til kredittinstitusjoner',
                            'Leverandørgjeld', 'Betalbar skatt', 'Skattetrekk og andre trekk',
                            'Skyldige offentlige avgifter', 'Utbytte',
                            'Annen kortsiktig gjeld')

StandardAccountID <- c(20,21,22,23,24,25,26,27,28,29)

`EKGJ tall` <- 0 # placeholder

BalanseEKGJ <- data.frame(`Egenkapital og Gjeld`, StandardAccountID, `EKGJ tall`, check.names = 'false')

# Get sum from SumBySAID and insert into BalanseEKGJ
setDT(BalanseEKGJ)[SumBySAID, `EKGJ tall` := x, on = .(StandardAccountID)]

BalanseEKGJ$StandardAccountID <- NULL



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
                 icon = icon("credit-card")),
        menuItem("About",
                 tabName = "about",
                 icon = icon("info-circle")))),

    
    dashboardBody(
      fluidRow(
        tags$head(
          tags$style(HTML("hr {border-top: 1px solid #000000;}")), # make hr more visible
          tags$style(HTML("
            .content{
               width: 90%;
            }"))
        ),
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
                  h2(
                    "Balance statement"),
                    tabPanel("BalanseEiendeler", DT::dataTableOutput("BalanseEiendeler")),
                    hr(),
                    tabPanel("BalanseEKGJ", DT::dataTableOutput("BalanseEKGJ"))),
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
    output$BalanseEiendeler <- DT::renderDataTable({
      DT::datatable(BalanseEiendeler)
    })
    output$BalanseEKGJ <- DT::renderDataTable({
      DT::datatable(BalanseEKGJ)
    })
    output$art.data <- DT::renderDataTable({
      DT::datatable(art.data)
    })
    
  }

# Run app
shinyApp(ui,server)
