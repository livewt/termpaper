library(shiny)
library(shinydashboard)
library(plotly)
library(data.table)
library(foreach)

#Open with encoding UTF-8 to get ÆØÅ

# Here, we choose the Telenor file as we run the app

source("Project.R")

# Order given dataset by StandardAccountID and sum ClosingDebitBalance
SumBySAID <- aggregate(main_df$ClosingDebitBalance,
                       by=list(StandardAccountID=main_df$StandardAccountID),
                       FUN=sum)
SumBySAID$StandardAccountID <- as.numeric(SumBySAID$StandardAccountID) # to compare later,
                                                                       # easier fix more than welcome

SumByAID <- aggregate(main_df$ClosingDebitBalance,
                      by=list(AccountID=main_df$AccountID),
                      FUN=sum)
SumByAID$AccountID <- as.numeric(SumByAID$AccountID)

# --------------------
# Resultatregnskap etter art
# --------------------

`Resultatregnskap etter art` <- c('Salgsinntekt', 'Varekostnad', 'Lønnskostnad',
                                  'Avskrivning', 'Nedskrivning',
                                  'Annen driftskostnad', 'Finansinntekt',
                                  'Finanskostnad',
                                  'Skattekostnad på ordinært resultat',
                                  'Ekstraordinær inntekt', 
                                  'Ekstraordinær kostnad', 
                                  'SKattekostnad på ekstraordinært resultat',
                                  'Årsresultat', 'Overføringer/disponeringer')

AccountIDRange <- c(Salgsinntekt = 3000:3970, Varekostnad = 4000:4990, Lønnskostnad = 5000:5930,
                    Avskrivning = 6000:6020, Nedskrivning = 6050, `Annen driftskostnad` = 6100:7910,
                    Finansinntekt = 8000:8080, Finanskostnad = 8100:8170, 
                    `Skattekostnad på ordinært resultat` = 8300:8320,
                    `Ekstraordinær inntekt` = 8400, `Ekstraordinær kostnad` = 8500,
                    `SKattekostnad på ekstraordinært resultat` = 8600:8620,
                    Årsresultat = 8800, `Overføringer/disponeringer` = 8900:8990)

match(names(AccountIDRange), `Resultatregnskap etter art`)

`REA Tall` <- 0 # placeholder

# Get sum from SumByAID and insert into REA
setDT(AccountIDRange)[SumByAID, `REA Tall` := x, on = .(AccountIDRange)]

BalanseREA <- data.frame(`Resultatregnskap etter art`, AccountIDRange, 
                       `REA Tall`, check.names = 'false')


# --------------------
# EIENDELER
# --------------------

Eiendeler <- c('Immaterielle eiendeler o.l',
               'Tomter, bygninger og annen fast eiendom',
               'Transportmidler, inventar og maskiner o.l.', 
               'Finansielle anleggsmidler',
               'Varelager og forskudd til leverandører', 
               'Kortsiktige fordringer',
               'Merverdiavgift, opptjente offentlige tilskudd o.l.', 
               'Forskuddsbetalt kostnad, påløpt inntekt o.l.',
               'Kortsiktige finansinvesteringer', 
               'Bankinnskudd, kontanter og lignende')

StandardAccountID <- c(10:19)

`Eiendeler tall` <- 0 # placeholder

BalanseEiendeler <- data.frame(Eiendeler, StandardAccountID, 
                               `Eiendeler tall`, check.names = 'false')

# Get sum from SumBySAID and insert into BalanseEiendeler
setDT(BalanseEiendeler)[SumBySAID, `Eiendeler tall` := x, on = .(StandardAccountID)]

# Remove SAID from BalanseEiendeler
BalanseEiendeler$StandardAccountID <- NULL


# --------------------
# EGENKAPITAL OG GJELD
# --------------------

`Egenkapital og Gjeld` <- c('Egenkapital AS/ASA', 'Avsetning for forpliktelser',
                            'Annen langsiktig gjeld', 
                            'Kortsiktige konvertible lån, obligasjonslån og gjeld til kredittinstitusjoner',
                            'Leverandørgjeld', 'Betalbar skatt', 
                            'Skattetrekk og andre trekk',
                            'Skyldige offentlige avgifter', 'Utbytte',
                            'Annen kortsiktig gjeld')

StandardAccountID <- c(20:29)

`EKGJ tall` <- 0 # placeholder

BalanseEKGJ <- data.frame(`Egenkapital og Gjeld`, StandardAccountID, 
                          `EKGJ tall`, check.names = 'false')

# Get sum from SumBySAID and insert into BalanseEKGJ
setDT(BalanseEKGJ)[SumBySAID, `EKGJ tall` := x, on = .(StandardAccountID)]

# Remove SAID from BalanseEKGJ
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
          
          tabItem(tabName = "liquidity",
                  box(
                    title = 
                      "Return on Equity",
                    status =
                      "warning",
                    solidHeader = 
                      TRUE,
                    height = 
                      275,
                    roe),
                  valueBox(
                    round(
                      Capital_turnover,
                      digits = 3),
                    "Capital Turnover",
                    icon = icon("hand-holding-usd"),
                    width = 3,
                    color = "orange"),
                  valueBox(
                    round(
                      Inventory_turnover,
                      digits = 3),
                    "Inventory Turnover",
                    icon = icon("warehouse"),
                    width = 3,
                    color = "orange"),
                  box(
                    title = "Wages to Salary Ratio",
                    status = "warning",
                    solidHeader = TRUE,
                    height = 275,
                    renderPlot(
                      w_to_s_chart))),
          tabItem(tabName = "transactions",
                  box(
                    title = "Transaction Plot",
                    status = "primary",
                    solidHeader = TRUE,
                    height = 275
#                   ,name of the transaction plot here!
                  )),
        
          
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
      DT::datatable(BalanseREA)
    })
    
  }

# Run app
shinyApp(ui,server)
