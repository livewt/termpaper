library(shiny)
library(shinydashboard)
library(plotly)
library(data.table)

#Open with encoding UTF-8 to get ÆØÅ

# Here, we choose the Telenor file as we run the app

source("Project.R")

# Order given dataset by StandardAccountID and sum ClosingDebitBalance
SumBySAID <- aggregate(main_df$ClosingDebitBalance,
                       by=list(StandardAccountID=main_df$StandardAccountID),
                       FUN=sum)
SumBySAID$StandardAccountID <- as.numeric(SumBySAID$StandardAccountID) 


SumByAID <- aggregate(main_df$ClosingDebitBalance,
                      by=list(AccountID=main_df$AccountID),
                      FUN=sum)
SumByAID$AccountID <- as.numeric(SumByAID$AccountID)


# create dataset
`Resultatregnskap etter art` <- c('Salgsinntekt', 'Varekostnad', 'Lønnskostnad',
                                  'Avskrivning', 'Nedskrivning',
                                  'Annen driftskostnad', 'Finansinntekt',
                                  'Finanskostnad',
                                  'Skattekostnad på ordinært resultat',
                                  'Ekstraordinær inntekt', 
                                  'Ekstraordinær kostnad', 
                                  'SKattekostnad på ekstraordinært resultat',
                                  'Årsresultat', 'Overføringer/disponeringer')

AccountID <- c(3000:3970, 4000:4990, 5000:5930, 6000:6020, 6050, 6100:7910,
               8000:8080, 8100:8170, 8300:8320, 8400, 8500, 8600:8620,
               8800, 8900:8990)

Resultatregnskap <- 0

#art.data <- data.frame(`Resultatregnskap etter art`, AccountID, 
#                       Resultatregnskap, check.names = 'false')

# EIENDELER

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

StandardAccountID <- c(10,11,12,13,14,15,16,17,18,19)

`Eiendeler tall` <- 0 # placeholder

BalanseEiendeler <- data.frame(Eiendeler, StandardAccountID, 
                               `Eiendeler tall`, check.names = 'false')

# Get sum from SumBySAID and insert into BalanseEiendeler
setDT(BalanseEiendeler)[SumBySAID, `Eiendeler tall` := x, on = .(StandardAccountID)]

BalanseEiendeler$StandardAccountID <- NULL

# EGENKAPITAL OG GJELD

`Egenkapital og Gjeld` <- c('Egenkapital AS/ASA', 'Avsetning for forpliktelser',
                            'Annen langsiktig gjeld', 
                            'Kortsiktige konvertible lån, obligasjonslån og gjeld til kredittinstitusjoner',
                            'Leverandørgjeld', 'Betalbar skatt', 
                            'Skattetrekk og andre trekk',
                            'Skyldige offentlige avgifter', 'Utbytte',
                            'Annen kortsiktig gjeld')

StandardAccountID <- c(20,21,22,23,24,25,26,27,28,29)

`EKGJ tall` <- 0 # placeholder

BalanseEKGJ <- data.frame(`Egenkapital og Gjeld`, StandardAccountID, 
                          `EKGJ tall`, check.names = 'false')

# Get sum from SumBySAID and insert into BalanseEKGJ
setDT(BalanseEKGJ)[SumBySAID, `EKGJ tall` := x, on = .(StandardAccountID)]

BalanseEKGJ$StandardAccountID <- NULL


# Create ui
ui =
  dashboardPage(
    skin = "black",
    dashboardHeader(
      title = "Financial Report",
      titleWidth = 250),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        menuItem("Dashboard",
                 tabName = "dashboard",
                 icon = icon("dashboard")),
        menuItem("Transactions",
                 tabName = "transactions",
                 icon = icon("people-arrows")),
        menuItem("Income Statement",
                 tabName = "incomestatement",
                 icon = icon("cash-register")),
        menuItem("Balance Statement",
                 tabName = "balancestatement",
                 icon = icon("balance-scale")),
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
                  h1(div("Efficiency Ratios",
                  style = "color:orange",
                  align = "center")),
                  box(
                   title = "Wages to Salary Ratio",
                   status = "warning",
                   solidHeader = TRUE,
                   height = 413,
                   w_to_s_chart),
                valueBox(
                 round(Capital_turnover,
                       digits = 3),
                 "Capital Turnover",
                 icon = icon("hand-holding-usd"),
                 width = 3,
                 color = "orange"),
               valueBox(
                 round(Inventory_turnover,
                       digits = 3),
                 "Inventory Turnover",
                 icon = icon("warehouse"),
                 width = 3,
                 color = "orange"),
                  box(
                    title = "Return on Equity*",
                    status = "warning",
                    solidHeader = TRUE,
                    height = 290,
                    roe,
                    h6(
                      "*Return on equity is calculated pre-tax"))),
          tabItem(tabName = "transactions",
                  box(
                    title = "Transactions",
                    status = "primary",
                    solidHeader = TRUE,
#                    ,*plot name here*
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
