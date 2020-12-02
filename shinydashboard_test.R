
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

#AccountIDRange <- c(Salgsinntekt = 3000:3970, Varekostnad = 4000:4990, Lønnskostnad = 5000:5930,
#                    Avskrivning = 6000:6020, Nedskrivning = 6050, `Annen driftskostnad` = 6100:7910,
#                    Finansinntekt = 8000:8080, Finanskostnad = 8100:8170, 
#                    `Skattekostnad på ordinært resultat` = 8300:8320,
#                    `Ekstraordinær inntekt` = 8400, `Ekstraordinær kostnad` = 8500,
#                    `SKattekostnad på ekstraordinært resultat` = 8600:8620,
#                    Årsresultat = 8800, `Overføringer/disponeringer` = 8900:8990)

#match(names(AccountIDRange), `Resultatregnskap etter art`)

#`REA Tall` <- 0 # placeholder

# Get sum from SumByAID and insert into REA
#setDT(AccountIDRange)[SumByAID, `REA Tall` := x, on = .(AccountIDRange)]

#BalanseREA <- data.frame(`Resultatregnskap etter art`, AccountIDRange, 
#                       `REA Tall`, check.names = 'false')


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
        tabItems(
          tabItem(tabName = "dashboard",
                  #I commented this out to maybe use for later
#                  tabBox(
#                    side = "left",
#                    width = 6,
#                    tabPanel("Return on Assets", roa),
#                    tabPanel("Return on Equity*", roe,
#                             h5("*Return on equity is calculated pre-tax"))),
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

