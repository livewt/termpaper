library(shiny)
library(shinydashboard)
library(plotly)
library(data.table)

#Open with encoding UTF-8 to get ÃÃÃ

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
`Resultatregnskap etter art` <- c('Salgsinntekt', 'Varekostnad', 'LÃ¸nnskostnad',
                                  'Avskrivning', 'Nedskrivning',
                                  'Annen driftskostnad', 'Finansinntekt',
                                  'Finanskostnad',
                                  'Skattekostnad pÃ¥ ordinÃ¦rt resultat',
                                  'EkstraordinÃ¦r inntekt', 
                                  'EkstraordinÃ¦r kostnad', 
                                  'SKattekostnad pÃ¥ ekstraordinÃ¦rt resultat',
                                  'Ãrsresultat', 'OverfÃ¸ringer/disponeringer')

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
               'Varelager og forskudd til leverandÃ¸rer', 
               'Kortsiktige fordringer',
               'Merverdiavgift, opptjente offentlige tilskudd o.l.', 
               'Forskuddsbetalt kostnad, pÃ¥lÃ¸pt inntekt o.l.',
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
                            'Kortsiktige konvertible lÃ¥n, obligasjonslÃ¥n og gjeld til kredittinstitusjoner',
                            'LeverandÃ¸rgjeld', 'Betalbar skatt', 
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
                  h1(div("Profitability Ratios",
                         style = "color:darkred",
                         align = "center")),
                  box(
                    title = "Return on Assets",
                    status = "danger",
                    solidHeader = TRUE,
                    height = 295,
                    roa),
                  box(
                    title = "Operating Margin",
                    status = "danger",
                    solidHeader = TRUE,
                    height = 295,
                    h5("I'm working on this")),
                  h1(div("Efficiency Ratios",
                  style = "color:orange",
                  align = "center")),
                  box(
                   title = "Wages to Salary Ratio",
                   status = "warning",
                   solidHeader = TRUE,
                   height = 417,
                   h5(div(
                     "Wages to salary ratio is",
                     higher_lower(Open_wages_sale_inc,
                                  Close_wages_sale_inc),
                     "it was at the beginning of the year",
                     align = "center")),
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
                    title = "Return on Equity",
                    status = "warning",
                    solidHeader = TRUE,
                    height = 295,
                    roe,
                    h6(
                      "Return on equity is calculated pre-tax")),
               h1(div("Liquidity Ratios",
                      style = "color:green",
                      align = "center")),
               box(
                 title = "Current Ratio",
                 status = "success",
                 solidHeader = TRUE,
                 h5(div(
                   "Current ratio is",
                   higher_lower(Open_Current_ratio,
                                Close_Current_ratio),
                                " it was at the beginning of the year"),
                   align = "center"),
                 current_chart,
                 h6(
                   "A good current ratio is considered to be between 1 and 2.")),
               box(
                 title = "Quick Ratio",
                 status = "success",
                 solidHeader = TRUE,
                 h5(
                   div(
                   "Quick ratio is",
                   higher_lower(Open_Acid_test,
                                Close_Acid_test),
                   "it was at the beginning of the year"),
                   align = "center"),
                 quick_chart,
                 h6(
                   "A good quick ratio is considered to be above 1."))),
          tabItem(tabName = "transactions",
                  box(
                    solidHeader = FALSE,
                    width = 12,
                    girafeOutput("trans_plot"),
                    #plotOutput("trans_plot"),
                    dateRangeInput(inputId = "date",
                                   label = "Select date",
                                   start = min(plot_info$TransactionDate),
                                   end = max(plot_info$TransactionDate),
                                   min = min(plot_info$TransactionDate),
                                   max = max(plot_info$TransactionDate))
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
    #output$trans_plot <- renderGirafe({
     # girafe(ggobj = transaction_plot)
    #})
    trans_subset <- reactive({
      a <- subset(plot_info, plot_info$TransactionDate>= input$date[1] & plot_info$TransactionDate <= input$date[2])
      a
    })
    #tooltip_ <- c(paste0("Description: ", trans_subset$Description,
    #                     "\n Transaction ID: ", trans_subset$TransactionID,
    #                     "\n Amount: ", as.integer(trans_subset$`trans_sum$Amounts`), " NOK")) #int to remove uneccesary deciamls in plot
    #output$datestart <- renderText({
    #  as.character(input$date[1])
    #})
    #output$dateend <- renderText({
    #  as.character(input$date[2])
    #})
    #tooltip_ <- observe({
     # c(paste0("Description: ", trans_subset$Description,
      #                   "\n Transaction ID: ", trans_subset$TransactionID,
      #                   "\n Amount: ", as.integer(trans_subset$`trans_sum$Amounts`), " NOK")) #int to remove uneccesary deciamls in plot
#})
   # tooltip_ <- c(paste0("Description: ", a$Description,
  #                                         "\n Transaction ID: ", a$TransactionID,
  #                                         "\n Amount: ", as.integer(a$`trans_sum$Amounts`), " NOK")) #int to remove uneccesary deciamls in plot
                         
    output$trans_plot <- renderGirafe({
      plottt <- ggplot(data = trans_subset()) +
        geom_point_interactive(aes(x = 1:length(`trans_sum$Amounts`), y = `trans_sum$Amounts`,
                                   tooltip = c(paste0("Description: ", Description,
                                   "\n Transaction ID: ", TransactionID,
                                   "\n Amount: ", as.integer(`trans_sum$Amounts`), " NOK")),
                                   data_id = TransactionID))+
        ylab("Amount")+
        xlab("Transcation number")+
        ggtitle("Hover over points to view description of the transcation")+
        scale_x_continuous(labels = scales::comma)+
        scale_y_continuous(labels = scales::comma)
        
        girafe(ggobj = plottt)
    })
    #output$test <- renderTable(
     # test2 <- subset(trans_plot, trans_plot$TransactionDate >= input$date[1] & trans_plot$TransactionDate <= input$date[2])
    #)
  }

# Run app
shinyApp(ui,server)
