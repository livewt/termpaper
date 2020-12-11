library(shiny)
library(shinydashboard)
library(plotly)
library(data.table)
library(DT)
library(shinyWidgets)
#Open with encoding UTF-8 to get norwegian letters

# Here, we choose the Telenor file as we run the app


source("Project.R", encoding = "UTF-8")


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
                         style = "color:dodgerblue",
                         align = "center")),
                  box(
                    title = "Bruttofortjeneste",
                    status = "info",
                    solidHeader = TRUE,
                    height = 417,
                    h5(div(
                      "Gross profit is", higher_lower(Open_GrossProfit,
                                                      Close_GrossProfit),
                      "it was last year"),
                      align = "center"),
                    profit_chart),
                  valueBox(
                    round(Close_profit_margin1*100,
                          digits = 2),
                    "Resultatsgrad i %",
                    icon = icon("piggy-bank"),
                    width = 3,
                    color = "aqua"),
                  valueBox(
                    round(Close_profit_margin2*100,
                          digits = 2),
                    "Resultatsmargin i %",
                    icon = icon("money-check-alt"),
                    width = 3,
                    color = "aqua"),
                  box(
                    title = "Totalkapitalens rentabilitet",
                    status = "info",
                    solidHeader = TRUE,
                    height = 295,
                    roa,
                    h6(div(
                      "A good return on assets is considered to be above 5%"),
                      align = "center")),
                  box(
                    title = "Bruttofortjeneste i %",
                    status = "info",
                    solidHeader = TRUE,
                    height = 380,
                    h5(div("Gross profit margin is",
                       higher_lower(Open_GrossProfit_percent,
                                    Close_GrossProfit_percent),
                       "it was last year"),
                       align = "center"),
                    gross_percent_chart,
                    h6(div(
                      "High: 20%,"),
                      align = "center"),
                    h6(div(
                      "Average: 10%"),
                      align = "center"),
                    h6(div(
                      "Low: 5%"),
                      align = "center")),
                  box(
                    title = "Driftsmargin",
                    status = "info",
                    solidHeader = TRUE,
                    height = 380,
                    h5(div("Operating margin is",
                           higher_lower(Open_Operating_margin,
                                        Close_Operating_margin),
                           "it was last year"),
                       align = "center"),
                    operating_chart,
                    h6(div(
                      "High: 20%,"),
                      align = "center"),
                    h6(div(
                      "Average: 10%"),
                      align = "center"),
                    h6(div(
                      "Low: 5%"),
                      align = "center")),
                  
                  h1(div("Leverage Ratios",
                         style = "color:darkred",
                         align = "center")),
                  valueBox(
                    round(Close_debt_ratio,
                          digits = 3),
                    "Gjeldsgrad",
                    icon = icon("credit-card"),
                    width = 4,
                    color = "red"),
                  valueBox(
                    round(Close_equity_ratio,
                          digits = 3),
                    "Egenkapitalandel",
                    icon = icon("coins"),
                    width = 4,
                    color = "red"),
                  valueBox(
                    round(Close_interest_ratio,
                          digits = 3),
                    "Rentedekningsgrad",
                    icon = icon("chart-line"),
                    width = 4,
                    color = "red"),
                  h1(div("Efficiency Ratios",
                         style = "color:darkorange",
                         align = "center")),
                  box(
                   title = "Lønnskostnader i % af salgsinntekt",
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
                  box(
                    title = "Egenkapitalens rentabilitet",
                    status = "warning",
                    solidHeader = TRUE,
                    height = 295,
                    roe,
                    h6(div(
                      "Return on equity is calculated pre-tax"),
                      align = "center")),
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
                 h6(div(
                   "A good current ratio is considered to be between 1 and 2"),
                   align = "center")),
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
                 h6(div(
                   "A good quick ratio is considered to be above 1"),
                   align = "center"))),
          tabItem(tabName = "transactions",
                  box(
                    solidHeader = FALSE,
                    width = 12,
                    girafeOutput("trans_plot"),
                    dateRangeInput(inputId = "date",
                                   label = "Select date",
                                   start = min(plot_info$TransactionDate),
                                   end = max(plot_info$TransactionDate),
                                   min = min(plot_info$TransactionDate),
                                   max = max(plot_info$TransactionDate)),
                    numericRangeInput(inputId = "monetary",
                                      label ="Choose transcation amount range",
                                      value = c(min(plot_info$`trans_sum$Amounts`),
                                      max(plot_info$`trans_sum$Amounts`)))
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
      DT::datatable(BalanseEiendeler) %>%
                      formatRound(columns = "Eiendeler tall",
                                  interval = 3,
                                  mark = ",",
                                  digits = 0)
      
    })
    output$BalanseEKGJ <- DT::renderDataTable({
      DT::datatable(BalanseEKGJ) %>%
        formatRound(columns = "EKGJ tall",
          interval = 3,
          mark = ",",
          digits = 0
        )
    })
    output$art.data <- DT::renderDataTable({
      DT::datatable(Resultatregnskap)%>%
        formatRound(columns = "Tall",
                    interval = 3,
                    mark = ",",
                    digits = 0
        )
    })
    #output$trans_plot <- renderGirafe({
     # girafe(ggobj = transaction_plot)
    #})
    trans_subset <- reactive({
      a <- subset(plot_info, plot_info$TransactionDate>= input$date[1] & plot_info$TransactionDate <= input$date[2]
                  & plot_info$`trans_sum$Amounts` >= input$monetary[1] & plot_info$`trans_sum$Amounts` <= input$monetary[2])
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
      if (nrow(trans_subset()) >0){
      plottt <- ggplot(data = trans_subset()) +
        geom_point_interactive(aes(x = 1:length(`trans_sum$Amounts`), y = `trans_sum$Amounts`,
                                   tooltip = c(paste0("Description: ", Description,
                                   "\n Transaction ID: ", TransactionID,
                                   "\n Amount: ", as.integer(`trans_sum$Amounts`), " NOK")),
                                   data_id = TransactionID))+
        ylab("Amount")+
        xlab(" ")+
        ggtitle("Hover over observations to view description of the transaction")+
        (if (nrow(trans_subset())==1){
          labs(subtitle = paste0("Currently viewing ",nrow(trans_subset())," transaction"))
        }
        else {
            labs(subtitle = paste0("Currently viewing ",nrow(trans_subset())," transactions"))
          })+
       # scale_x_continuous(labels = scales::comma)+
        scale_y_continuous(labels = scales::comma)+
        theme(axis.text.x=element_blank())
        
        girafe(ggobj = plottt)
      }else{
        girafe(ggobj = error_plot)
    }
    })
    
     
    
    #output$test <- renderTable(
     # test2 <- subset(trans_plot, trans_plot$TransactionDate >= input$date[1] & trans_plot$TransactionDate <= input$date[2])
    #)
  }

# Run app
shinyApp(ui,server)
