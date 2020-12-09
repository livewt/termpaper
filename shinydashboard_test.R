library(shiny)
library(shinydashboard)
library(plotly)
library(data.table)

#Open with encoding UTF-8 to get norwegian letters

# Here, we choose the Telenor file as we run the app


source("Project.R")


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
                    title = "Return on Assets",
                    status = "info",
                    solidHeader = TRUE,
                    height = 295,
                    roa,
                    h6(div(
                      "A good return on assets is considered to be above 5%"),
                      align = "center")),
                  box(
                    title = "Operating Margin",
                    status = "info",
                    solidHeader = TRUE,
                    height = 295,
                    h5("I'm working on this, just really tired of it")),

                  h1(div("Leverage Ratios",
                         style = "color:darkred",
                         align = "center")),
                  valueBox(
                    round(Close_debt_ratio,
                          digits = 3),
                    "Debt Ratio",
                    icon = icon("credit-card"),
                    width = 4,
                    color = "red"),
                  valueBox(
                    round(Close_equity_ratio,
                          digits = 3),
                    "Equity Ratio",
                    icon = icon("coins"),
                    width = 4,
                    color = "red"),
                  valueBox(
                    round(Close_interest_ratio,
                          digits = 3),
                    "Interest Coverage Ratio",
                    icon = icon("chart-line"),
                    width = 4,
                    color = "red"),

                  
                  
                  
                  h1(div("Efficiency Ratios",
                         style = "color:darkorange",
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
                  box(
                    title = "Return on Equity",
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
                   "A good current ratio is considered to be between 1 and 2."),
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
                   "A good quick ratio is considered to be above 1."),
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
      DT::datatable(Resultatregnskap)
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
