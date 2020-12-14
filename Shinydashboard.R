
# Source the "Project.R" file with encoding UTF-8 to get norwegian letters.
# We choose the "SAF-T fictitious company.xml" file.

source("Project.R", encoding = "UTF-8")

# Create ui

ui = # Check if all the error handling measures in Project.R is passed
  if (the_true_test == TRUE & class(main)[1] == "XMLInternalDocument" & class(main)[2] == "XMLAbstractDocument"){
  dashboardPage(
    skin = "black",
    dashboardHeader( # Create header
      title = "SAF-T Analyseprogram",
      titleWidth = 250),
    dashboardSidebar( # Create sidebar
      width = 250,
      sidebarMenu( # Create items in sidebar
        menuItem("Startside",
                 tabName = "about",
                 icon = icon("info-circle")),
        menuItem("Dashboard",
                 tabName = "dashboard",
                 icon = icon("dashboard")),
        menuItem("Transaksjoner",
                 tabName = "transactions",
                 icon = icon("people-arrows")),
        menuItem("Resultatregnskap",
                 tabName = "incomestatement",
                 icon = icon("cash-register")),
        menuItem("Balanseoppstilling",
                 tabName = "balancestatement",
                 icon = icon("balance-scale")))),

    
    dashboardBody( # Create the page's body
      fluidRow(
        tags$head(
          tags$style(HTML("hr {border-top: 1px solid #000000;}")), # make hr more visible
          tags$style(HTML("
            .content{
               width: 90%;
            }"))
        ),
        tabItems( # Add content to the "Start Here" page
          tabItem(tabName = "about",
                  h1(div(
                    "Velkommen!",
                    style = "color:navy",
                    align = "center")),
                  box(
                    background = "navy",
                    title = "Hvordan bruke SAF-T dashboardet",
                    h5(
                        "Dashboardet er ment for analyse av SAF-T regnskap. SAF-T Regnskap (Financial)
                        er et standardformat for utveksling av regnskapsdata. 
                        SAF-T, eller Standard Audit File-Tax, er utviklet i fellesskap av bransjeorganisasjoner,
                        systemleverandører og Skatteetaten, etter anbefaling fra OECD."),
                      h5(
                       "Dashboardet fungerer best med maksimert vindu.
                       Dersom man forandrer størrelse på vinduet, kan det være lurt å trykke på oppdater.")),
                  box(
                    title = "Studenter",
                    background = "navy",
                    height = 210,
                    h5(
                      "Herdís Birta Jónsdóttir"),
                    h5(
                      "Live Wold Thorsø"),
                    h5(
                      "Morten Hønsi Følling"),
                    h5(
                      "Sindre Lunner Nyberg"))),
          
          tabItem(tabName = "dashboard", # Add content to the "Dashboard" page
                  h1(div("Profitability Ratios",
                         style = "color:dodgerblue",
                         align = "center")),
                  box(
                    title = "Bruttofortjeneste",
                    status = "info",
                    solidHeader = TRUE,
                    height = 417,
                    h5(div(
                      "Bruttofortjenesten er", higher_lower(Open_GrossProfit,
                                                      Close_GrossProfit),
                      "den var i fjor"),
                      align = "center"),
                    div(profit_chart,
                        align = "center")),
                  box(
                    title = "Totalkapitalens rentabilitet",
                    status = "info",
                    solidHeader = TRUE,
                    height = 295,
                    div(roa,
                        align = "center"),
                    h6(div(
                      "En god totalkapitalrentabilitet pleier å være over 5%"),
                      align = "center")),
                  valueBox(
                    paste(round(Close_profit_margin1*100,
                          digits = 1), "%", sep = ""),
                    "Resultatsgrad",
                    icon = icon("piggy-bank"),
                    width = 3,
                    color = "aqua"),
                  valueBox(
                    paste(round(Close_profit_margin2*100,
                          digits = 1),"%", sep = ""),
                    "Resultatsmargin",
                    icon = icon("money-check-alt"),
                    width = 3,
                    color = "aqua"),
                  box(
                    title = "Bruttofortjeneste i %",
                    status = "info",
                    solidHeader = TRUE,
                    height = 350,
                    h5(div("Bruttofortjeneste i % er",
                       higher_lower(Open_GrossProfit_percent,
                                    Close_GrossProfit_percent),
                       "den var i fjor"),
                       align = "center"),
                    div(gross_percent_chart,
                        align = "center"),
                    h6(div(
                      "Høy: 20%, Gjennomsnitt: 10%, Lav: 5%"),
                      align = "center")),
                  box(
                    title = "Driftsmargin",
                    status = "info",
                    solidHeader = TRUE,
                    height = 350,
                    h5(div("Driftsmargin er",
                           higher_lower(Open_Operating_margin,
                                        Close_Operating_margin),
                           "den var i fjor"),
                       align = "center"),
                    div(operating_chart,
                        align = "center"),
                    h6(div(
                      "Høy: 20%, Gjennomsnitt: 10%, Lav: 5%"),
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
                   title = "Lønnskostnader i % av salgsinntekt",
                   status = "warning",
                   solidHeader = TRUE,
                   height = 438,
                   h5(div(
                     "Lønnskostnader i % av salgsinntekt er",
                     higher_lower(Open_wages_sale_inc,
                                  Close_wages_sale_inc),
                     "den var på begynnelsen av året",
                     align = "center")),
                   div(w_to_s_chart,
                       align = "center")),
                  box(
                    title = "Egenkapitalens rentabilitet",
                    status = "warning",
                    solidHeader = TRUE,
                    height = 295,
                    div(roe,
                        align = "center"),
                    h6(div(
                      "Egenkapitalens rentabilitet er kalkulert før-skatt"),
                      align = "center")),
                valueBox(
                 round(Capital_turnover,
                       digits = 3),
                 "Kapitalens omløpshastighet",
                 icon = icon("hand-holding-usd"),
                 width = 3,
                 color = "orange"),
               valueBox(
                 round(Inventory_turnover,
                       digits = 3),
                 "Varelagerets omløpshastighet",
                 icon = icon("warehouse"),
                 width = 3,
                 color = "orange"),
               
               h1(div("Likviditetsnøkkeltall",
                      style = "color:green",
                      align = "center")),
               box(
                 title = "Likviditetsgrad 1",
                 status = "success",
                 solidHeader = TRUE,
                 h5(div(
                   "Likviditetsgrad 1 er",
                   higher_lower(Open_Current_ratio,
                                Close_Current_ratio),
                                " den var på begynnelsen av året"),
                   align = "center"),
                 div(current_chart,
                     align = "center"),
                 h6(div(
                   "En god likviditetsgrad 1 pleier å ligge mellom 1 og 2"),
                   align = "center")),
               box(
                 title = "Likviditetsgrad 2",
                 status = "success",
                 solidHeader = TRUE,
                 h5(
                   div(
                   "Likviditetsgrad 2 er",
                   higher_lower(Open_Acid_test,
                                Close_Acid_test),
                   "den var på begynnelsen av året"),
                   align = "center"),
                 div(quick_chart,
                     align = "center"),
                 h6(div(
                   "En god likviditetsgrad 2 pleier å være større enn 1"),
                   align = "center"))),
          tabItem(tabName = "transactions", # Add content to the "Transactions" page
                  box(
                    solidHeader = FALSE,
                    width = 12,
                    girafeOutput("trans_plot"),
                    dateRangeInput(inputId = "date",
                                   label = "Velg datointervall",
                                   start = min(plot_info$TransactionDate),
                                   end = max(plot_info$TransactionDate),
                                   min = min(plot_info$TransactionDate),
                                   max = max(plot_info$TransactionDate)),
                    numericRangeInput(inputId = "monetary",
                                      label ="Velg beløpintervall",
                                      value = c(min(plot_info$`trans_sum$Amounts`),
                                      max(plot_info$`trans_sum$Amounts`)))
                  )),
  
           tabItem(tabName = "balancestatement", # Add content to the "Balance Statement" page"
              h1(
                 "Balanseoppstilling"),
              tabPanel("BalanseEiendeler", DT::dataTableOutput("BalanseEiendeler")),
                hr(),
              tabPanel("BalanseEKGJ", DT::dataTableOutput("BalanseEKGJ"))),
           tabItem(tabName = "incomestatement", # Add content to the "Income Statement" page
              h1(
                "Resultatregnskap"),
             tabPanel("art.data", DT::dataTableOutput("art.data")))
           

      )
    )
  ))
  } else {
    #If a file with the wrong format is selected, the output is a simple error window
      fluidPage(
        titlePanel("Error! Filen er ikke kompatibel. Vennligst lukk vinduet og velg en annen fil."))
    
  }

# Create server
server = 
  function(input,output){ # Check if all the error handling measures in Project.R is passed
    if (the_true_test == TRUE & class(main)[1] == "XMLInternalDocument" & class(main)[2] == "XMLAbstractDocument"){
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
# Reactive function which subsets the trans_info. Used for transaction plot. Subsets on date range and monetary range
    trans_subset <- reactive({
      a <- subset(plot_info, plot_info$TransactionDate>= input$date[1] & plot_info$TransactionDate <= input$date[2]
                  & plot_info$`trans_sum$Amounts` >= input$monetary[1] & plot_info$`trans_sum$Amounts` <= input$monetary[2])
      a
    })

    #girafeplot which makes it possible to hover over observations in transaction plot                   
    output$trans_plot <- renderGirafe({
      if (nrow(trans_subset()) >0){#check if the subsetting doesnt remove all rows
      plottt <- ggplot(data = trans_subset()) +
        geom_point_interactive(aes(x = 1:length(`trans_sum$Amounts`), y = `trans_sum$Amounts`,
                                   tooltip = c(paste0("Tekst: ", Description,
                                   "\n Transaksjons ID: ", TransactionID,
                                   "\n Beløp: ", as.integer(`trans_sum$Amounts`), " NOK")),
                                   data_id = TransactionID))+
        ylab("Beløp")+
        xlab(" ")+
        ggtitle("Beveg musa over observasjonene for å se transaksjonsinfo")+
        (if (nrow(trans_subset())==1){
          labs(subtitle = paste0("Du ser nå ",nrow(trans_subset())," transaksjon"))
        }
        else {
            labs(subtitle = paste0("Du ser nå ",nrow(trans_subset())," transaksjoner"))
          })+
        scale_y_continuous(labels = scales::comma)+
        theme(axis.text.x=element_blank())
        
        girafe(ggobj = plottt)
      }else{ #if all rows removed, output error plot
        girafe(ggobj = error_plot)
    }
    })
    }}
    


# Run app
shinyApp(ui,server)
