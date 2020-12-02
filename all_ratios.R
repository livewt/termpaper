# Making data frame to compare relevant financial ratios
# from beginning of the year to the end of the year

open_ratios = 
  c(Open_Current_ratio,
    Open_Acid_test,
    Open_GrossProfit_percent,
    Open_GrossProfit,
    Open_Operating_margin,
    Open_profit_margin1,
    Open_profit_margin2,
    Open_wages_sale_inc,
    Open_interest_ratio,
    Open_equity_ratio,
    Open_debt_ratio)

close_ratios =
  c(Close_Current_ratio,
    Close_Acid_test,
    Close_GrossProfit_percent,
    Close_GrossProfit,
    Close_Operating_margin,
    Close_profit_margin1,
    Close_profit_margin2,
    Close_wages_sale_inc,
    Close_interest_ratio,
    Close_equity_ratio,
    Close_debt_ratio)

alltsaman = 
  c(open_ratios,
    close_ratios)

myrows = 
  c("Current Ratio",
    "Acid Test",
    "Gross Profit (%)",
    "Gross profit",
    "Operating Margin (%)",
    "Profit Margin 1?",
    "Profit Margin 2?",
    "Wages/Sale income",
    "Interest Coverage Ratio",
    "Equity Ratio",
    "Debt Ratio")

mycols = 
  c("Beginning of year",
    "End of year")

all_ratios =
  data.frame(
    "Beginning of year" = round(open_ratios,
                                digits = 4),
    "End of year" = round(close_ratios,
                          digits = 4),
    row.names = myrows)