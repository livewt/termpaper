library(XML)
library(tidyverse)
library(taRifx)
library(bit64)
library(magrittr)
library(docstring)
library(devtools)
library(plotly)
library(ggplot2)

choose_file <- choose.files(caption ="Select your SAF-T file (xml format)")
#making DF from saf-t xml file
main <- xmlParse(choose_file)
#or choose automatic (for testing purposes)
main <- xmlParse("SAF-T Telenor 2019 (fictious).xml")

namespace <- xmlNamespaceDefinitions(main)[1]
namespace[[1]][1] == "nl"
namespace <- as.character(namespace[[1]][1])

namespace <- paste("//", namespace, sep = "", ":Account")


main_df <- xmlToDataFrame(nodes = getNodeSet(main, namespace))
main_df <- main_df %>%
  replace(is.na(.), 0) #theres not always registered numbers on the nodes.
#removing "helping account"
main_df <- main_df %>%
  filter(AccountDescription!="Hjelpekonto")


#Making sure all account IDS from specification by skatteetaten is in our data

#vektor1 contais account IDS from SAF-T file
dataframe_check_STD <- data.frame(main_df$StandardAccountID) %>%
  remove.factors(.)
vektor1 <- character()
for (chr in dataframe_check_STD){
  vektor1 <- c(vektor1, chr)
}
vektor1 <- vektor1 %>% unique()

#importing specification from skatteetaten
STD_accounts_check <- xmlParse("General_Ledger_Standard_Accounts_2_character.xml")
STD_accounts_check_df <- xmlToDataFrame(nodes = getNodeSet(STD_accounts_check, "//AccountID")) %>%
  remove.factors(.)

skatteetaten_unique <- unique(STD_accounts_check_df$text)
vektor2 <- character()
#adding IDs to vektor 2 if IDs from our data is in the specification by skatteetaten
for (i in 1:length(skatteetaten_unique)){
  if (vektor1[i] %in% skatteetaten_unique){
    vektor2 <- c(vektor2, vektor1[i])
  }
}
#making sure IDs from our data is equal to the IDS by skatteetaten
#Giving error message if its not true
for (i in 1:length(vektor1)){
  if (vektor2[i] != vektor1[i]){
    print("Your SAF-T file is not compatible with this program")
  }
}
#IDEA: making sure data is correct by asking user to verify accounts

######## Financial ratios #################

##current ratio

#base "as.integer()" has max value of +/-2*10^9. Base integer convert decimal right e.g "0.0" to 0
#"as.integer64()" has higher max value, but converts decimal chr to NA's. E.G "0.0" to NA
#Function to remove decimal from chr and return the "as.integer64" of it to do calcs later on
#Even on numbers exeeding 2*10^9
#Fixed problem with rounding!
charToInt64 <- function(s){
  #' Convert char elements in vector into int64
  #' 
  #' Removes decimal(s), rounds and converts to closest integer64 
  #'
  #'
  #' @param s the vector to be converted
  #' 
  stopifnot( is.character(s) )
  x <- character() #placeholder for the number
  y <- character() #placeholder for the decimals
  z <- list()
  for (i in s){
    z <- c(z, strsplit(i, "\\D")) #splits , and . and adds to list
  }
  for (i in z){ #for every first element add to x (e.g 10.1, then 10 will be added)
    x <- c(x, i[[1]])
    x <- as.integer64(x)
    
    if(!is.na(i[2])){ #Checks if there is a split (decimal)
      y <- c(y, i[[2]])
    }
    else {
      y <- c(y,0) #adds 0 as decimal if not
    }
  }
  for (i in 1:length(x)){ #checks if the first chr of the decimal is >= 5 for rounding
    if (substr(y[i],1,1) >=5){
      x[i] <- x[i]+1
    }
  }
  x #return the number
}

#test
testvektor <- c("0.0", "0", "10", "20000000000", "0.011", "1.1", "4,5") #last one should be rounded
charToInt64(testvektor)
docstring(charToInt64)
?charToInt64

#finally no bugs with numbers
#unfinshed code to start working on calculating fincancial numbers
#Converting chr to integer64 for all balances
main_df$OpeningDebitBalance <- charToInt64(main_df$OpeningDebitBalance)
main_df$OpeningCreditBalance <- charToInt64(main_df$OpeningCreditBalance)
main_df$ClosingDebitBalance <- charToInt64(main_df$ClosingDebitBalance)
main_df$ClosingCreditBalance <- charToInt64(main_df$ClosingCreditBalance)

#Functions to make financial figures calculations!

Open_func <- function (std_id){
  #' Output opening balance.
  #' 
  #' Subsets main_df based on standard account ID for Opening balance for assets. Returns the sum of subset$OpeningDebitBalance minus the sum of subset$OpeningCreditBalance
  #'
  #' @param std_id the standard account ID(s) for opening balance for assets
  #' 
  a <- subset(main_df, main_df$StandardAccountID==std_id) #subsetting standard acc ID
  #rownames(a) <- NULL #resetting rows
  b <- sum(a$OpeningDebitBalance) - sum(a$OpeningCreditBalance)
  b
}
docstring(Open_func)

Sum_Open_func <- function(start, end){
  #' Sums accounts with account IDs for Opening balance assets.
  #' 
  #' A for loop that iterates through accounts from based on ID, from start to end. Adds balance to vector for each iteration. Returns the sum of accounts between from start ID until end ID. 
  #'
  #' @param start the standard account ID to start loop
  #' @param end the standard account ID to stop loop
  #' 
  x <- integer64()
  for (i in start:end){
    x <- c(x, Open_func(i)) #uses the subset function above
  }
  x <- sum(x)
  x
}
docstring(Sum_Open_func)


Close_func <- function (std_id){
  #' Output closing balance.
  #' 
  #' Subsets main_df based on standard account IDs for closing balance for assets. Returns the sum of subset$ClosingDebitBalance minus the sum of subset$ClosingCreditBalance
  #'
  #' @param std_id the standard account IDs for closing balance for assets
  #' 
  a <- subset(main_df, main_df$StandardAccountID==std_id) #subsetting standard acc ID
  #rownames(a) <- NULL #resetting rows
  b <- sum(a$ClosingDebitBalance) - sum(a$ClosingCreditBalance)
  b
}
docstring(Close_func)

Sum_Close_func <- function(start, end){
  #' Sums accounts with account IDs for closing balance.
  #' 
  #' A for loop that iterates through accounts from based on ID, from start to end. Adds balance to vector for each iteration. Returns the sum of accounts between from start ID until end ID. 
  #'
  #' @param start the standard account ID to start loop
  #' @param end the standard account ID to stop loop
  #' 
  x <- integer64()
  for (i in start:end){
    x <- c(x, Close_func(i)) #uses the subset function above
  }
  x <- sum(x)
  x
}
docstring(Sum_Close_func)
### FINANCIAL RATIOS ###
#Remember to always switch - to + in equation since our functions already treats costs(debit) as -

##Opening balances###

#Current ratio (Likviditetsgrad 1)
Open_Current_ratio <- 
  Sum_Open_func(14,19)/
  -Sum_Open_func(23,29)

#Acid test for(Likviditetsgrad 2)
Open_Acid_test <-
  Sum_Open_func(15,19)/
  -Sum_Open_func(23,29)

#Gross profit % (Bruttofortjeneste i %)
Open_GrossProfit_percent <- # + since our function already treats costs as -
  (-Sum_Open_func(30,37) -Sum_Open_func(40,49))/
  -Sum_Open_func(30,37)
#Gross profit (Bruttofortjeneste)
Open_GrossProfit <- # + since our function already treats costs as -
  (-Sum_Open_func(30,37) - Sum_Open_func(40,49))
#Operating margin (Driftsmargin i %)
Open_Operating_margin <-
  (-Sum_Open_func(30,39) - Sum_Open_func(40,79))/
  -Sum_Open_func(30,39)
#Profit margin 1? (Resultatgrad i %)
Open_profit_margin1 <-
  (-Sum_Open_func(30,39) - Sum_Open_func(40,79)-Open_func(80))/
  -Sum_Open_func(30,39)
#Profit margin 2? (Resultatmargin i %)
Open_profit_margin2 <- #- since here we add tax and special costs to acc 88 to get pretax result
  (-Open_func(88) + Sum_Open_func(83,85))/
  -Sum_Open_func(30,39)
#Wages / sale income (L?nnskostnader i % av salgsinntekt)
Open_wages_sale_inc <-
  Sum_Open_func(50,59)/ # * -1 since costs is negative..
  -Sum_Open_func(30,37)

#Interest coverage ratio (Rentedekningsgrad)
Open_interest_ratio <- #- since add costs back to get ordinary profit pretax
  (-Open_func(88) + Sum_Open_func(83,86) + Open_func(81))/
  Open_func(81)

#Equity Ratio (Egenkapitalandel)
Open_equity_ratio <- 
  -Open_func(20)/
  Sum_Open_func(10,19)

#Debt ratio (Gjeldsgrad)
Open_debt_ratio <-
  -Sum_Open_func(21,29)/
  -Open_func(20)

###Closing balances###

#Current ratio (Likviditetsgrad 1)
Close_Current_ratio <-
  Sum_Close_func(14,19)/
  -Sum_Close_func(23,29)

#Acid test (Likviditetsgrad 2)
Close_Acid_test <-
  Sum_Close_func(15,19)/
  -Sum_Close_func(23,29)
#Gross profit % (Bruttofortjeneste i %)
Close_GrossProfit_percent <- 
  (-Sum_Close_func(30,37) - Sum_Close_func(40,49))/
  -Sum_Close_func(30,37)
#Gross profit (Bruttofortjeneste)
Close_GrossProfit <- 
  -Sum_Close_func(30,37) - Sum_Close_func(40,49)
#Operating margin (Driftsmargin i %)
Close_Operating_margin <- 
  (-Sum_Close_func(30,39) - Sum_Close_func(40,79))/
  -Sum_Close_func(30,39)
#Profitmargin 1 (Resultatgrad i %)
Close_profit_margin1 <-
  (-Sum_Close_func(30,39) - Sum_Close_func(40,49) - Close_func(80))/
  -Sum_Close_func(30,39)
#Profitmargin 2 (Resultatmargin i %)
Close_profit_margin2 <-
  (-Close_func(88) + Sum_Close_func(83,85))/
  -Sum_Close_func(30,39)
#Wages/sale income (L?nnskostnader i % av salgsinntekt)
Close_wages_sale_inc <- 
  Sum_Close_func(50,59)/
  -Sum_Close_func(30,37)
#Interest Coverage Ratio (Rentedekningsgrad)
Close_interest_ratio <- 
  (-Close_func(88) + Sum_Close_func(83,86) + Close_func(81))/
  Close_func(81)

#Equity Ratio (Egenkapitalandel)
Close_equity_ratio <- 
  -Close_func(20)/
  Sum_Close_func(10,19)

#Debt ratio (Gjeldsgrad)
Close_debt_ratio <-
  -Sum_Close_func(21,29)/
  -Close_func(20)

##Financial ratios exclusive to closing balance##

#Return on assets (Totalkapitalens rentabilitet)
Return_assets <- 
  (-Close_func(88) + Sum_Close_func(83,86) + Close_func(81))/
  ((Sum_Open_func(10,19) + Sum_Close_func(10,19))/2)

#Capital turn over rate (Kapitalens oml?pshastighet)
Capital_turnover <-
  -Sum_Close_func(30,39)/
  ((Sum_Open_func(10,19)+Sum_Close_func(10,19))/2)

#Inventory turnover rate
Inventory_turnover <-
  Sum_Close_func(40,49)/
  ((Open_func(14)+ Close_func(14))/2)

#Equity return pre tax (Egenkapitalens rentabilitet)
Return_equity <-
  (-Close_func(88) + Sum_Close_func(83,86))/
  ((-Open_func(20) - Close_func(20))/2)

# Visualizations!

# Return on equity

roe = plot_ly(
  value = round(Return_equity * 100,
                digits = 2),
  number = list(suffix = "%"),
  type = "indicator",
  mode = "gauge+number",
  height = 200,
  gauge = list(
    axis = list(range = list(NULL, 100),
                tickcolor = "darkorange",
                ticksuffix = "%"),
    bar = list(color = "darkorange"),
    borderwidth = 1))

roe = 
  roe %>% 
  layout(margin = list(l = 20, r = 30),
         font = list(color = "darkorange"))

# Return on assets

roa = 
  plot_ly(
    value = Return_assets * 100,
    number = list(suffix = "%"),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      axis = list(range = list(NULL, 100),
                  tickcolor = "darkgreen",
                  ticksuffix = "%"),
      bar = list(color = "darkgreen"),
      borderwidth = 1))

roa =
  roa %>% 
  layout(margin = list(l = 20, r = 30),
         font = list(color = "darkgreen"))

# Wages to sales ratio

wtos_when = c("Beginning of Year", "End of Year")
wtos_value = c(Open_wages_sale_inc*100,
               Close_wages_sale_inc*100)
w_to_s = 
  data.frame(wtos_when,
             wtos_value)

w_to_s$wtos_value = round(w_to_s$wtos_value,
                          digits = 2)


w_to_s_chart = 
  plot_ly(w_to_s,
          x = c("Beginning of Year","End of Year"),
          y = wtos_value,
          type = "bar",
          marker = list(color = 
                          c("yellow","orange")),
          opacity = 0.6,
          height = 300)

mytext = c(round(Open_wages_sale_inc*100,
                 digits = 2),
           round(Close_wages_sale_inc*100,
                 digits = 2))

mytext = paste(mytext,"%", sep = "")

w_to_s_chart = 
  w_to_s_chart %>% 
  add_annotations(text = mytext)

# Current ratio

current_chart = 
  plot_ly(
  value = round(Close_Current_ratio,
                digits = 3),
  type = "indicator",
  mode = "gauge+number+delta",
  height = 200,
  gauge = list(
    axis = list(range = list(NULL, 2),
                tickcolor = "black"),
    bar = list(color = "black",
               width = 1),
    borderwidth = 1,
    steps = list(
      list(range = c(0,0.7), color = "red"),
      list(range = c(0.7,1), color = "yellow"),
      list(range = c(1, 2), color = "green"))),
  delta = list(reference = round(Open_Current_ratio,
                                 digits = 2)))

current_chart = 
  current_chart %>% 
  layout(margin = list(l = 20, r = 30),
         font = list(color = "black"))

# Quick Ratio (Acid Test)

quick_chart = 
  plot_ly(
    value = Close_Acid_test,
    type = "indicator",
    mode = "gauge+number+delta",
    height = 200,
    gauge = list(
      axis = list(range = list(NULL,2),
                  tickcolor = "black"),
      bar = list(color = "black",
                 width = 1),
      borderwidth = 1,
      steps = list(
        list(range = c(0,0.7), color = "red"),
        list(range = c(0.7,1), color = "yellow"),
        list(range = c(1,2), color = "green"))),
    delta = list(reference = round(Open_Acid_test,
                                   digits = 2)))

quick_chart = 
  quick_chart %>% 
  layout(margin = list(l = 20, r = 30),
         font = list(color = "black"))

# Higher_lower function for explaining purposes in the Shiny app:
# Inputs are opening ratio and closing ratio.
# Prints "higher than" if closing ratio is higher than opening ratio,
# "lower than" if opening ratio is higher than closing ratio,
# and "equal to" if closing ratio and opening ratio is the same.

higher_lower = function(open,close){
  for(i in 1:length(c(open,close))){
    if(open - close > 0){
      solution = c("lower than")
      break}
    if(close - open > 0){
      solution = c("higher than")
      break}
    else
      solution = ("equal to what")
      }
  print(solution)
}



#######################trans plot####
library(XML)
library(tidyverse)
library(taRifx)
library(bit64)
#for example 3yrs old
#choose_file <- choose.files(caption ="Select your SAF-T file (xml format)")



#main <- xmlParse(choose_file)
#main <- xmlParse("saf-t example (3yrs old).xml") #placeholder to skip choosing manually
#main <- (choose_file)

main_list <- 
  xmlToList(main) #making a list of the xml file


main_list <- xmlToList(main)


#as.data.frame(list[[3]][[4]][[4]])


journal <- main_list[[3]][[4]] %>% #extracting the journal information from the list
  map_df(~as.data.frame(.)) #and making a df with map function
#test %>%
# select(Period, test[ ,grepl("Line.Amount", names(test))])

trans_sum <- journal[ ,grepl("Line.Amount", names(journal))] #making new df with all the different records (lines). This is dynamic so doesnt matter if max 7 or 4 records in the journal
trans_sum <- trans_sum %>%
  map_df(as.numeric) #mapping the columns to numeric instead of chr
trans_sum[is.na(trans_sum)] <- 0 #and making NAs to 0s

#Now we have all transactions in trans_sum. Here they are recorded 2 times (credit and debit to different account).
#we want only the transcation counted 1 time, so to do this we simply sum the rows and divide by 2.
trans_sum <- trans_sum %>%
  mutate(Sum = rowSums(.)) %>%
  mutate(Amounts = Sum/2)
#then cbinding the new "amounts" column back to journal df.
journal <- cbind(journal, trans_sum$Amounts)

#making a new df with info used to plotting
plot_info <- journal%>%
  select(Description, TransactionID,`trans_sum$Amounts`, TransactionDate)

#mva_test <- mva_test %>%
# drop_na(Line.TaxInformation.TaxType)
plot_info <- plot_info %>%
  drop_na(TransactionID) #removes first 3 rows of journal, which always will be there(standarized xml structure)
#mva_test2 <- mva_test$`sum$Amounts` %>%
#  as.numeric()
#mva_test2 <- as.data.frame(mva_test2)
#mva_test2$mva_test22 <- mva_test2$mva_test2

#mva_test %>%
#  head
#For colouring purposes?
#wssplot <- function(data, nc=15, seed=1234){ #function found online!!!
#  wss <- (nrow(data)-1)*sum(apply(data,2,var))
#  for (i in 2:nc){
#    set.seed(seed)
#    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
#  plot(1:nc, wss, type="b", xlab="Number of Clusters",
#       ylab="Within groups sum of squares")
#  wss
#}

#wssplot(mva_test2)
#cluster_amount <- as.numeric(readline(prompt = "How many clusters do you want? "))
#KM = kmeans(mva_test2, cluster_amount)
#KM$betweenss
#KM$centers
#cluster <- KM$cluster
#autoplot(KM,mva_test2, frame=TRUE)

#install.packages("ggiraph")
library(ggiraph)
#mva_test$cluster <- as.character(KM$cluster)
#mva_test$Line.Amount.1 <- as.numeric(mva_test$Line.Amount.1)
plot_info$TransactionID <- as.numeric(plot_info$TransactionID)

tooltip_ <- c(paste0("Description: ", plot_info$Description,
                     "\n Transaction ID: ", plot_info$TransactionID,
                     "\n Amount: ", as.integer(plot_info$`trans_sum$Amounts`), " NOK")) #int to remove uneccesary deciamls in plot

transaction_plot <- ggplot(data = plot_info) +
  geom_point_interactive(aes(x = 1:53, y = `trans_sum$Amounts`,
                             tooltip = tooltip_, data_id = TransactionID))+
  ylab("Amount")+
  xlab("Transcation number")+
  ggtitle("Hover over points to view description of the transcation")+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)


transaction_plot
#girafe(code = print(testplot))
girafe(ggobj = transaction_plot)


#check if all transcations are included (53 in this case)
as.numeric(main_list[[3]][[1]]) == length(plot_info$`trans_sum$Amounts`)
#check if sum of transcations is right
sum(plot_info$`trans_sum$Amounts`)== as.numeric(main_list[[3]][[2]])
as.numeric(main_list[[3]][[3]]) == as.numeric(main_list[[3]][[2]])

library(anytime)

plot_info$TransactionDate <- anytime(plot_info$TransactionDate)

max(plot_info$TransactionDate)
