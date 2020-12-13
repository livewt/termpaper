
#making DF from saf-t xml file
choose_file <- choose.files(caption ="Select your SAF-T file (xml format)")

the_true_test <- FALSE #placeholder used to error handling
main <- FALSE # placeholder used for error handling
try(
main <- xmlParse(choose_file), silent = T)
if (class(main)[1] == "XMLInternalDocument" & class(main)[2] == "XMLAbstractDocument"){ #this throws out anything not XML

#extracting namespace from xml file
namespace <- xmlNamespaceDefinitions(main)[1]
namespace <- as.character(namespace[[1]][1])
namespace <- paste("//", namespace, sep = "", ":Account")


main_df <- xmlToDataFrame(nodes = getNodeSet(main, namespace)) #extracting all the "Account" nodes
main_df <- main_df %>%
  replace(is.na(.), 0) #there is not always registered nodes for opening/closing, debit/credit on all account nodes!. Thats also the reason for "warnings" when making DF.

#removing "helping account" if any
try(
main_df <- main_df %>%
  filter(AccountDescription!="Hjelpekonto")
, silent=T)


#vektor1 contains account IDS from SAF-T file
try(
dataframe_check_STD <- data.frame(main_df$StandardAccountID) %>%
  remove.factors(.)
, silent = T)
vektor1 <- character()
try(
for (chr in dataframe_check_STD){
  vektor1 <- c(vektor1, chr)
}
, silent =T)
vektor1 <- vektor1 %>% unique() #we want the unique standard account IDs

#We also want the expected coloumn names which our program will produce from saf-t file (standardized text in saf-t)
coltest <- c("AccountID", "AccountDescription", "StandardAccountID", "AccountType", "OpeningDebitBalance",
             "ClosingDebitBalance", "OpeningCreditBalance", "ClosingCreditBalance")

truecols <- vector()
try(
for (i in 1:length(coltest)){ #checking column names from the saf-t file and the expected outcome.
  if (colnames(main_df)[i] == coltest[i]){
    truecols <- c(truecols, TRUE) #returns true values if theres a match
  } else {
    truecols <- c(truecols, FALSE)
  }
}
, silent = T)

#this vector is all the expected unique standard acccount IDs.
#saf-t file should be mapped to include them all. And we only accept saf-t files that fullfill this requirement
std.acc.vector <- c("10", "11","12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "36", "37",
                  "38", "39", "40", "41", "42", "43", "45", "49", "50", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
                  "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "83", "84", "85", "86", "88", "89")
std.acc.test <- std.acc.vector == vektor1 #for testing purposes below

#only if the xml file fullfills this set of requirements, the rest of the code will run. Otherwise "the_true_test" is false and shiny will show custom error output
if (!FALSE %in% truecols & !FALSE %in% std.acc.test & length(truecols) == 8 & length(std.acc.test)==72){
  the_true_test <- TRUE
} else {
  the_true_test <- FALSE
}

if (the_true_test ==TRUE){

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


docstring(charToInt64)
?charToInt64

#Converting chr to integer64 for all balances
main_df$OpeningDebitBalance <- charToInt64(main_df$OpeningDebitBalance)
main_df$OpeningCreditBalance <- charToInt64(main_df$OpeningCreditBalance)
main_df$ClosingDebitBalance <- charToInt64(main_df$ClosingDebitBalance)
main_df$ClosingCreditBalance <- charToInt64(main_df$ClosingCreditBalance)

#Functions to make financial figures calculations!

Open_func <- function (std_id){
  #' Output opening balance.
  #' 
  #' Subsets main_df based on standard account ID for Opening balance. Returns the sum of subset$OpeningDebitBalance minus the sum of subset$OpeningCreditBalance
  #'
  #' @param std_id the standard account ID(s) for opening balance for assets
  #' 
  a <- subset(main_df, main_df$StandardAccountID==std_id) #subsetting standard acc ID
  b <- sum(a$OpeningDebitBalance) - sum(a$OpeningCreditBalance)
  b
}
docstring(Open_func)

Sum_Open_func <- function(start, end){
  #' Sums accounts with account IDs for Opening balance.
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
  #' Subsets main_df based on standard account IDs for closing balance. Returns the sum of subset$ClosingDebitBalance minus the sum of subset$ClosingCreditBalance
  #'
  #' @param std_id the standard account IDs for closing balance for assets
  #' 
  a <- subset(main_df, main_df$StandardAccountID==std_id) #subsetting standard acc ID
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
Open_GrossProfit_percent <-
  (-Sum_Open_func(30,37) -Sum_Open_func(40,49))/
  -Sum_Open_func(30,37)
#Gross profit (Bruttofortjeneste)
Open_GrossProfit <- 
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
Open_profit_margin2 <- 
  (-Open_func(88) + Sum_Open_func(83,85))/
  -Sum_Open_func(30,39)
#Wages / sale income (L?nnskostnader i % av salgsinntekt)
Open_wages_sale_inc <-
  Sum_Open_func(50,59)/ 
  -Sum_Open_func(30,37)

#Interest coverage ratio (Rentedekningsgrad)
Open_interest_ratio <- 
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
#Wages/sale income (Lnnskostnader i % av salgsinntekt)
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

# Visualizations:

# Return on equity

roe = plot_ly(
  value = round(Return_equity * 100,
                digits = 1),
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
    value = round(Return_assets * 100,
                  digits = 1),
    number = list(suffix = "%"),
    type = "indicator",
    mode = "gauge+number",
    height = 200,
    gauge = list(
      axis = list(range = list(NULL, 100),
                  tickcolor = "dodgerblue",
                  ticksuffix = "%"),
      bar = list(color = "dodgerblue"),
      borderwidth = 1,
      threshold = list(
        line = list(color = "red", width = 2),
        thickness = 1,
        value = 5)))

roa =
  roa %>% 
  layout(margin = list(l = 20, r = 30),
         font = list(color = "dodgerblue"))

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

# Gross profit

profit_when = c("Last Year", "This Year")
profit_value = as.numeric(c(Open_GrossProfit,
                            Close_GrossProfit))
profit = 
  data.frame(profit_when,
             profit_value)

profit_chart = 
  plot_ly(profit,
          x = c("Last Year", "This Year"),
          y = profit_value,
          type = "bar",
          marker = list(color = 
                          c("darkblue","dodgerblue")),
          opacity = 0.6,
          height = 300)

profit_text = as.character(format(profit$profit_value,
                                  big.mark = ","))

profit_chart = 
  profit_chart %>% 
  add_annotations(text = profit_text)

# Operating Margin

operating_chart =
  plot_ly(
    type = "indicator",
    mode = "number+gauge+delta",
    value = round(Close_Operating_margin*100,
                  digits = 1),
    delta = list(reference = round(Open_Operating_margin*100,
                                   digits = 1),
                 ticksuffix = "%"),
    gauge = list(
      axis = list(range = list(NULL,100),
                  tickcolor = "purple",
                  ticksuffix = "%"),
      bar = list(color = "purple",
                 width = 1),
      borderwidth = 1,
      steps = list(
        list(range = c(4.7,5.3), color = "red"),
        list(range = c(9.7,10.3), color = "yellow"),
        list(range = c(19.7,20.3), color = "green"))),
    height = 210,
    number = list(suffix = "%"))

operating_chart = 
  operating_chart %>% 
  layout(margin = list(l = 20, r = 30),
         font = list(color = "purple"))


# Gross Profit Margin

gross_percent_chart =
  plot_ly(
    type = "indicator",
    mode = "number+gauge+delta",
    value = round(Close_GrossProfit_percent*100,
                  digits = 3),
    delta = list(reference = round(Open_GrossProfit_percent*100,
                                   digits = 1),
                 ticksuffix = "%"),
    gauge = list(
      axis = list(range = list(NULL,100),
                  tickcolor = "navy",
                  ticksuffix = "%"),
      bar = list(color = "navy",
                 width = 1),
      borderwidth = 1,
      steps = list(
        list(range = c(4.7,5.3), color = "red"),
        list(range = c(9.7,10.3), color = "yellow"),
        list(range = c(19.7,20.3), color = "green"))),
      height = 210,
    number = list(suffix = "%"))

gross_percent_chart = 
  gross_percent_chart %>% 
  layout(margin = list(l = 20, r = 30),
         font = list(color = "navy"))

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
  solution
}



#######################transaction plot##############################
library(XML)
library(tidyverse)
library(taRifx)
library(bit64)

#making list of the same xml file as in the very start of this code.
main_list <- 
  xmlToList(main) 

#The main purpose for this different extracting of numbers is that in the journal section of the saf-t files
# there will be duplicate coulmns if doing the xmlToDataframe approach.
# all transactions is registered doubble (credit and debit), but there might be different records for each transaction.
# example: one transaction can be registered to as debit to 1 account and credited on 1, 2, 3, 4 etc different accounts
main_list <- xmlToList(main)

journal <- main_list[[3]][[4]] %>% #extracting the journal information from the list (standarized saf-t format and should work across all saf-t files)
  map_df(~as.data.frame(.)) #and making a df with map function

trans_sum <- journal[ ,grepl("Line.Amount", names(journal))] #making new df with all the different records (lines). This is dynamic so doesnt matter if max 7 or 4 records in the journal
trans_sum <- trans_sum %>%
  map_df(as.numeric) #mapping the columns to numeric instead of chr
trans_sum[is.na(trans_sum)] <- 0 #and making NAs to 0s

#Now we have all transactions in trans_sum. Here they are recorded 2 times (credit and debit to different accounts).
#we want only the transcation counted 1 time, so to do this we simply sum the rows and divide by 2.
trans_sum <- trans_sum %>%
  mutate(Sum = rowSums(.)) %>%
  mutate(Amounts = Sum/2)
#then cbinding the new "amounts" column back to journal df.
journal <- cbind(journal, trans_sum$Amounts)

#making a new df with info used to plotting
plot_info <- journal%>%
  select(Description, TransactionID,`trans_sum$Amounts`, TransactionDate)


plot_info <- plot_info %>%
  drop_na(TransactionID) #removes first 3 rows of journal, which always will be there(standarized xml structure)

#converting coloumns
plot_info$TransactionID <- as.numeric(plot_info$TransactionID)
plot_info$TransactionDate <- as.Date(plot_info$TransactionDate)


#error plot
#An alternative plot which will be outputted when reactive function in shiny subsets the plot_info so its empty
error_plot_df <- data.frame(matrix(nrow=1,ncol=1))

error_plot <- ggplot (data = error_plot_df)+
  geom_text(aes(1,1),label="No transactions in given date- or monetary amount range", size=5)+
  theme(text=element_text(size=200),
        axis.title = element_blank(),
        axis.text=element_blank())

##########################################################################################

#Sum by StandardAccountID - Debit
SumBySAIDDebit <- aggregate(main_df$ClosingDebitBalance - main_df$ClosingCreditBalance,
                             by=list(StandardAccountID=main_df$StandardAccountID),
                             FUN=sum)
SumBySAIDDebit$StandardAccountID <- as.numeric(SumBySAIDDebit$StandardAccountID)

#Sum by StandardAccountID - Credit
SumBySAIDCredit <- aggregate(main_df$ClosingCreditBalance - main_df$ClosingDebitBalance,
                       by=list(StandardAccountID=main_df$StandardAccountID),
                       FUN=sum)
SumBySAIDCredit$StandardAccountID <- as.numeric(SumBySAIDCredit$StandardAccountID) 

#Sum by Account ID - Debit
SumByAIDDebit <- aggregate(main_df$ClosingDebitBalance - main_df$ClosingCreditBalance,
                      by=list(AccountID=main_df$AccountID),
                      FUN=sum)
SumByAIDDebit$AccountID <- as.numeric(SumByAIDDebit$AccountID)

#Sum by Account ID - Credit
SumByAIDCredit <- aggregate(main_df$ClosingCreditBalance - main_df$ClosingDebitBalance,
                           by=list(AccountID=main_df$AccountID),
                           FUN=sum)
SumByAIDCredit$AccountID <- as.numeric(SumByAIDCredit$AccountID)


#--------------------
#RESULTATREGNSKAP ETTER ART
#--------------------

#Vector with row names for income statement
`Resultatregnskap etter art` <- c('Salgsinntekt', 'Varekostnad', 'Lønnskostnad',
                                  'Avskrivning', 'Nedskrivning',
                                  'Annen driftskostnad', 'Finansinntekt',
                                  'Finanskostnad',
                                  'Skattekostnad på ordinært resultat',
                                  'Ekstraordinær inntekt',
                                  'Ekstraordinær kostnad',
                                  'SKattekostnad på ekstraordinært resultat',
                                   'Ãrsresultat', 'Overføringer/disponeringer')

#Calculate numbers for income statement
SI <- as.numeric(sum(SumByAIDCredit[which(SumByAIDCredit[,1]>=3000 & SumByAIDCredit[,1]<=3970),2]))
VK <- as.numeric(sum(SumByAIDDebit[which(SumByAIDDebit[,1]>=4000 & SumByAIDDebit[,1]<=4990),2]))
LK <- as.numeric(sum(SumByAIDDebit[which(SumByAIDDebit[,1]>=5000 & SumByAIDDebit[,1]<=5930),2]))
Avskr <- as.numeric(sum(SumByAIDDebit[which(SumByAIDDebit[,1]>=6000 & SumByAIDDebit[,1]<=6020),2]))
Nedskr <- as.numeric(subset(`SumByAIDCredit`, AccountID == 6050)[,2])
AnnenDK <- as.numeric(sum(SumByAIDDebit[which(SumByAIDDebit[,1]>=6100 & SumByAIDDebit[,1]<=7910),2]))
FI <- as.numeric(sum(SumByAIDCredit[which(SumByAIDCredit[,1]>=8000 & SumByAIDCredit[,1]<=8080),2]))
FK <- as.numeric(sum(SumByAIDDebit[which(SumByAIDDebit[,1]>=8100 & SumByAIDDebit[,1]<=8170),2]))
SKord <- as.numeric(sum(SumByAIDDebit[which(SumByAIDDebit[,1]>=8300 & SumByAIDDebit[,1]<=8320),2]))
EkstraI <- as.numeric(sum(SumByAIDCredit[which(SumByAIDCredit[,1]>=8400 & SumByAIDCredit[,1]<=8499),2]))
EkstraK <- as.numeric(sum(SumByAIDDebit[which(SumByAIDDebit[,1]>=8500 & SumByAIDDebit[,1]<=8599),2]))
SKekstra <- as.numeric(sum(SumByAIDDebit[which(SumByAIDDebit[,1]>=8600 & SumByAIDDebit[,1]<=8620),2]))
Res <- as.numeric(subset(`SumByAIDCredit`, AccountID == 8800)[,2])
Disp <- as.numeric(sum(SumByAIDDebit[which(SumByAIDDebit[,1]>=8900 & SumByAIDDebit[,1]<=8990),2]))

#Vector with the calculated numbers from above
Tall <- c(SI, VK, LK, Avskr, Nedskr, AnnenDK, FI, FK, SKord, EkstraI, EkstraK,
          SKekstra, Res, Disp)

#Data frame with the vector with names and numbers
Resultatregnskap <- data.frame(`Resultatregnskap etter art`, Tall, check.names = 'false')


# --------------------
# EIENDELER
# --------------------

#Vector with row names for balance statement assets
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

#Vector with numbers for balance statement assets
StandardAccountID <- c(10:19)

`Eiendeler tall` <- 0 #Placeholder

#Data frame with vectors with row names and numbers
BalanseEiendeler <- data.frame(Eiendeler, StandardAccountID, 
                               `Eiendeler tall`, check.names = 'false')

#Get sum from SumBySAID and insert into BalanseEiendeler
setDT(BalanseEiendeler)[SumBySAIDDebit, `Eiendeler tall` := x, on = .(StandardAccountID)]

#Removes StandardAccountID from data frame
BalanseEiendeler$StandardAccountID <- NULL

# --------------------
# EGENKAPITAL OG GJELD
# --------------------

#Vector with row names for balance statement equity and debt
`Egenkapital og Gjeld` <- c('Egenkapital AS/ASA', 'Avsetning for forpliktelser',
                            'Annen langsiktig gjeld', 
                            'Kortsiktige konvertible lån, obligasjonslån og gjeld til kredittinstitusjoner',
                            'Leverandørgjeld', 'Betalbar skatt', 
                            'Skattetrekk og andre trekk',
                            'Skyldige offentlige avgifter', 'Utbytte',
                            'Annen kortsiktig gjeld')

#Vector with numbers for balance statement equity and debt
StandardAccountID <- c(20:29)

`EKGJ tall` <- 0 #Placeholder

#Data fram with vectors row names and numbers
BalanseEKGJ <- data.frame(`Egenkapital og Gjeld`, StandardAccountID, 
                          `EKGJ tall`, check.names = 'false')

#Get sum from SumBySAID and insert into BalanseEKGJ
setDT(BalanseEKGJ)[SumBySAIDCredit, `EKGJ tall` := x, on = .(StandardAccountID)]

#Removes StandardAccountID from data frame
BalanseEKGJ$StandardAccountID <- NULL

}
}
