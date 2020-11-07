library(XML)
library(tidyverse)
library(taRifx)
library(bit64)

#making DF from saf-t xml file
main <- xmlParse("SAF-T Telenor 2019 (fictious).xml")

namespace <- xmlNamespaceDefinitions(main)[1]
namespace[[1]][1] == "nl"
namespace <- as.character(namespace[[1]][1])

namespace <- paste("//", namespace, sep = "", ":Account")


main_df <- xmlToDataFrame(nodes = getNodeSet(main, namespace))
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
#Problem that it chops off decimals and does not round numbers up if example number is 10.6
charToInt64 <- function(s){
  stopifnot( is.character(s) )
  s <- s %>%
    strsplit("\\D") #splits decimals e.g "0.0" to "0" "0" and returns list
  x <- character()  
  for (i in s){
    x <- c(x, i[1]) #extracting only first element of list. E.g only "0" of the "0" "0" vector
  }
  x <- as.integer64(x) #converts the char to integer 64 bit version
  x
}


#test
testvektor <- c("0.0", "0", "10", "20000000000", "0.011", "1.1", "4,5")
charToInt64(testvektor)




#finally no bugs with numbers
#unfinshed code to start working on calculating fincancial numbers
#Converting chr to integer64 for all balances
main_df$OpeningDebitBalance <- charToInt64(main_df$OpeningDebitBalance)
main_df$OpeningCreditBalance <- charToInt64(main_df$OpeningCreditBalance)
main_df$ClosingDebitBalance <- charToInt64(main_df$ClosingDebitBalance)
main_df$ClosingCreditBalance <- charToInt64(main_df$ClosingCreditBalance)

#Functions to make financial figures calculations!
#Function to subset main_df based on standard account ID for Opening balance for assets (IDs 10-19)
Open_asset_func <- function (std_id){
  a <- subset(main_df, main_df$StandardAccountID==std_id) #subsetting standard acc ID
  rownames(a) <- NULL #resetting rows
  b <- sum(a$OpeningDebitBalance) - sum(a$OpeningCreditBalance)
  b
}
#same but with closing blanace
Close_asset_func <- function (std_id){
  a <- subset(main_df, main_df$StandardAccountID==std_id) #subsetting standard acc ID
  rownames(a) <- NULL #resetting rows
  b <- sum(a$ClosingDebitBalance) - sum(a$ClosingCreditBalance)
  b
}
#Function to subset main_df on standard acc IDs for opening balance for equity, liabilites and income/loss
Open_credit_func <- function (std_id){
  a <- subset(main_df, main_df$StandardAccountID==std_id) #subsetting standard acc ID
  rownames(a) <- NULL #resetting rows
  b <- sum(a$OpeningCreditBalance) - sum(a$OpeningDebitBalance)
  b
}
#Same with closing balance
Close_credit_func <- function (std_id){
  a <- subset(main_df, main_df$StandardAccountID==std_id) #subsetting standard acc ID
  rownames(a) <- NULL #resetting rows
  b <- sum(a$ClosingCreditBalance) - sum(a$ClosingDebitBalance)
  b
}


#Function to sum accounts with different account IDS for Opening balance IDs 10-19 (Assets)
Sum_Open_asset_func <- function(start, end){
  x <- integer64()
  for (i in start:end){
    x <- c(x, Open_asset_func(i)) #uses the subset function above
  }
  x <- sum(x)
  x
}
#same but for closing balance
Sum_Close_asset_func <- function(start, end){
  x <- integer64()
  for (i in start:end){
    x <- c(x, Close_asset_func(i)) #uses the subset function above
  }
  x <- sum(x)
  x
}

#function to sum all OPENING balance with ID from 20 and beyond for equity,liability and income/loss
Sum_Open_credit_func <- function(start, end){
  x <- integer64()
  for (i in start:end){
    x <- c(x, Open_credit_func(i)) #uses the subset function above
  }
  x <- sum(x)
  x
}
#Same but for CLOSING balance for ids 20 and beyond
Sum_Close_credit_func <- function(start, end){
  x <- integer64()
  for (i in start:end){
    x <- c(x, Close_credit_func(i)) #uses the subset function above
  }
  x <- sum(x)
  x
}


### FINANCIAL RATIOS ###
#Remember to always switch - to + in equation since our functions already treats costs(debit) as -
##Opening balances

#Current ratio for opening balance (Likviditetsgrad 1)
Open_Current_ratio <- 
  Sum_Open_asset_func(14,19)/
  Sum_Open_credit_func(23,29)

#Acid test for opening balance (Likviditetsgrad 2)
Open_Acid_test <-
  Sum_Open_asset_func(15,19)/
  Sum_Open_credit_func(23,29)

#Gross profit % (Bruttofortjeneste i %)
Open_GrossProfit_percent <- # + since our function already treats costs as -
  (Sum_Open_credit_func(30,37) + Sum_Open_credit_func(40,49))/
  Sum_Open_credit_func(30,37)
#Gross profit (Bruttofortjeneste)
Open_GrossProfit <- # + since our function already treats costs as -
  (Sum_Open_credit_func(30,37) + Sum_Open_credit_func(40,49))
#Operating margin (Driftsmargin i %)
Open_Operating_margin <-
  (Sum_Open_credit_func(30,39)+ Sum_Open_credit_func(40,79))/
  Sum_Open_credit_func(30,39)
#Profit margin 1? (Resultatgrad i %)
Open_profit_margin1 <-
  (Sum_Open_credit_func(30,39) + Sum_Open_credit_func(40,79)+Open_credit_func(80))/
  Sum_Open_credit_func(30,39)
#Profit margin 2? (Resultatmargin i %)
Open_profit_margin2 <- #- since here we add tax and special costs to acc 88 to get pretax result
  (Open_credit_func(88) - Sum_Open_credit_func(83,85))/
  Sum_Open_credit_func(30,39)
#Wages / sale income (Lønnskostnader i % av salgsinntekt)
Open_wages_sale_inc <-
  (Sum_Open_credit_func(50,59)*-1)/ # * -1 since costs is negative..
  Sum_Open_credit_func(30,32)





##Closing balances

#Current ratio for closing balance (Likviditetsgrad 1)
Close_Current_ratio <-
  Sum_Close_asset_func(14,19)/
  Sum_Close_credit_func(23,29)

#Acid test for closing balance (Likviditetsgrad 2)
Close_Acid_test <-
  Sum_Close_asset_func(15,19)/
  Sum_Close_credit_func(23,29)
