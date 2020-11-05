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

####Financial figure calculations:
##Opening balance figures:
#Current ratio
Open_Current_ratio <- 
  (
  Open_asset_func(14)+
  Open_asset_func(15)+
  Open_asset_func(16)+
  Open_asset_func(17)+
  Open_asset_func(18)+
  Open_asset_func(19)
  )/(
  Open_credit_func(23)+
  Open_credit_func(24)+
  Open_credit_func(25)+
  Open_credit_func(26)+
  Open_credit_func(27)+
  Open_credit_func(28)+
  Open_credit_func(29)
  )



##closing balance financial ratios
#current ratio
Close_Current_ratio <- 
  (
    Close_asset_func(14)+
      Close_asset_func(15)+
      Close_asset_func(16)+
      Close_asset_func(17)+
      Close_asset_func(18)+
      Close_asset_func(19)
  )/(
    Close_credit_func(23)+
      Close_credit_func(24)+
      Close_credit_func(25)+
      Close_credit_func(26)+
      Close_credit_func(27)+
      Close_credit_func(28)+
      Close_credit_func(29)
  )
class(main_df$ClosingDebitBalance
      )
