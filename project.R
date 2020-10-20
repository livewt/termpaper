library(XML)
library(tidyverse)
library(taRifx)

#making DF from saf-t xml file
doc2 <- xmlParse("SAF-T new example2.xml")

namespace <- xmlNamespaceDefinitions(doc2)[1]
namespace[[1]][1] == "nl"
namespace <- as.character(namespace[[1]][1])

namespace <- paste("//", namespace, sep = "", ":Account")


dataframe <- xmlToDataFrame(nodes = getNodeSet(doc2, namespace))
###

#making df from GeneralLedger specification posted on saf-t git
# "https://github.com/Skatteetaten/saf-t/tree/master/General%20Ledger%20Standard%20Accounts/XML"
GL_accounts_check <- xmlParse("General_Ledger_Standard_Accounts_4_character.xml")
GL_accounts_check_df <- xmlToDataFrame(nodes = getNodeSet(GL_accounts_check, "//AccountID")) %>%
  remove.factors(.)

dataframe_check <- data.frame(dataframe$AccountID) %>%
  remove.factors(.)
#making a chr vector from the skatteetaten GL df
vektor <- character()
for (chr in dataframe_check){
  vektor <- c(vektor, chr)
}
#checking if the saf-t file contains accountIDs similar to the specification from skatteetaten
vektor2 <- character()
vektor3 <- character()
for (chr in vektor){
  ifelse(chr %in% GL_accounts_check_df$text, vektor2 <- c(vektor2, chr), 
         vektor3 <- c(vektor3, chr))
}
#letting the user know what accounts in their SAF-T file, which is not consistent with spesification from skatteetaten
paste("the accountID", vektor3, "is not in the general ledger specification by skatteetaten")

#doing the same thing with StandardAccountIDs
STD_accounts_check <- xmlParse("General_Ledger_Standard_Accounts_2_character.xml")
STD_accounts_check_df <- xmlToDataFrame(nodes = getNodeSet(STD_accounts_check, "//AccountID")) %>%
  remove.factors(.)
dataframe_check_STD <- data.frame(dataframe$StandardAccountID) %>%
  remove.factors(.)
vektor_STD <- character()
for (chr in dataframe_check_STD){
  vektor_STD <- c(vektor_STD, chr)
}
vektor_STD <- vektor_STD %>% unique()
vektor2_STD <- character()
vektor3_STD <- character()
for (chr in vektor_STD){
  ifelse(chr %in% STD_accounts_check_df$text, vektor2_STD <- c(vektor2_STD, chr), 
         vektor3_STD <- c(vektor3_STD, chr))
}
paste("The StandardAccountID", vektor3_STD, "is not in the skatteetaten spesification")

dataframe[dataframe$StandardAccountID==vektor3_STD, ] %>%
  select(AccountID, AccountDescription) 
