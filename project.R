library(XML)
library(tidyverse)
library(taRifx)

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
test <- main_df %>% subset(StandardAccountID == c("10","11","12"))%>%
  summarise(OpeningDebitBalance,ClosingDebitBalance)


main_df %>% subset(StandardAccountID == "11") %>%
  summarise(OpeningDebitBalance, ClosingDebitBalance)











