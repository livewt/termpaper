library(XML)
library(tidyverse)
library(taRifx)
library(bit64)
#for example 3yrs old
choose_file <- choose.files(caption ="Select your SAF-T file (xml format)")

#making DF from saf-t xml file

main <- xmlParse(choose_file)
main <- xmlParse("saf-t example (3yrs old).xml") #placeholder to skip choosing manually
#main <- (choose_file)

list <- 
  xmlToList(main)


main_list <- xmlToList(main)


#as.data.frame(list[[3]][[4]][[4]])

test <- as.data.frame(list[[3]][[4]][[4]])
test <- list[[3]][[4]] %>%
  map_df(~as.data.frame(.))
#test %>%
 # select(Period, test[ ,grepl("Line.Amount", names(test))])

sum <- test[ ,grepl("Line.Amount", names(test))]
sum <- sum %>%
  map_df(as.numeric)
sum[is.na(sum)] <- 0


sum <- sum %>%
  mutate(Sum = rowSums(.)) %>%
  mutate(Amounts = Sum/2)
test <- cbind(test, sum$Amounts)

mva_test <- test%>%
  select(Description, TransactionID,`sum$Amounts`)

#mva_test <- mva_test %>%
 # drop_na(Line.TaxInformation.TaxType)
mva_test <- mva_test %>%
 drop_na(TransactionID)
mva_test2 <- mva_test$`sum$Amounts` %>%
  as.numeric()
mva_test2 <- as.data.frame(mva_test2)
mva_test2$mva_test22 <- mva_test2$mva_test2

mva_test %>%
  head
#For colouring purposes?
wssplot <- function(data, nc=15, seed=1234){ #function found online!!!
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(mva_test2)
cluster_amount <- as.numeric(readline(prompt = "How many clusters do you want? "))
KM = kmeans(mva_test2, cluster_amount)
KM$betweenss
KM$centers
#cluster <- KM$cluster
#autoplot(KM,mva_test2, frame=TRUE)

#install.packages("ggiraph")
library(ggiraph)
mva_test$cluster <- as.character(KM$cluster)
#mva_test$Line.Amount.1 <- as.numeric(mva_test$Line.Amount.1)
mva_test$TransactionID <- as.numeric(mva_test$TransactionID)

tooltip_ <- c(paste0("Description: ", mva_test$Description, "\n Transaction ID: = ", mva_test$TransactionID, "\n Amount: ", mva_test$`sum$Amounts`, " NOK"))

testplot <- ggplot(data = mva_test) +
  geom_point_interactive(aes(x = 1:length(`sum$Amounts`), y = `sum$Amounts`, color = cluster,
                             tooltip = tooltip_, data_id = TransactionID))+
  ylab("Transcation amount")+
  xlab("Transcations")+
  ggtitle("Hover over points to view description of the transcation")+
scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)


testplot
#girafe(code = print(testplot))
girafe(ggobj = testplot)


#check if all transcations are included (53 in this case)
as.numeric(list[[3]][[1]]) == length(mva_test$`sum$Amounts`)
#check if sum of transcations is right
sum(mva_test$`sum$Amounts`)== as.numeric(list[[3]][[2]])
