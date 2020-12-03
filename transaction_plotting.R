library(XML)
library(tidyverse)
library(taRifx)
library(bit64)
#for example 3yrs old
choose_file <- choose.files(caption ="Select your SAF-T file (xml format)")



main <- xmlParse(choose_file)
main <- xmlParse("saf-t example (3yrs old).xml") #placeholder to skip choosing manually
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
  geom_point_interactive(aes(x = TransactionDate, y = `trans_sum$Amounts`,
                             tooltip = tooltip_, data_id = TransactionID))+
  ylab("Transcation amount")+
  xlab("Transcations")+
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
