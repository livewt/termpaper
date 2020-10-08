library(XML)


doc2 <- xmlParse("fictitious_toy.xml")
dataframe <- xmlToDataFrame(nodes = getNodeSet(doc2, "//n1:Account"))
dataframe[is.na(dataframe)] <- 0

