#libraries
library(arules)

# Reading the data file
mydata<-read.csv("./data/OnlineRetail.csv")

#remove 1st column (counting # of items in basket)
#mydata <- mydata[,-1]

#write updated CSV with removed column
#mydata <- write.csv(mydata, "./data/groceriesClean.csv")

#read updated CSV into transactions object
#trans <- read.transactions("./data/groceriesClean.csv", format="single", sep=",", cols=1)

colvec <- c("InvoiceNo","Description")

# read transactions from csv
trans <- read.transactions(
  file = "./data/OnlineRetail.csv",
  format = "single",
  header = TRUE,
  sep = ",",
  cols=colvec,
  rm.duplicates = T
)

itemFrequencyPlot(trans,topN=20,type="relative",main="Relative Item Frequency Plot")

association.rules <- apriori(trans, parameter = list(supp=0.01, conf=0.7,maxlen=15))

inspect(association.rules)
