# Combine two data frames ----

dat1<-read.csv("./data/OnlineRetail.csv")
dat2<-read.csv("./data/OnlineRetail.csv")

combined <- rbind (dat1, dat2)

write.csv(combined, "./data/online_retail_II_combined.csv")


# aPriori ---------------------------------------------------------------------

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

association.rules <- apriori(trans, parameter = list(supp=0.01, conf=0.7,maxlen=100))

inspect(association.rules)


# FP-Growth ---------------------------------------------------------------------

library(rCBA)

mydata<-read.csv("./data/OnlineRetail.csv")

fpgrowth(train, support = 0.01, confidence = 1, maxLength = 5, consequent = NULL, verbose = TRUE, parallel = TRUE)

