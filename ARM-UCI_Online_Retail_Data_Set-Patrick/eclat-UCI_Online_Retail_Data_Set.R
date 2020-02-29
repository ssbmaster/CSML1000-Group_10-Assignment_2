#libraries
library(arules)
library(rJava)
library(rCBA)
library(arulesViz)

# Reading the data file
mydata<-read.csv("./data/online_retail_II_combined_clean.csv")

#remove 1st column (counting # of items in basket)
#mydata <- mydata[,-1]

#write updated CSV with removed column
#mydata <- write.csv(mydata, "./data/groceriesClean.csv")

#read updated CSV into transactions object
#trans <- read.transactions("./data/groceriesClean.csv", format="single", sep=",", cols=1)

colvec <- c("InvoiceNo","Description")

# read transactions from csv
trans <- read.transactions(
  file = "./data/online_retail_II_combined_clean.csv",
  format = "single",
  header = TRUE,
  sep = ",",
  cols=colvec,
  rm.duplicates = T
)

itemFrequencyPlot(trans,topN=20,type="relative",main="Relative Item Frequency Plot")

#apriori algorithm
#apriori.rules <- apriori(trans, parameter = list(supp=0.01, conf=0.7,minlen = 3, maxlen=5))
#inspect(apriori.rules)

#FP growth
#fp.rules <- rCBA::fpgrowth(trans, support = 0.01, confidence = 0.7, minlen = 3, maxlen = 5)
#inspect(fp.rules)

#ECLAT algorithm
eclat.itemset <- eclat(trans, parameter = list(supp=0.005,maxlen=20))
eclat.rules <- ruleInduction(eclat.itemset, trans, confidence = 0.5)
inspect(eclat.rules)

#plot rules
plot(apriori.rules, method = "graph")
plot(eclat.rules, method = "graph")
