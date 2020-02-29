#libraries
library(arules)
library(rJava)
library(rCBA)
library(arulesViz)

# Reading the data file
mydata<-read.csv("../data/online_retail_II_combined.csv")
mydata <- mydata[,-1]
mydata <- mydata[-grep("C", mydata$InvoiceNo),] 
mydata <- mydata[-grep("A", mydata$InvoiceNo),]
mydata <- mydata[!(mydata$Description == ""),]
mydata <- mydata[!(mydata$Description == "DOTCOM POSTAGE"),]

#remove 1st column (counting # of items in basket)
#mydata <- mydata[,-1]

#write updated CSV with cleaned InvoiceNo column
mydata <- write.csv(mydata, "../data/OnlineRetail_II_combined.csv")

#read updated CSV into transactions object
#trans <- read.transactions("../data/groceriesClean.csv", format="single", sep=",", cols=1)

colvec <- c("InvoiceNo","Description")

# read transactions from csv
trans <- read.transactions(
  file = "../data/OnlineRetail_II_combined.csv",
  format = "single",
  header = TRUE,
  sep = ",",
  cols=colvec,
  rm.duplicates = T
)

itemFrequencyPlot(trans,topN=20,type="absolute",main="Item Frequency Plot")

#apriori algorithm
ap_start <- Sys.time()
apriori.rules <- apriori(trans, parameter = list(supp=0.005, conf=0.5,maxlen=20))
apriori.rules_df<- data.frame(lhs = labels(lhs(apriori.rules)), rhs = labels(rhs(apriori.rules)), apriori.rules@quality)
ap_end <- Sys.time()
ap_time <- as.numeric(ap_end - ap_start)
head(apriori.rules_df)
inspect(apriori.rules)


#FP growth
#fp.rules <- rCBA::fpgrowth(trans, support = 0.01, confidence = 0.7, maxLength = 5, consequent = "Description")
#inspect(fp.rules)

#ECLAT algorithm
  eclat_start <- Sys.time()
  eclat.itemset <- eclat(trans, parameter = list(supp=0.005,maxlen=20))
  eclat.rules <- ruleInduction(eclat.itemset, trans, confidence = 0.5)
  eclat_end <- Sys.time()
  eclat_time <- as.numeric(eclat_end - eclat_start)
eclat.rules_df<- data.frame(lhs = labels(lhs(eclat.rules)), rhs = labels(rhs(eclat.rules)), eclat.rules@quality)
head(eclat.rules_df)

plot(apriori.rules, method = "scatterplot")
#plot rules
#plot(apriori.rules, measure = c("support", "lift"), shading = "confidence", interactive = TRUE)
plot(apriori.rules, method = "scatterplot", main = "Apriori Rules")
plot(eclat.rules, method = "graph")

