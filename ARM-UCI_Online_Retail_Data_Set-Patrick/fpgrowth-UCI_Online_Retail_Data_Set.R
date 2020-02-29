# Combine two source data frames ----

#dat1<-read.csv("./data/OnlineRetail.csv")
#dat2<-read.csv("./data/OnlineRetail.csv")

#combined <- rbind (dat1, dat2)

#write.csv(combined, "./data/online_retail_II_combined.csv")


# FP-Growth ----
library(rCBA)
library(arules)

data<-read.csv("./data/online_retail_II_combined_clean.csv")

colvec <- c("InvoiceNo","Description")

trans <- arules::read.transactions(
  file = "./data/online_retail_II_combined_clean.csv",
  format = "single",
  header = TRUE,
  sep = ",",
  cols=colvec,
  rm.duplicates = T
)
memory.limit(size = 56000)
data<-read.csv("./data/online_retail_II_combined_clean.csv")
data <- data[ -c(2, 4:8) ]
train <- sapply(data, as.factor)
train <- data.frame(train, check.names=FALSE)
txns <- as(train,"transactions")


association.rules = rCBA::fpgrowth(txns, support = 0.005, confidence = 0.5, maxLength = 20,
         consequent = "Description", verbose = TRUE, parallel = FALSE)


predictions <- rCBA::classification(train,rules)
table(predictions)
sum(as.character(train$Species)==as.character(predictions),na.rm=TRUE)/length(predictions)

prunedRules <- rCBA::pruning(train, rules, method="m2cba", parallel=FALSE)

predictions <- rCBA::classification(train, prunedRules)
table(predictions)
sum(as.character(train$Species)==as.character(predictions),na.rm=TRUE)/length(predictions)

fp.rules <- rCBA::fpgrowth(trans, support = 0.01, confidence = 0.7, minlen = 3, maxlen = 5)
inspect(fp.rules)
