# Combine two source data frames ----

#dat1<-read.csv("../data/OnlineRetail.csv")
#dat2<-read.csv("../data/OnlineRetail.csv")

#combined <- rbind (dat1, dat2)

#write.csv(combined, "../data/online_retail_II_combined.csv")


# FP-Growth ----
library(rCBA)
library(arules)

data<-read.csv("../data/online_retail_II_combined_clean.csv")

# colvec <- c("X","InvoiceNo","Description")
# 
# trans <- arules::read.transactions(
#   file = "../data/online_retail_II_combined_clean.csv",
#   format = "basket",
#   header = TRUE,
#   sep = ",",
#   cols=colvec,
#   rm.duplicates = T
# )


colvec <- c("InvoiceNo","Description")

trans <- arules::read.transactions(
  file = "../data/online_retail_II_combined_clean.csv",
  format = "single",
  header = TRUE,
  sep = ",",
  cols=colvec,
  rm.duplicates = T
)

memory.limit(size = 56000)
data<-read.csv("../data/online_retail_II_combined_clean.csv")
data_cols_removed <- data[ -c(1, 2, 4, 6:10) ]
train <- sapply(data_cols_removed, as.factor)
train <- data.frame(train, check.names=FALSE)

#remove duplicates from train
train_no_duplicates = train[!duplicated(train), ]

#including this because allocation vector size is WAY TOO BIG
#samp <- sample(nrow(train),10000) #500000 rows
#txns <- as(train[samp,],"transactions")
txns <- as(train,"transactions")

#fpgroowth with trans
# association.rules = rCBA::fpgrowth(trans, support = 0.005, confidence = 0.5, maxLength = 20,
#          consequent = "Description", verbose = TRUE, parallel = FALSE)

#fpgroowth with txns
association.rules = rCBA::fpgrowth(txns, support = 0.005, confidence = 0.5, maxLength = 20,
          consequent = "Description", verbose = TRUE, parallel = FALSE)


Tickepredictions <- rCBA::classification(train,rules)
table(predictions)
sum(as.character(train$Species)==as.character(predictions),na.rm=TRUE)/length(predictions)

prunedRules <- rCBA::pruning(train, rules, method="m2cba", parallel=FALSE)

predictions <- rCBA::classification(train, prunedRules)
table(predictions)
sum(as.character(train$Species)==as.character(predictions),na.rm=TRUE)/length(predictions)

fp.rules <- rCBA::fpgrowth(trans, support = 0.01, confidence = 0.7, minlen = 3, maxlen = 5)
inspect(fp.rules)
