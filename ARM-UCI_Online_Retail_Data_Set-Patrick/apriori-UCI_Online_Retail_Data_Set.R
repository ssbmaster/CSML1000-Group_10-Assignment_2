# Combine two data frames ----

#dat1<-read.csv("./data/OnlineRetail.csv")
#dat2<-read.csv("./data/OnlineRetail.csv")

#combined <- rbind (dat1, dat2)

#write.csv(combined, "./data/online_retail_II_combined.csv")


# aPriori ---------------------------------------------------------------------

#libraries
library(arules)

# Reading the data file
#mydata<-read.csv("./data/OnlineRetail.csv")
#mydata<-read.csv("./data/online_retail_II_combined.csv")
#mydata <- mydata[-grep("C", mydata$InvoiceNo),] 
#mydata <- mydata[-grep("A", mydata$InvoiceNo),]
#mydata <- mydata[!(mydata$Description == ""),]
#mydata <- mydata[!(mydata$Description == "DOTCOM POSTAGE"),]

#write.csv(mydata, "./data/online_retail_II_combined_clean.csv")

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

itemFrequencyPlot(trans,topN=20,type="absolute",main="Absolute Item Frequency Plot")

association.rules <- apriori(trans, parameter = list(supp=0.005, conf=0.5,maxlen=20))

inspect(association.rules)



#generate sample shopping cart
cart <- c("CHARLOTTE BAG SUKI DESIGN",                                                                                           
  "PACK OF 72 RETROSPOT CAKE CASES",                                                                                     
  "RED RETROSPOT CHARLOTTE BAG",                                                                                         
  "REGENCY CAKESTAND 3 TIER",                                                                                            
  "WOODLAND CHARLOTTE BAG")

#cart <- as(list(cart), "itemMatrix")

#generaet a subset of the full list of rules, where the lhs matches an exact subset of the cart and the rhs
#does not match an exact subset of the cart (rhs is not already in cart)
rules.sub <- subset(association.rules, subset = lhs %oin% cart & !(rhs %oin% cart))
#rules.sub <- is.subset(association.rules, cart, proper = FALSE)

#shows subset of rules matching cart
inspect(rules.sub)

#converts subset of rules to data frame, separating out lhs and rhs
recom <- DATAFRAME(rules.sub, separate=TRUE)

#prints out only rhs (will be used to generate recommendation)
print(recom$RHS, max.levels = 0)

#export rules data to file
save(association.rules, file="rules.RData")
