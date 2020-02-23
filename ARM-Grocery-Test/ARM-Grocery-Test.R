#libraries
library(arules)

# Reading the data file
mydata<-read.csv("./data/groceriesNOMILK.csv",header=T)

#remove 1st column (counting # of items in basket)
mydata <- mydata[,-1]

#write updated CSV with removed column
mydata <- write.csv(mydata, "./data/groceriesClean.csv")

#read updated CSV into transactions object
trans <- read.transactions("./data/groceriesClean.csv", format="basket", sep=",", cols=1)

itemFrequencyPlot(trans,topN=20,type="relative",main="Relative Item Frequency Plot")

association.rules <- apriori(trans, parameter = list(supp=0.001, conf=0.8,maxlen=3))

inspect(association.rules)