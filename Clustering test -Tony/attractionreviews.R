# Load libraries
library(tidyverse)
library(mclust)

# Read the data
initialData <- as_tibble(read.csv('google_review_ratings.csv', header = TRUE, na.strings = c('NA','','#NA')))

# Explore the data a bit
summary(initialData)
head(initialData)
tail(initialData)

# Rename and remove bad columns
initialData <- initialData[, -26]
names(initialData) <- c('user', 'churches', 'resorts', 'beaches', 'parks', 'theatres', 'museums',
                        'malls', 'zoos', 'restaurants', 'pubs', 'services', 'burgers', 'lodgings',
                        'juicebars', 'galleries', 'clubs', 'pools', 'gyms', 'bakeries', 'spas',
                        'cafes', 'viewpoints', 'monuments', 'gardens'
                        )
initialData <- initialData[, -12]

# We can now remove any records that have NAs
myDataClean <- na.omit(initialData)
summary(myDataClean)
xOnlyData <- myDataClean[, -1]

# Let us apply kmeans for k=3 clusters 
kmm = kmeans(xOnlyData, 6, nstart = 50, iter.max = 15) 
# We keep number of iter.max=15 to ensure the algorithm converges and nstart=50 to 
# Ensure that atleat 50 random sets are choosen  
kmm

# Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 30
wss <- sapply(1:k.max, function(k){kmeans(xOnlyData, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Bayesian Inference Criterion for k means to validate choice from Elbow Method
d_clust <- Mclust(as.matrix(xOnlyData), G=1:30, 
                  modelNames = mclust.options("emModelNames"))
d_clust$BIC
plot(d_clust)

# 30 indices to find the best one
library(NbClust)
nb <- NbClust(xOnlyData, diss=NULL, distance = "euclidean", 
              min.nc=5, max.nc=23, method = "kmeans", 
              index = "all", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

