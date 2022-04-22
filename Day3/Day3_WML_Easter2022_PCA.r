# initial settings
#Note the installation of the forecast packages takes a while
# libary_path <- paste(getwd(), "packages",sep="/")
# dir.create(libary_path,showWarnings = FALSE)
# .libPaths(libary_path)

if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}
if(!require(repr)){
    install.packages("repr")
    library(repr)
}
if(!require(lubridate)){
    install.packages("lubridate")
    library(lubridate)
}
if(!require(janitor)){
    install.packages("janitor")
    library(janitor)
}
if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
}



# Plot size depending on your screen resolution
options(repr.plot.width=6, repr.plot.height=6)

### BEGIN SOLUTION

# (part a): generating 3 simulated data
set.seed(3)
n = 20 # the number of samples per class
p = 50 # the number of variables
# Create data for class 1:
X_1 = matrix( rnorm(n*p), nrow=n, ncol=p )
for( row in 1:n ){ ## +1 to all
  X_1[row,] = X_1[row,] + rep( 1, p )
}
# Create data for class 2:
X_2 = matrix( rnorm(n*p), nrow=n, ncol=p )
for( row in 1:n ){ ## -1 to all
  X_2[row,] = X_2[row,] + rep( -1, p )
}
# Create data for class 3:
X_3 = matrix( rnorm(n*p), nrow=n, ncol=p )
for( row in 1:n ){ ## half +1/half -1
  X_3[row,] = X_3[row,] + c( rep( +1, p/2 ), rep( -1, p/2 ) )
}
X = rbind( X_1, X_2, X_3 )
labels = c( rep(1,n), rep(2,n), rep(3,n))

### END SOLUTION

### BEGIN SOLUTION

# running the pca
pca.out = prcomp(X, center = TRUE, scale. = TRUE) # when running the pca, always activate the argument scale=TRUE to normalise the data
print(pca.out)

### END SOLUTION
# plotting the distribution of the PC's
plot(pca.out)

# displaying the summary results of my PCA analysis
summary(pca.out)

### BEGIN SOLUTION
plot(pca.out$x[,1:2], col=labels, xlab = "PC1", ylab = "PC2", pch = 19) # the variable to be used to colour should be numeric or factor
### END SOLUTION

### BEGIN SOLUTION
set.seed(1)
kmean.out = kmeans(X, centers = 3, nstart = 50)
table(kmean.out$cluster, labels)
### END SOLUTION

# checking the clusters modeled
kmean.out$cluster
kmean.out.4 = kmeans(X, centers = 4, nstart = 50)
kmean.out.4$cluster

# performing the knn in the clusters built by PCA
kmean.out.pca = kmeans(pca.out$x[, 1:2], centers = 3, nstart = 50)
kmean.out.pca$cluster

# print the clustering produced by k-means compared with the ones produced by PCA
table(kmean.out.pca$cluster,labels)

# loading the data
wine.df <- read.csv("./Day3/wine.csv")
head(wine.df)

# checking types of wine
unique(wine.df$Type)

# pca without scale
options(scipen = 999, digits = 2) # to avoid scientific notaiton
pcs.cor <- prcomp(wine.df[,-1])
summary(pcs.cor)

# pca with scale
options(scipen = 999, digits = 2) # to avoid scientific notaiton
pcs.cor <- prcomp(wine.df[,-1], scale. = TRUE)
summary(pcs.cor)

# plotting the distribution of the PC's
plot(pcs.cor)

# plotting the clusters according the PC1 and PC2
plot(pcs.cor$x[,1:2], col = as.factor(wine.df$Type), xlab = "PC1", ylab = "PC2", pch = 19)


