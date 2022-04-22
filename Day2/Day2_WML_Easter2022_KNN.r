# initial settings
# Note the installation of the forecast packages takes a while
# library_path <- paste(getwd(), "packages",sep="/")
# dir.create(library_path,showWarnings = FALSE)
# .libPaths(library_path)

if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require(repr)) {
  install.packages("repr")
  library(repr)
}
if (!require(rpart)) {
  install.packages("rpart")
  library(rpart)
}
if (!require(rpart.plot)) {
  install.packages("rpart.plot")
  library(rpart.plot)
}
if (!require(caret)) {
  install.packages("caret")
  library(caret)
}
if (!require(precrec)) {
  install.packages("precrec")
  library(precrec)
}
if (!require(e1071)) {
  install.packages("e1071")
  library(e1071)
}
if (!require(ISLR)) {
  install.packages("ISLR")
  library(ISLR)
}
if (!require(Metrics)) {
  install.packages("Metrics")
  library(Metrics)
}
if (!require(class)) {
  install.packages("RPostgreSQL")
  library(class)
}

# library(tidyverse)
# library(rpart)
# library(rpart.plot)
library(caret)
# library(class)
# library(e1071)

# install.packages('precrec',lib='.', verbose=TRUE)
# library(precrec,lib.loc='.')



# Plot size depending on your screen resolution to 9 x 6
options(repr.plot.width = 9, repr.plot.height = 6)


# load the data
universal.df <- read.csv("./Day2/UniversalBank.csv")
dim(universal.df)
t(t(names(universal.df)))

# partition the data
set.seed(1)
train.index <- sample(row.names(universal.df), 0.6 * dim(universal.df)[1])
valid.index <- setdiff(row.names(universal.df), train.index)
train.df <- universal.df[train.index, -c(1, 5)]
valid.df <- universal.df[valid.index, -c(1, 5)]
t(t(names(train.df)))

# building the new customer data
new.cust <- data.frame(
  Age = 40,
  Experience = 10,
  Income = 84,
  Family = 2,
  CCAvg = 2,
  Education = 2,
  Mortgage = 0,
  Securities.Account = 0,
  CD.Account = 0,
  Online = 1,
  CreditCard = 1
)


# normalize the data
train.norm.df <- train.df[, -8]
valid.norm.df <- valid.df[, -8]

new.cust.norm <- new.cust
norm.values <- preProcess(train.df[, -8], method = c("center", "scale"))
train.norm.df <- predict(norm.values, train.df[, -8])
valid.norm.df <- predict(norm.values, valid.df[, -8])
new.cust.norm <- predict(norm.values, new.cust.norm)

# running the kNN
knn.pred <- class::knn(
  train = train.norm.df,
  test = new.cust.norm,
  cl = train.df$Personal.Loan, k = 1
)
knn.pred

# optimal k
accuracy.df <- data.frame(k = seq(1, 15, 1), overallaccuracy = rep(0, 15))
for (i in 1:15) {
  knn.pred <- class::knn(
    train = train.norm.df,
    test = valid.norm.df,
    cl = train.df$Personal.Loan, k = i
  )
  accuracy.df[i, 2] <- confusionMatrix(
    knn.pred,
    as.factor(valid.df$Personal.Loan)
  )$overall[1]
}

which(accuracy.df[, 2] == max(accuracy.df[, 2]))

# checking the accuracy per level of k built by the code above
accuracy.df

# knn with k = 3
knn.pred <- class::knn(
  train = train.norm.df,
  test = valid.norm.df,
  cl = train.df$Personal.Loan, k = 3
)

confusionMatrix(knn.pred, as.factor(valid.df$Personal.Loan), positive = "1")

# predict new customer with k = 3
knn.pred <- class::knn(
  train = train.norm.df,
  test = new.cust.norm,
  cl = train.df$Personal.Loan, k = 3
)
knn.pred

# 3-way partition
set.seed(1)
train.index <- sample(row.names(universal.df), 0.5 * dim(universal.df)[1])
valid.index <- sample(
  setdiff(row.names(universal.df), train.index),
  0.3 * dim(universal.df)[1]
)
test.index <- setdiff(row.names(universal.df), c(train.index, valid.index))
train.df <- universal.df[train.index, -c(1, 5)]
valid.df <- universal.df[valid.index, -c(1, 5)]
test.df <- universal.df[test.index, -c(1, 5)]

# normalization
train.norm.df <- train.df[, -8]
valid.norm.df <- valid.df[, -8]
test.norm.df <- test.df[, -8]
norm.values <- preProcess(train.df[, -8], method = c("center", "scale"))
train.norm.df <- predict(norm.values, train.df[, -8])
valid.norm.df <- predict(norm.values, valid.df[, -8])
test.norm.df <- predict(norm.values, test.df[, -8])


# predictions on train
knn.predt <- class::knn(
  train = train.norm.df,
  test = train.norm.df,
  cl = train.df$Personal.Loan, k = 3
)
confusionMatrix(knn.predt, as.factor(train.df$Personal.Loan), positive = "1")

# predictions on validation
knn.predv <- class::knn(
  train = train.norm.df,
  test = valid.norm.df,
  cl = train.df$Personal.Loan, k = 3
)

confusionMatrix(knn.predv, as.factor(valid.df$Personal.Loan), positive = "1")

# predictions on test
knn.predtt <- class::knn(
  train = train.norm.df,
  test = test.norm.df,
  cl = train.df$Personal.Loan, k = 3
)
confusionMatrix(knn.predtt, as.factor(test.df$Personal.Loan), positive = "1")
