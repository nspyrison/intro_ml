# initial settings
#Note the installation of the forecast packages takes a while
# library_path <- paste(getwd(), "packages",sep="/")
# dir.create(library_path,showWarnings = FALSE)
# .libPaths(library_path)

if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}
if(!require(repr)){
    install.packages("repr")
    library(repr)
}
if(!require(rpart)){
    install.packages("rpart")
    library(rpart)
}
if(!require(rpart.plot)){
    install.packages("rpart.plot")
    library(rpart.plot)
}
if(!require(caret)){
    install.packages("caret")
    library(caret)
}
if(!require(precrec)){
    install.packages("precrec")
    library(precrec)
}
if(!require(e1071)){
    install.packages("e1071")
    library(e1071)
}
if(!require(MASS)){
    install.packages("MASS")
    library(MASS)
}
if(!require(glmulti)){
    install.packages("glmulti")
    library(glmulti)
}



# Plot size depending on your screen resolution to 6 x 9
options(repr.plot.width=9, repr.plot.height=6)


#load the data
ebay.df <- read.csv("./Day2/ebayAuctions.csv", stringsAsFactors = FALSE)
head(ebay.df)

data.frame(tapply(ebay.df$Competitive., ebay.df$Category, mean))
data.frame(tapply(ebay.df$Competitive., ebay.df$currency, mean))
data.frame(tapply(ebay.df$Competitive., ebay.df$endDay, mean))
data.frame(tapply(ebay.df$Competitive., ebay.df$Duration, mean))

names(ebay.df)
ebay.df$endDay[ebay.df$endDay == "Sun"] <- "Sun_Fri"
ebay.df$endDay[ebay.df$endDay == "Fri"] <- "Sun_Fri"
ebay.df$Category[ebay.df$Category == "Business/Industrial"] <- "Computer"
ebay.df$Category[ebay.df$Category == "Antique/Art/Craft"] <- "Collectibles"


head(ebay.df)

#data partition
ntotal <- length(ebay.df$Competitive.)
ntrain <- round(ntotal * 0.6)
nvalid <- ntotal - ntrain
set.seed(202)  # Seed of random number generator for reproducibility.
ntrain.index <- sort(sample(ntotal, ntrain))  # Sample rows randomly.
train.df <- ebay.df[ntrain.index, ]
valid.df <- ebay.df[-ntrain.index, ]
options(scipen = 999, digits = 4)

#logistic regression using all the variables
reg <- glm(Competitive. ~ ., data = train.df, family = "binomial")
summary(reg)


# predictions on validation data
pred <- predict(reg, newdata = valid.df, type = "response")
head(pred)

# confusion matrix
confusionMatrix(factor(ifelse(pred > 0.5, 1, 0)),
                factor(valid.df$Competitive.), positive = "1")

#logistic regression excluding closing price variable
reg <- glm(Competitive. ~ ., data = train.df[-6], family = "binomial")

#predictions on validation data
pred <- predict(reg, newdata = valid.df, type = "response")
head(pred)

#confusion matrix
confusionMatrix(factor(ifelse(pred > 0.5, 1, 0)),
                factor(valid.df$Competitive.), positive = "1")

### BEGIN SOLUTION

# stepwise logistic regression using the function stepAIC() from the package MASS
library(MASS)

stepwise <- stepAIC(reg, direction = "both")
summary(stepwise)

### END SOLUTION

#logistic regresssion using the selection from the stepwise process
reg_stepwise <- glm(Competitive. ~ Category + currency + sellerRating + Duration + endDay,
                         data = train.df[-6], family = "binomial")


summary(reg_stepwise)

# predictive error rate using stepwise selection (training set)
pred <- predict(reg_stepwise, data = train.df, type = "response")

#confusion matrix
confusionMatrix(factor(ifelse(pred > 0.5, 1, 0)),
                factor(train.df$Competitive.), positive = "1")

#predictive error rate using stepwise selection (Validation set)
pred <- predict(reg_stepwise, newdata = valid.df, type = "response")

#confusion matrix
confusionMatrix(factor(ifelse(pred > 0.5, 1, 0)),
                factor(valid.df$Competitive.), positive = "1")



