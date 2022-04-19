# initial settings

# setting the working directory
#setwd("~/Desktop/ML_Workshop/Codes") # adjust for your directory path

# Note the installation of the forecast packages takes a while
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
if(!require(forecast)){
    install.packages("forecast",verbose=TRUE)
    library(forecast)
}
if(!require(lubridate)){
    install.packages("lubridate")
    library(lubridate)
}
if(!require(ggrepel)){
    install.packages("ggrepel")
    library(ggrepel)
}

# Plot size deppening on your screen resolution to 5 x 5
options(repr.plot.width=5, repr.plot.height=5)


# turning off the warnings
options(warn=-1)
#options(warn=0) # to turn warnings back on

#::::::::::::::::::::::::::::::::
# Exercise 1: Slide Example
#::::::::::::::::::::::::::::::::

car.df <- read.csv("Day1/ToyotaCorolla.csv")
# use first 1000 rows of data
car.df <- car.df[1:1000, ]
# select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)


# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)

library(forecast)
# use predict() to make predictions on a new set.
car.lm.pred <- predict(car.lm, valid.df)
options(scipen=999, digits = 0)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
    "Residual" = some.residuals)
options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(car.lm.pred, valid.df$Price)

car.lm.pred <- predict(car.lm, valid.df)
residuals <- valid.df$Price - car.lm.pred
residuals.df <- data.frame(residuals)
ggplot(residuals.df, aes(x=residuals)) +
  geom_histogram()


# adjusting the plotting size
options(repr.plot.width=13, repr.plot.height=8)

# Using the plot() function to produce diagnostic plots of the linear regression fit
par(mfcol=c(2,2))
plot(car.lm)

#::::::::::::::::::::::::::::::::::::::::::::
# Exercise 2: With the support of plotting
#::::::::::::::::::::::::::::::::::::::::::::

### BEGIN SOLUTION
econ.df <- read.table("./Day1/EconDataPop.csv", header=TRUE, sep =",")
econ.df %>% head()
### END SOLUTION

# Plot size deppening on your screen resolution to 5 x 5
options(repr.plot.width=5, repr.plot.height=5)

### BEGIN SOLUTION
ggplot(data = econ.df,
       aes(x = CPI,
           y = HDI))+
  geom_point()
### END SOLUTION

### BEGIN SOLUTION
p <- ggplot(data = econ.df,
       aes(x      = CPI,
           y      = HDI,
           colour = Region,
           size   = Population))+
    geom_point ()
p
### END SOLUTION

### BEGIN SOLUTION
p<-ggplot(data = econ.df,
       aes(x      = CPI,
           y      = HDI,
           colour = Region,
           size   = Population))+
    geom_point (alpha=0.5) +
    scale_size(range = c(0,15))
p
### END SOLUTION

reg <- lm(HDI ~ CPI, data=econ.df)
reg

# or for more complete results
summary(reg)

### BEGIN SOLUTION
econ.df$linearFitHDI=predict(reg)
head(econ.df)
### END SOLUTION

### BEGIN SOLUTION
p<-ggplot(data = econ.df,
       aes(x      = CPI,
           y      = HDI,
           colour = Region,
           size   = Population)) +
    geom_point (alpha=0.5) +
    scale_size(range = c(0,15)) +
    geom_line(aes(y      = linearFitHDI,
                  colour = NULL,
                  size   = NULL))
p
### END SOLUTION

### BEGIN SOLUTION
ggplot(data = econ.df,
       aes(x      = CPI,
           y      = HDI)) +
    geom_point (alpha=0.5,
       aes(colour = Region,
           size   = Population)) +
    scale_size(range = c(0,15)) +
    geom_line(aes(y = linearFitHDI))
### END SOLUTION

### BEGIN SOLUTION
ggplot(data = econ.df,
       aes(x      = CPI,
           y      = HDI)) +
    geom_point (alpha=0.5,
               aes(colour = Region,
                   size   = Population)) +
    scale_size(range = c(0,15)) +
    geom_smooth(method = 'loess')
### END SOLUTION

### BEGIN SOLUTION
ggplot(data = econ.df,
       aes(x      = CPI,
           y      = HDI)) +
    geom_point (alpha=0.5,
       aes(colour = Region,
           size   = Population)) +
    scale_size(range = c(0,15)) +
    geom_smooth(method = 'lm', se=FALSE)
### END SOLUTION

### BEGIN SOLUTION
ggplot(data = econ.df,
       aes(x      = CPI,
           y      = HDI)) +
    geom_point (alpha=0.5,
       aes(colour = Region,
           size   = Population)) +
    scale_size(range = c(0,15)) +
    geom_smooth(method   = 'lm',
                se       = FALSE,
                formula = y~log(x))
### END SOLUTION

#::::::::::::::::::::::::::::::::::::::
# Exercise 3: Selecting the best model
#::::::::::::::::::::::::::::::::::::::

#install.packages("leaps")
#install.packages("gains")
#install.packages("forecast")
#install.packages("corrgram")
library(leaps) #for subset selection
library(gains) #for gains and lift chart
library(forecast) #for accuracy measures
library(corrgram) #for producing a graphical display of a correlation matrix

# load the data and preprocess
housing.df <- read.csv("./Day1/BostonHousing.csv")
head(housing.df)
t(names(housing.df))

# remove the categorical response variable CAT..MEDV
housing.df <- housing.df[,-c(14)]
t(names(housing.df))

# fit the model
reg <- lm(MEDV ~ CRIM + CHAS + RM, data = housing.df)
summary(reg)

#median price for the new tract
print(reg$coef %*% c(1, 0.1, 0, 6))

# the prediction error of the new data is obtained by setting se.fit = TRUE in the predict() function
new <- data.frame(CHAS = 0, CRIM = 0.1, RM = 6)
pred <- predict(reg, newdata = new, se.fit = TRUE)
pred$fit

# as shown above, the median house price is therefore $20,832.32.

# prediction error
pred$se.fit # prediction error is $334.8071

cor(housing.df[,c("CRIM","CHAS","RM")], housing.df)
cor(housing.df[,c("INDUS", "NOX", "TAX")])

#correlation table for the 12 numerical predictors
(t(names(housing.df)))
cor(housing.df[,-c(4)])

#library(leaps)

# partition data
set.seed(1)
train.index <- sample(c(1:dim(housing.df)[1]),
                      0.6*dim(housing.df)[1])
valid.index <- setdiff(c(1:dim(housing.df)[1]), train.index)
train.df <- housing.df[train.index, ]
valid.df <- housing.df[valid.index, ]

# stepwise regression with exhaustive search
search <- regsubsets(MEDV ~ .,
                     data = housing.df,
                     nbest = 1,
                     nvmax = dim(train.df)[2],
                     method = "exhaustive")



sum <- summary(search)
sum$which

t(t(sum$adjr2))
# top 3 models
models <- order(sum$adjr2, decreasing = T)[1:3]
models


library(gains) #for gains and lift chart

# Plot size deppening on your screen resolution to 5 x 3
options(repr.plot.width=12, repr.plot.height=5)

# run model on training and validation
par(mfcol=c(1,3))
for (model in models){
  print(model) #print model number
  selected.vars = names(train.df)[sum$which[model,]]
  reg.model <- lm(MEDV ~ ., data = train.df[,selected.vars])

  # training
  print(accuracy(reg.model$fitted.values, train.df$MEDV)[2]) #print RMSE
  print(accuracy(reg.model$fitted.values, train.df$MEDV)[5]) #print MAPE

  # validation
  pred <- predict(reg.model, valid.df)
  print(accuracy(pred, valid.df$MEDV)[2]) #print RMSE
  print(accuracy(pred, valid.df$MEDV)[5]) #print MAPE

  #lift charts
  gain <- gains(valid.df$MEDV, pred)
  plot(c(0, gain$cume.pct.of.total*sum(valid.df$CRIM)) ~
         c(0, gain$cume.obs),
       xlab="# cases", ylab="Cumulative", main="", type="l")
}

library(stats)

# fit the model with all the variables first.
reg2 <- lm(MEDV ~ ., data = train.df)

# backward regression
step.backward <- step(reg2, direction = "backward")

summary(step.backward)

# predictions on validation set
lm1 <- lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO
          + LSTAT, data = train.df)

pred1.valid <- predict(lm1, data = valid.df)

accuracy(pred1.valid, valid.df$MEDV)

lm0 <- lm(MEDV ~ ., data = train.df)
pred0.valid <- predict(lm0, data = valid.df)

accuracy(pred0.valid, valid.df$MEDV)

# forward regression
step.forward <- step(reg2, direction = "forward")
summary(step.forward)


# predictions on validation set
lm2 <- lm(MEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD +
            TAX + PTRATIO + LSTAT, data = train.df)
pred2.valid <- predict(lm2, data = valid.df)
accuracy(pred2.valid, valid.df$MEDV)

accuracy(pred1.valid, valid.df$MEDV)

# stepwise regression in both the directions.
step.both <- step(reg2, direction = "both")
summary(step.both)

# predictions on validation set
lm3 <- lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO +
            LSTAT, data = train.df)
pred3.valid <- predict(lm3, data = valid.df)
accuracy(pred3.valid, valid.df$MEDV)

accuracy(pred2.valid, valid.df$MEDV)
accuracy(pred1.valid, valid.df$MEDV)


