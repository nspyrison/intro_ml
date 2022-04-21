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
if(!require(tree)){
    install.packages("tree")
    library(tree)
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
if(!require(ISLR)){
    install.packages("ISLR")
    library(ISLR)
}
if(!require(Metrics)){
    install.packages("Metrics")
    library(Metrics)
}
if(!require(RPostgreSQL)){
    install.packages("RPostgreSQL")
    library(RPostgreSQL)
}




# Plot size depending on your screen resolution to 5 x 3
options(repr.plot.width=9, repr.plot.height=6)


str(Carseats)
head(Carseats)

#If you do not have an ID per row, use the following code to create an ID
carseats.df <- Carseats %>%
  mutate(id = row_number())
#Check IDs
head(carseats.df)
#Create training set
train.df <- carseats.df %>%
  sample_frac(.75)
#Create test set
test.df <- anti_join(carseats.df, train.df, by = 'id')

train.df <- train.df %>%
  select(-id)
test.df <- test.df %>%
    select(-id)

head(train.df)
head(test.df)


tree_model<-rpart(Sales ~ .,
                  data=train.df,
                  method="anova",
                  control=rpart.control( maxdepth = 2, # max depth
                                         xval = 1)) # to use all samples for the first try out
tree_model

printcp(tree_model)

rpart.plot(tree_model,
           fallen.leaves = FALSE, # to position the leaf nodes at the bottom of the graph changes this to TRUE
           type  = 1, # 1 Label all nodes, not just leaves.
           extra = 1, # 1 Display the number of observations that fall in the node
           split.font = 1, # Font for the split labels. 1=normal 2=bold
           varlen = -10) # Length of variable names in text at the splits (and, for class responses, the class in the node label). Default -8, meaning truncate to eight characters.


treePred.reg <- predict(tree_model, newdata = test.df)

head(treePred.reg)

rmse(test.df$Sales, treePred.reg)




### BEGIN SOLUTION
tmp.df <- data.frame(actual=test.df$Sales,predicted=treePred.reg)
head(tmp.df)

ggplot(data=tmp.df,
       aes(x=predicted, y=actual)) +
  geom_point() +
  ylim(0,13) +
  xlim(0,13)
### END SOLUTION


minsplit <- 1:2
minbucket <- 1:2
maxdepth <- 1:5

n<- length(minsplit)*length(minbucket)*length(maxdepth)

gridsearch.df <- data.frame(minsplit = numeric(n),
                            minbucket = numeric(n),
                            maxdepth = numeric(n),
                            rmse = numeric(n))

### BEGIN SOLUTION
i <- 1
for(split in minsplit){
    for(bucket in minbucket){
        for(depth in maxdepth) {
            tmp_tree<-rpart(Sales ~ .,
                         data=train.df,
                         method="anova",
                         control=rpart.control(
                             minsplit = split,
                             minbucket = bucket,
                             maxdepth = depth,
                             xval = 1))

            tmp.reg <- predict(tmp_tree, newdata = test.df)
            gridsearch.df$minsplit[i]  <- split
            gridsearch.df$minbucket[i] <- bucket
            gridsearch.df$maxdepth[i]  <- depth
            gridsearch.df$rmse[i]      <- rmse(test.df$Sales, tmp.reg)
            i <- i + 1
        }
    }
}
### END SOLUTION

gridsearch.df %>%
    arrange(rmse)

### BEGIN SOLUTION
tmp_tree<-rpart(Sales ~ .,
                         data=train.df,
                         method="anova",
                         control=rpart.control(
                             minsplit = 1,
                             minbucket = 2,
                             maxdepth = 5,
                             xval = 1))

tmp.reg <- predict(tmp_tree, newdata = test.df)


tmp.df <- data.frame(actual=test.df$Sales,predicted=tmp.reg)
head(tmp.df)

ggplot(data=tmp.df,aes(x=predicted,
                       y=actual)) +
        geom_point() +
        ylim(0,13) +
        xlim(0,13)
### END SOLUTION


### BEGIN SOLUTION
auth <- readr::read_csv('./Day2/auth.csv') # never save a password in a notebook directly
drv <- dbDriver('PostgreSQL')
# you need to close the connection
con <- dbConnect(
  drv,
  host = "118.138.239.213",
  dbname = "summer2022",
  user = toString(auth$username),
  password = toString(auth$password)
)

query <-
"SELECT *
FROM public.flight_delay_workshop
where destination != 'EGE';"

flights.df <- dbGetQuery(con, query)
head(flights.df)


train.df <- flights.df %>%
  filter(month<12) %>%
  select(arrdelay, carrier, dayofweek, departurehour, destination, weatherdelay, airtime)
head(train.df)

test.df <- flights.df %>%
                filter(month==12) %>%
                select(arrdelay, carrier, dayofweek, departurehour, destination, weatherdelay, airtime )
head(test.df)

tmp_tree<-rpart(arrdelay ~ .,
                data=train.df,
                method="anova",
                control=rpart.control(
                  xval = 10))
tmp_tree

tmp.reg <- predict(tmp_tree, newdata = test.df)


tmp.df <- data.frame(actual=test.df$arrdelay,predicted=tmp.reg)
head(tmp.df)

ggplot(data=tmp.df,
       aes(x=predicted, y=actual)) +
  geom_point()

rmse(test.df$arrdelay, tmp.reg)

options(repr.plot.width=15, repr.plot.height=10)

rpart.plot(tmp_tree,
           fallen.leaves = FALSE, # to position the leaf nodes at the bottom of the graph changes this to TRUE
           type  = 1, # 1 Label all nodes, not just leaves.
           extra = 1, # 1 Display the number of observations that fall in the node
           split.font = 1, # Font for the split labels. 1=normal 2=bold
           varlen = -10) # Length of variable names in text at the splits (and, for class responses, the class in the node label). Default -8, meaning truncate to eight characters.
### END SOLUTION

