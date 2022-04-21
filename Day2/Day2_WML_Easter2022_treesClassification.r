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





#Plot size depending on your screen resolution to 5 x 3
options(repr.plot.width=9, repr.plot.height=6)


### BEGIN SOLUTION
mowner.df <- read.table("./Day2/Mowner01.csv", header=TRUE, sep =",")
mowner.df
### END SOLUTION

### BEGIN SOLUTION
mowner.df$Owner<-factor(mowner.df$Owner,levels = c(0,1),labels = c("Noowner", "Owner"))
head(mowner.df,15)
### END SOLUTION


tree_model<-rpart(Owner ~ Income + Lot_Size,
                 data=mowner.df,
                 method="class",
                 control=rpart.control(minsplit  = 5, #the minimum number of observations that must exist in a node in order for a split to be attempted.
                                       minbucket = 5, # the minimum number of observations in any terminal <leaf> node.
                                       xval = 1)) # to use all samples for the first try out
tree_model

rpart.plot(tree_model,
           fallen.leaves = FALSE, # to position the leaf nodes at the bottom of the graph changes this to TRUE
           type  = 1, # 1 Label all nodes, not just leaves.
           extra = 1, # 1 Display the number of observations that fall in the node
           split.font = 1, # Font for the split labels. 1=normal 2=bold
           varlen = -10) # Length of variable names in text at the splits (and, for class responses, the class in the node label). Default -8, meaning truncate to eight characters.


treePred.class <- predict(tree_model, data = Mowner, type = "class")
head(treePred.class)

treePred.score <- predict(tree_model, data = Mowner, type = "prob")
head(treePred.score)

ownerScore <- treePred.score[,2]
head(ownerScore)

sscurves <- evalmod(scores = ownerScore, labels = mowner.df$Owner)
autoplot(sscurves, "ROC")
auc(sscurves) %>% filter(curvetypes=='ROC')

confusionMatrix(mowner.df$Owner,factor( ifelse(ownerScore > 0.5, "Owner", "Noowner") ),positive = 'Owner')

confusionMatrix(mowner.df$Owner,factor( ifelse(ownerScore > 0.3, "Owner", "Noowner") ),positive = 'Owner')

autoplot(sscurves, "PRC")


set.seed(3800)
cv.ct<-rpart(Owner ~ Income + Lot_Size,
                 data=mowner.df,
                 method="class",
                 control=rpart.control(minsplit  = 2, #the minimum number of observations that must exist in a node in order for a split to be attempted.
                                       minbucket = 4, # the minimum number of observations in any terminal <leaf> node.
                                       xval = 4))     # number of cross-validations.

printcp(cv.ct)

pruned.ct <- prune(cv.ct, cp = 0.010)
pruned.ct

rpart.plot(pruned.ct,
           fallen.leaves = FALSE, # to position the leaf nodes at the bottom of the graph changes this to TRUE
           type  = 1, # 1 Label all nodes, not just leaves.
           extra = 1, # 1 Display the number of observations that fall in the node
           split.font = 1, # Font for the split labels. 1=normal 2=bold
           varlen = -10) # Length of variable names in text at the splits (and, for class responses, the class in the node label). Default -8, meaning truncate to eight characters.


treePred.score <- predict(pruned.ct, data = Mowner, type = "prob")
ownerScore <- treePred.score[,2]

sscurves <- evalmod(scores = ownerScore, labels = mowner.df$Owner)
autoplot(sscurves, "ROC")
auc(sscurves) %>% filter(curvetypes=='ROC')


