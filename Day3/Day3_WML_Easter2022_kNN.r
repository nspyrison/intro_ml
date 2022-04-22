# initial settings
# Note the installation of the forecast packages takes a while
# libary_path <- paste(getwd(), "packages", sep="/")
# dir.create(libary_path,showWarnings = FALSE)
# .libPaths(libary_path)

if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require(repr)) {
  install.packages("repr")
  library(repr)
}
if (!require(lubridate)) {
  install.packages("lubridate")
  library(lubridate)
}
if (!require(janitor)) {
  install.packages("janitor")
  library(janitor)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
}

# Plot size depending on your screen resolution
options(repr.plot.width = 4, repr.plot.height = 4)


### BEGIN SOLUTION
ktc.df <- read.table("./Day3/KTC.csv", header = TRUE, sep = ",")
head(ktc.df)

ktcNumeric.df <- ktc.df %>%
  select(ID, Age, Income, Children) %>%
  column_to_rownames(var = "ID") %>%
  scale()
head(ktcNumeric.df)

km <- kmeans(ktcNumeric.df, centers = 3, nstart = 25)
km
### END SOLUTION


km$betweenss / km$totss
km$betweenss
km$totss
str(km)


### BEGIN SOLUTION
ktcNumeric.df %>%
  as_tibble() %>%
  mutate(k3 = km$cluster) %>%
  ggplot(
    data = .,
    aes(
      x = Age,
      y = Income,
      colour = factor(k3),
      label = k3
    )
  ) +
  geom_text()

ktcNumeric.df %>%
  as_tibble() %>%
  mutate(k3 = km$cluster) %>%
  ggplot(
    data = .,
    aes(
      x = Children,
      y = Income,
      colour = factor(k3),
      label = k3
    )
  ) +
  geom_text()

ktcNumeric.df %>%
  as_tibble() %>%
  mutate(k3 = km$cluster) %>%
  ggplot(
    data = .,
    aes(
      x = Children,
      y = Age,
      colour = factor(k3),
      label = k3
    )
  ) +
  geom_text()
### END SOLUTION

### BEGIN SOLUTION
n <- 6
ss.df <- data.frame(
  k = numeric(n), # the column to store the number of cluster
  ss = numeric(n), # the column for the corresponding within cluster sum of squares
  tot = numeric(n)
)
for (i in 1:n) {
  km <- kmeans(ktcNumeric.df, centers = i, nstart = 25) # calculate the clusters
  ss.df$k[i] <- i # store the index of how many clusters
  ss.df$ss[i] <- km$betweenss / km$totss # store the calculated
  ss.df$tot[i] <- km$tot.withinss
}
### END SOLUTION

ss.df


### BEGIN SOLUTION
ggplot(
  data = ss.df,
  aes(
    x = k,
    y = ss
  )
) +
  geom_line() +
  xlab("k: Number of Clusters") +
  ylab("ss: Between ss / total ss")

ggplot(
  data = ss.df,
  aes(
    x = k,
    y = tot
  )
) +
  geom_line() +
  xlab("k: Number of Clusters") +
  ylab("tot: The total within-cluster sum of square")
### END SOLUTION

### BEGIN SOLUTION
km <- kmeans(ktcNumeric.df, centers = 3, nstart = 25)
ktc.df %>%
  mutate(k3 = km$cluster) %>%
  select(-ID, ) %>%
  group_by(k3) %>%
  summarise_all(mean)
### END SOLUTION

### BEGIN SOLUTION
# read raw data
house_raw <- readr::read_csv("./Day3/MELBOURNE_HOUSE_PRICES_LESS.csv")

# clean up data
house_less <- house_raw %>%
  janitor::clean_names() %>%
  mutate(across(c(suburb, address, type, method, postcode, regionname, council_area),
    .fns = as_factor
  ),
  date = dmy(date),
  price = price / 1e3
  ) %>%
  select(-c(address, seller_g, propertycount, regionname)) %>%
  drop_na(price)
### END SOLUTION

### BEGIN SOLUTION
houseNumeric.df <- house_less %>%
  select(price, distance, rooms) %>%
  scale()

house_less$km <- kmeans(houseNumeric.df, centers = 6, nstart = 25)$cluster


house_less %>%
  select(price, distance, rooms, km) %>%
  group_by(km) %>%
  summarise_all(mean)
### END SOLUTION

### BEGIN SOLUTION
options(repr.plot.width = 8, repr.plot.height = 8)

ggplot(data = house_less, aes(
  y = price,
  x = distance,
  colour = as.factor(km)
)) +
  geom_point()
### END SOLUTION

### BEGIN SOLUTION
ggplot(data = house_less, aes(
  y = rooms,
  x = distance,
  colour = as.factor(km)
)) +
  #geom_point() +
  geom_jitter()
### END SOLUTION

# loading the iris dataset
iris.pc <- prcomp(iris[, 1:4], center = FALSE, scale. = FALSE)$x %>% as.data.frame()
head(iris.pc)

# k-means
km.cluster <- kmeans(iris[, 1:4], centers = 3, iter.max = 20, nstart = 2)
iris.pc$kmeans.cluster <- km.cluster$cluster
table(iris$Species, km.cluster$cluster) ## confusion matrix

# plotting the result of modelling
ggplot(data = iris.pc, aes(
  y = PC2,
  x = PC1,
  colour = as.factor(kmeans.cluster)
)) +
  geom_point() +
  geom_jitter() +
  labs(x = "principal comp 1", y = "principal comp 2")

# comparing with the clustering promoted by "Random Forest" clustering
library(randomForest)
rf.fit <- randomForest(x = iris[, 1:4], y = NULL, ntree = 10000, proximity = TRUE, oob.prox = TRUE)
hclust.rf <- hclust(as.dist(1 - rf.fit$proximity), method = "ward.D2")
rf.cluster <- cutree(hclust.rf, k = 3)
iris.pc$rf.clusters <- rf.cluster
table(rf.cluster, iris$Species)

# plotting the clustering executed by Random Forest
ggplot(data = iris.pc, aes(
  y = PC2,
  x = PC1,
  colour = as.factor(rf.clusters)
)) +
  geom_point() +
  geom_jitter() +
  labs(x = "principal comp 1", y = "principal comp 2")
