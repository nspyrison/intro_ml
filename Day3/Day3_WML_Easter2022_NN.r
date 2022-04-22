# install.packages("mvtnorm")

# Data Generation
## Libraries:
library(mvtnorm) # generates multivariate Gaussian samples and calculate the densities
library(ggplot2)
library(reshape2)

## Setting parameters
set.seed(12345) # set random seed
N <- 100 # total number of samples
D <- 2 # number of dimensions

## Initialization
c0 <- "0"
c1 <- "1" # class labels
mu0 <- c(1.0, 4.0)
p0 <- 0.60
mu1 <- c(4.5, 0.5)
p1 <- 1 - p0

sigma <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE) # shared covariance matrix
sigma0 <- sigma
sigma1 <- sigma

data <- data.frame(x1 = double(), x2 = double(), label = double()) # empty data.frame

## Generate class labels
data[1:N, "label"] <- sample(c(c0, c1), N, replace = TRUE, prob = c(p0, p1))

## calculate the size of each class
N0 <- sum(data[1:N, "label"] == c0)
N1 <- N - N0

## Sample from the Gaussian distribution according to the class labels and statistics.
data[data$label == c0, c("x1", "x2")] <- rmvnorm(n = N0, mu0, sigma0)
data[data$label == c1, c("x1", "x2")] <- rmvnorm(n = N1, mu1, sigma1)

## Split data to train and test datasets
train.len <- round(N / 2)
train.index <- sample(N, train.len, replace = FALSE)
train.data <- data[train.index, c("x1", "x2")]
test.data <- data[-train.index, c("x1", "x2")]
train.label <- data[train.index, "label"]
test.label <- data[-train.index, "label"]

## Take a look at the data set
ggplot(data = data[1:N, ], aes(x = x1, y = x2, color = label, label = ifelse(label == c0, "0", "1"))) +
  geom_point(x = mu0[1], y = mu0[2], size = 4, color = "black") +
  geom_point(x = mu1[1], y = mu1[2], size = 4, color = "black") +
  # geom_density2d() +
  geom_text(size = 5, alpha = 0.5) +
  ggtitle("Data set") +
  theme_minimal()

# auxiliary functions
## the activation function (sigmoid here)
h <- function(z) {
  return(1 / (1 + exp(-3 * z)))
}
## the derivitive of the activation function (sigmoid here)
h.d <- function(z) {
  return(h(z) * (1 - h(z)))
}
## Class Probabilities
probability <- function(X, W1, W2, b1, b2) {
  a2 <- h(sweep(W1 %*% X, 1, b1, "+"))
  a3 <- h(sweep(W2 %*% a2, 1, b2, "+"))
  return(a3)
}
## prediction
prediction <- function(X, W1, W2, b1, b2, threshold = 0.5) {
  return(ifelse(probability(X, W1, W2, b1, b2) >= threshold, 1, 0))
}
## Accuracy
accuracy <- function(Y, T) {
  return(sum(Y == T) / length(T) * 100)
}
## The following structure helps us to have functions with multiple outputs
### credit: https://stat.ethz.ch/pipermail/r-help/2004-June/053343.html
list <- structure(NA, class = "result")
"[<-.result" <- function(x, ..., value) {
  args <- as.list(match.call())
  args <- args[-c(1:2, length(args))]
  length(value) <- length(args)
  for (i in seq(along = args)) {
    a <- args[[i]]
    if (!missing(a)) eval.parent(substitute(a <- v, list(a = a, v = value[[i]])))
  }
  x
}

# Some conversions:
## rename just for convenience
N <- train.len
## convert data and labels to matrices
X1 <- t(unname(data.matrix(train.data)))
T1 <- t(data.matrix(as.numeric(train.label)))
X2 <- t(unname(data.matrix(test.data)))
T2 <- t(data.matrix(as.numeric(test.label)))

feedforward <- function(Xi, Ti, W1, b1, W2, b2) {
  ### 1st (input) layer
  a1 <- Xi
  y <- Ti
  ### 2nd (hidden) layer
  z2 <- W1 %*% a1 + b1
  a2 <- h(z2)
  ### 3rd (output) layer
  z3 <- W2 %*% a2 + b2
  a3 <- h(z3)
  return(list(a1, a2, a3, y, z2, z3))
}

backpropagation <- function(Ti, W2, z2, z3, a3) {
  ### 3rd (output) layer
  d3 <- -(Ti - a3) * h.d(z3)
  ### 2nd (hidden) layer
  d2 <- t(W2) %*% d3 * h.d(z2)
  return(list(d2, d3))
}

# Setting parameters
K <- 3 # number of units in the hidden layer
epoch.max <- 1000 # maximum number of iterations
eta <- 0.1 # learning rate
lambda <- 0.0001 # regularization term

# initialization
epoch <- 1 # epoch (iteration) counter
terminate <- FALSE # termination criteria

## weight vectors/matrices initialization
### w stands for weight and b for bias
### the numbers after the letters indicates the layer number
W1 <- matrix(0.01 * rnorm(D * K, sd = 0.5), nrow = K, ncol = D)
b1 <- matrix(0 * rnorm(1 * K), nrow = K, ncol = 1)
W2 <- matrix(0.01 * rnorm(K * 1, sd = 0.5), nrow = 1, ncol = K)
b2 <- matrix(0 * rnorm(1 * 1), nrow = 1, ncol = 1)

## tracing accuracy of the model
train.accuracy <- matrix(0, nrow = epoch.max, ncol = 1)

# main loop
while (!terminate) {

  ## delta vectors/matrices initialization (for batch backpropagation)
  ### .d stands for delta
  W1.d <- W1 * 0
  b1.d <- b1 * 0
  W2.d <- W2 * 0
  b2.d <- b2 * 0

  ## inner loop for each train sample
  for (i in 1:N) {

    ## Feedforward:
    list[a1, a2, a3, y, z2, z3] <- feedforward(X1[, i], T1[i], W1, b1, W2, b2)

    ## Backpropagation:
    list[d2, d3] <- backpropagation(T1[i], W2, z2, z3, a3)

    ## calculate the delta values
    ### 1st layer
    W1.d <- W1.d + d2 %*% t(a1)
    b1.d <- b1.d + d2
    ### 2nd layer
    W2.d <- W2.d + d3 %*% t(a2)
    b2.d <- b2.d + d3
  }

  ## update weight vectors and matrices
  ### 1st (input) layer
  W1 <- W1 - eta * (W1.d / N + lambda * W1)
  b1 <- b1 - eta * (b1.d / N)
  ### 2nd (hidden) layer
  W2 <- W2 - eta * (W2.d / N + lambda * W2)
  b2 <- b2 - eta * (b2.d / N)

  ## trace train accuracy
  train.accuracy[epoch] <- accuracy(prediction(X1, W1, W2, b1, b2), T1)

  ## increase the iteration counter
  epoch <- epoch + 1

  ## check the termination criteria
  if (epoch > epoch.max) {
    terminate <- TRUE
  }
}
print("Done!")

## Train Accuracy
plot(train.accuracy, main = "Train Accuracy", xlab = "epoch", ylab = "Accuracy(%)")

## Initialization
set.seed(12345)
N <- 500
c0 <- "0"
c1 <- "1"
c2 <- "2" # class labels
mu0 <- c(1.0, 4.0)
p0 <- 0.30
mu1 <- c(4.5, 0.5)
p1 <- 0.50
mu2 <- c(3.0, -3.0)
p2 <- 1 - p0 - p1

sigma <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE) # shared covariance matrix
sigma0 <- sigma
sigma1 <- sigma
sigma2 <- sigma

data <- data.frame(x1 = double(), x2 = double(), label = double()) # empty data.frame

## Generate class labels
data[1:N, "label"] <- sample(c(c0, c1, c2), N, replace = TRUE, prob = c(p0, p1, p2))
## calculate the size of each class
N0 <- sum(data[1:N, "label"] == c0)
N1 <- sum(data[1:N, "label"] == c1)
N2 <- N - N0 - N1

## Sample from the Gaussian distribution according to the class labels and statistics.
data[data[1:N, "label"] == c0, c("x1", "x2")] <- rmvnorm(n = N0, mu0, sigma0)
data[data[1:N, "label"] == c1, c("x1", "x2")] <- rmvnorm(n = N1, mu1, sigma1)
data[data[1:N, "label"] == c2, c("x1", "x2")] <- rmvnorm(n = N2, mu2, sigma2)
data[data[1:N, "label"] == c2, "label"] <- c0 ## relabel 3rd clust as first
## Take a look at the data set
ggplot(data = data, aes(x = x1, y = x2, color = label, label = ifelse(label == c0, "0", "1"))) +
  geom_text(size = 5, alpha = 0.5) +
  ggtitle("Data set") +
  theme_minimal()
N <- nrow(data)
train.len <- round(N / 2)
train.index <- sample(N, train.len, replace = FALSE)
train.data <- data[train.index, c("x1", "x2")]
test.data <- data[-train.index, c("x1", "x2")]
train.label <- data[train.index, "label"]
test.label <- data[-train.index, "label"]
# Some conversions:
## rename just for convenience
N <- train.len

## convert data and labels to matrices
X1 <- t(unname(data.matrix(train.data)))
T1 <- t(data.matrix(as.numeric(train.label)))

X2 <- t(unname(data.matrix(test.data)))
T2 <- t(data.matrix(as.numeric(test.label)))

X2

T2

N

NN <- function(K, N, X1, T1, X2, T2, tracing = FALSE, epoch.max = 500) {
  # Setting parameters
  D <- 2
  eta <- 0.1 # learning rate
  lambda <- 0.0001 # regularization term
  # initialization
  if (tracing == TRUE) {
    train.accuracy <- matrix(0, nrow = epoch.max, ncol = 1)
    test.accuracy <- train.accuracy
  }
  epoch <- 1 # epoch (iteration) counter
  terminate <- FALSE # termination criteria
  W1 <- matrix(0.01 * rnorm(D * K, sd = 0.5), nrow = K, ncol = D)
  b1 <- matrix(0 * rnorm(1 * K), nrow = K, ncol = 1)
  W2 <- matrix(0.01 * rnorm(K * 1, sd = 0.5), nrow = 1, ncol = K)
  b2 <- matrix(0 * rnorm(1 * 1), nrow = 1, ncol = 1)
  # main loop
  while (!terminate) {
    ## delta vectors/matrices initialization
    W1.d <- W1 * 0
    b1.d <- b1 * 0
    W2.d <- W2 * 0
    b2.d <- b2 * 0
    ## inner loop for each train sample
    for (i in 1:N) {
      ## Feedforward:
      list[a1, a2, a3, y, z2, z3] <- feedforward(X1[, i], T1[i], W1, b1, W2, b2)
      ## Backpropagation:
      list[d2, d3] <- backpropagation(T1[i], W2, z2, z3, a3)
      ## calculate the delta values
      ### 1st layer
      W1.d <- W1.d + d2 %*% t(a1)
      b1.d <- b1.d + d2
      ### 2nd layer
      W2.d <- W2.d + d3 %*% t(a2)
      b2.d <- b2.d + d3
    }
    ## update weight vectors and matrices
    W1 <- W1 - eta * (W1.d / N + lambda * W1)
    b1 <- b1 - eta * (b1.d / N)
    W2 <- W2 - eta * (W2.d / N + lambda * W2)
    b2 <- b2 - eta * (b2.d / N)
    ## record the errors
    if (tracing == TRUE) {
      train.accuracy[epoch] <- accuracy(prediction(X1, W1, W2, b1, b2), T1)
      test.accuracy[epoch] <- accuracy(prediction(X2, W1, W2, b1, b2), T2)
    }
    ## increase the iteration counter
    epoch <- epoch + 1
    ## check the termination criteria
    if (epoch > epoch.max) {
      terminate <- TRUE
    }
  }
  if (tracing == FALSE) {
    train.accuracy <- accuracy(prediction(X1, W1, W2, b1, b2), T1)
    test.accuracy <- accuracy(prediction(X2, W1, W2, b1, b2), T2)
  }
  return(cbind(train.accuracy, test.accuracy))
}

results <- NN(3, N, X1, T1, X2, T2, tracing = TRUE, epoch.max = 1000)
plot(results[, 1], main = "Train Accuracy", xlab = "epoch", ylab = "Accuracy(%)")

results[, 1]
