# This is a script to save your own tests for the function
source("FunctionsLR.R")

set.seed(123)

#Creating sample data for testing

n = 100 #number of samples per class
p = 2 #number of features

#Creating two normal populations
xClass1 = matrix(rnorm(n*p, mean =2), ncol = p)

xClass2 = matrix(rnorm(n*p, mean =2), ncol = p)

X = rbind(xClass1, xClass2)
X = cbind(1, X) #adding 1's as intercept

y = c(rep(0, n), rep(1,n))

#random test set 

Xt = X
yt = y

numIter = 50

eta = 0.1

lambda = 1


result = LRMultiClass(X, y, Xt, yt, numIter, eta, lambda)

cat("Training Error %: ", result$error_train, "\n")

cat("Testing Error %: ", result$error_test, "\n")

plot(result$objective, type = "l", main = "Objective Function", ylab = "Objective Value", xlab = "Iteration")

