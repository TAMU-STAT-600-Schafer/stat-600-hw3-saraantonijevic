# This is a script to save your own tests for the function
source("FunctionsLR.R")

set.seed(123)

#Creating sample data for testing

n = 100 #number of samples per class
p = 3 #number of features

#Creating two normal populations
xClass1 = matrix(rnorm(n*(p-1), mean = 0), nrow= n)
xClass2 = matrix(rnorm(n*(p-1), mean = 2), nrow = n)
X = rbind(cbind(1, xClass1), cbind(1, xClass2))
y = c(rep(0, n), rep(1,n))


# Test data with similar separation
XtClass1 = matrix(rnorm(n * (p - 1), mean = 1), nrow = n)
XtClass2 = matrix(rnorm(n * (p - 1), mean = 3), nrow = n)  # Increased separation for class 2
Xt = rbind(cbind(1, XtClass1), cbind(1, XtClass2))
yt = c(rep(0, n), rep(1, n))

#random test set 

numIter = 50
eta = 0.1
lambda = 0.1


result = LRMultiClass(X, y, Xt, yt, numIter, eta, lambda)

cat("Training Error %: ", result$error_train, "\n")

cat("Testing Error %: ", result$error_test, "\n")

plot(result$objective, type = "l", main = "Objective Function", ylab = "Objective Value", xlab = "Iteration")

