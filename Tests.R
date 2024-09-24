# This is a script to save your own tests for the function
source("FunctionsLR.R")

set.seed(123)

#Creating sample data for testing

n = 100
p = 2

#class 1: Mean (2, 2)
X1 = matrix(rnorm(n * p, mean = 2), ncol = p)
y0 = rep(0, n)

#class 2: Mean (-2, -2)
X2 = matrix(rnorm(n * p, mean = -2), ncol = p)
y2 = rep(1, n)

X = rbind(X1, X2)
y = c(y0, y2)

# Add intercept (column of 1's)
X = cbind(1, X)

#splitting train/test sets
trainIdx = sample(1:(2*n), size = 0.7 * 2*n)
XTrain = X[trainIdx, ]
yTrain = y[trainIdx]

xTest = X[-trainIdx, ]
yTest = y[-trainIdx]

#running multi-class logistic regression
result = LRMultiClass(XTrain, yTrain, xTest, yTest, numIter = 50, eta = 0.1, lambda = 1)

result$objective

plot(result$objective, type = "b", col = "blue", xlab = "Iteration", ylab = "Objective Value", main = "Objective Value Over Iterations")



