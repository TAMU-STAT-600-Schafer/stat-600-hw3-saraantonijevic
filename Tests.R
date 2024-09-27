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
X_train = X[trainIdx, ]
y_train = y[trainIdx]

X_test = X[-trainIdx, ]
y_test = y[-trainIdx]

#running multi-class logistic regression
result = LRMultiClass(X_train, y_train, X_test, y_test, numIter = 50, eta = 0.1, lambda = 1)

result$objective

plot(result$objective, type = "b", col = "blue", xlab = "Iteration", ylab = "Objective Value", main = "Objective Value Over Iterations")


#Testing classification errors over iterations
cat("Training error over iterations: ", result$error_train, "\n")
cat("Testing error over iterations: ", result$error_test, "\n")

plot(result$error_train, type = "b", col = "red", ylim = c(0, max(result$error_train, result$error_test)), 
     xlab = "Iteration", ylab = "Error %", main = "Training and Test Error Over Iterations")
lines(result$error_test, type = "b", col = "green")
legend("topright", legend = c("Train Error", "Test Error"), col = c("red", "green"), lty = 1, pch = 1)


#Check if decreasing
decreasing = all(diff(result$objective) <= 0)
if (decreasing) {
  cat("The objective function decreases across iterations.\n")
} else {
  cat("The objective function does not consistently decrease!\n")
}


#More sample tests
result2 = LRMultiClass(X_train, y_train, X_test, y_test, numIter = 50, eta = 0.05, lambda = 0.5)

cat("New Objective values with eta = 0.05 and lambda = 0.5:", result2$objective)
cat("New train errors with eta = 0.05 and lambda = 0.5:", result2$error_train)
cat("New test errors with eta = 0.05 and lambda = 0.5:", result2$error_test)


