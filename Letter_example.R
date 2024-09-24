# Application of multi-class logistic to letters data
install.packages("microbenchmark")
library(microbenchmark)
# Load the letter data
#########################
# Training data
letter_train = read.table("Data/letter-train.txt", header = F, colClasses = "numeric")
Y = letter_train[, 1]
X = as.matrix(letter_train[, -1])

# Testing data
letter_test = read.table("Data/letter-test.txt", header = F, colClasses = "numeric")
Yt = letter_test[, 1]
Xt = as.matrix(letter_test[, -1])

# [ToDo] Make sure to add column for an intercept to X and Xt
X = cbind(1, X)
Xt = cbind(1, Xt)


# Source the LR function
source("FunctionsLR.R")

# [ToDo] Try the algorithm LRMultiClass with lambda = 1 and 50 iterations. Call the resulting object out, i.e. out = LRMultiClass(...)
out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1)

# The code below will draw pictures of objective function, as well as train/test error over the iterations
plot(out$objective, col = "blue", type = 'o')
plot(out$error_train,  col = "red",type = 'o')
plot(out$error_test,  col = "green",type = 'o')

# Feel free to modify the code above for different lambda/eta/numIter values to see how it affects the convergence as well as train/test errors
out2 = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 2)
plot(out2$objective, col = "blue", type = 'o')
plot(out2$error_train,  col = "red",type = 'o')
plot(out2$error_test,  col = "green",type = 'o')

out3 = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.01, lambda = 2)
plot(out3$objective, col = "blue", type = 'o')
plot(out3$error_train,  col = "red",type = 'o')
plot(out3$error_test,  col = "green",type = 'o')



# [ToDo] Use microbenchmark to time your code with lambda=1 and 50 iterations. To save time, only apply microbenchmark 5 times.
benchmarkResult = microbenchmark(
  out = LRMultiClass(X, Y, Xt, Yt, numIter = 50, eta = 0.1, lambda = 1),
  times = 5
)

summary(benchmarkResult)

# [ToDo] Report the median time of your code from microbenchmark above in the comments below

# Median time:  (in sec)