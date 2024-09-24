# This is a script to save your own tests for the function
source("FunctionsLR.R")

lettersTrain = read.table("https://raw.githubusercontent.com/TAMU-STAT-600-Schafer/stat-600-hw3-saraantonijevic/main/Data/letter-train.txt", header = FALSE)


letterTest = read.table("https://raw.githubusercontent.com/TAMU-STAT-600-Schafer/stat-600-hw3-saraantonijevic/main/Data/letter-test.txt", header = FALSE)


#exctract covariates 
xTrain = as.matrix(lettersTrain[, -1]) #extracting training data features
yTrain = as.numeric(lettersTrain[, 1])

xTest = as.matrix(letterTest[,-1]) #all columns of test but features
yTest = as.numeric(letterTest[, 1])

numIter = 50 # number of iterations for newton's method
eta = 0.1 #learning rate
lambda = 1 #regularization param


#Starting to initialize for logistic regression
