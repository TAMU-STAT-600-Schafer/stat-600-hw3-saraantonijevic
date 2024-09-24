# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix 

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
LRMultiClass = function(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if(!all(X[,1] == 1)){ #for X
    stop("First columns of X is not all ones")
  }
  if(!all(Xt[,1] == 1)){ #for X transpose
    stop("First columns of X is not all ones")
  }
  
  
  # Check for compatibility of dimensions between X and Y
  if(nrow(X) != length(y)){
    stop("Number of rows in X doesn't match lenght of Y")
  }
  # Check for compatibility of dimensions between Xt and Yt
  if(nrow(Xt) != length(yt)){
    stop("Number of rows in Xt doesn't match lenght of Yt")
  }
  # Check for compatibility of dimensions between X and Xt
  if (ncol(X) != ncol(Xt)) {
    stop("The number of columns in X and Xt do not match.")
  }
  # Check eta is positive
  if(eta<= 0){
    stop("Eta must be positive")
  }
  
  # Check lambda is non-negative
  if(lambda < 0){
    stop("Lambda must be non-negative.")
  }
  
  
  K = length(unique(y)) #number of classes
  p = ncol(X) #number of features
  
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes. If not NULL, check for compatibility of dimensions with what has been already supplied.
  if (is.null(beta_init)){
    #initialize beta with p x K matrix of zeroes
    beta = matrix(0, nrow = p, ncol = K)
  }
  else{
    # not NULL, check for compatibility of dimensions with what has been already supplied
    if(nrow(beta_init) != p || ncol(beta_init) != K){
      stop("beta_init dimensions are incompatible with X and y")
    }
    beta = beta_init
  }
  
  #Initialize objective, error_train, and error_test vectors
  objective = numeric(numIter + 1)
  error_train = numeric(numIter + 1)
  error_test = numeric(numIter + 1) 
  
  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################

  #Calculate corresponding pk
  calculateProbs = function(X, beta){
    linearComb = X %*% beta #computes linear combo of feature matrix X and the coeff matrix beta
    expXB = exp(linearComb - apply(linearComb,1, max)) #applies softmax operation in numerically stable way 
    return(expXB/ rowSums(expXB))  #normalization to get probs  
  }
  
  #objective value f(beta_init)
  calcObjective = function(P, y, beta, lambda){
    #negative log-likelohood: sum over the log of the prob corresponding to the true class labels
    logLikelihood = -sum(log(P[cbind(1:nrow(X), y + 1)] + 1e-10)) #y +1 in order to adjust for R's indexing
    
    regularization =  (lambda / 2) * sum(beta^2)#L2 penalty
    
    return(logLikelihood + regularization)
  }

  #calculate training error
  
  calcError = function(P, y){
    predicted = max.col(P) - 1 #returns the column index of the highest value in each row
    return(mean(predicted != y) * 100) #percentage of misclassifications
  }
  
  #initial probabilities and errors
  prob_train = calculateProbs(X, beta) #calc probabilities for training data using beta values
  prob_test = calculateProbs(Xt, beta)#calc probabilities for testing data using beta values
  
  objective[1] = calcObjective(prob_train, y, beta, lambda) #calc the initial objective function value (negative log-likelihood + regularization) for the training data, using the current values of prob_train, y, beta, and lambda
 
  error_train[1] = calcError(prob_train, y) #calc the initial training error based on the current predictions prob_train and true class labels y
  error_test[1] = calcError(prob_test, yt) #calc the initial test error based on the current predictions prob_test and true class labels yt for the test data
  
  
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
 
  for( t in 1:numIter){
    
    
    # Update probabilities before Newton's method loop
    #probabilitiesUpdate = calculateProbs(X, beta)
    
    # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
    for(k in 1:K){
      Pk = prob_train[,k] #extract probabilities for class k
      
      weightedX = X * sqrt(Pk * (1 - Pk)) #element-wise multiplication for memory efficiency
      

      # Gradient for beta_k
      gradient = t(X) %*% (Pk - as.numeric(y == (k - 1))) + lambda * beta[, k]
     
      
       # Hessian for beta_k (approximated as X^T W_k X + lambda I)
      hessian = t(weightedX) %*% weightedX %*% X + lambda * diag(p)
      
      # Newton's method update for beta_k (with learning rate eta)
      beta[, k] = beta[, k] - eta * solve(hessian, gradient)
      
    }
    prob_train = calculateProbs(X, beta)
    prob_test = calculateProbs(Xt, beta)
    #update probabilities and ojective function
    objective[t+1] = calcObjective(prob_train, y, beta, lambda)
    error_train[t+1] = calcError(prob_train, y)
    error_test[t+1] = calcError(prob_test, yt)
    
   }
  
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}