
#################### functions used in the main file  ##################

#function to normalize the data
normalize <- function(x){
  if(abs(max(x)) > 1)
  {
    return (x/max(x))
  }
  
  return (x)
}


#function to check zero variance predictors or near zero variance predictors
checkZeroVariance <- function(x)
{
  #frequency ratio of the most prevalent value/ 2nd most prevalent value
  freq <- max(table(x))/nrow(data)
  #percent of unique values
  unique.perc <- length(table(x))/nrow(data)
}



#function to reduce dimensions of original data using PCA 
#returns compact data (PCs Scores)
pcaData <- function(data, k)
{
  
  #number of principal components
  n <- min(nrow(data), ncol(data))
  pca <- prcomp(data)
  lambda <- pca$sdev^2
  lambdaProp <- lambda/sum(lambda)
  numPCs <- length(which(cumsum(lambdaProp)<k)) #k
  
  compactData <- pca$x[, 1:numPCs] 
  
  
  return(compactData)
}



spcaData <- function(data, m)
{
  #data <- prostateData
  mylogit <- NULL
  n <- ncol(data.final)-1
  
  for(i in 2:(n+1))
  {
    temp <- coef(summary(glm(IsPayer ~ . ,data=data.final[c(1:i)], family = "binomial")))[2]
    mylogit <- c(mylogit,temp)
  }
  
  
  index <- which(abs(mylogit)>m)
  reducedX <- data[, c(index+1)]
  
  return(reducedX)
}

