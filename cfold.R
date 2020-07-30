#this function create n folds data for cross validation
#for cfold.indexes(), x is the data frame, n is the number of folds
# this function return row index of the testing sets


cfold.indexes <- function(x, n){
x = x[sample(nrow(x)), ]
testIndexes = list()
#Create n equally size folds
folds = cut(seq(1, nrow(x)), breaks = n, labels=FALSE)
for(i in 1:n){
  #Segment your data by fold using the which() function 
  testIndexes[[i]] = which(folds == i, arr.ind=TRUE)
  }
return(testIndexes)
}

#for cfold.test, x is the data frame, n is the number of folds, indx is the row index of testing sets
#this function return list of data frames of testing set
#last object of the list contains number of folds

cfold.test <- function(x, n, indx){
  testData <- list()
  for(i in 1:n){
    testData[[i]] = x[indx[[i]], ]
  }
  testData[[i+1]] = n
  return(testData)
}

#for cfold.train, x is the data frame, n is the number of folds, indx is the row index of testing sets
#this function return list of data frames of trainning set
#last object of the list contains number of folds

cfold.train <- function(x, n, indx){
  trainData <- list()
  for(i in 1:n){
    trainData[[i]] = x[-indx[[i]], ]
  }
  trainData[[i+1]] = n
  return(trainData)
}


