library(caTools)
library(tidyverse)
library(kernlab)
library(e1071)
library(RColorBrewer)
library(ISLR)
library(caret)
library(ROCR)
library(pROC)

# Set the working directory 
setwd("/Users/maido/OneDrive - Oxford Brookes University/Advanced Machine Learning DALT7012/Assessments/grammatical_facial_expression")

# Load the User A data
data1A <- read.delim("a_topics_datapoints.txt", header=TRUE, sep=" ") 
target1A <- read.delim("a_topics_targets.txt", header=FALSE) 

# Check the real number of positives and negatives in the topics dataset person A
dim(data1A)
dim(target1A)
table(target1A)

# Add the labels (target dataset) onto the data frames 
data1A <- cbind(data1A, target1A)

# Remove the first column
data1A <- select(data1A, -X0.0) 

# Split the data into a training & testing dataset
set.seed(123) 
split1A <- sample.split(data1A$V1, SplitRatio = 0.75) 
train1A <- subset(data1A, split1A == TRUE) 
test1A <- subset(data1A, split1A == FALSE) 

# Feature scaling
train1A[,-301] <- scale(train1A[,-301])  
test1A[,-301] <- scale(test1A[,-301])  

# Applying Grid Search to find the best parameters
svm1A = train(form = V1 ~ . , 
                   data = train1A, 
                   method = "svmLinear")

# Show the best parameters
svm1A$bestTune

# Fitting SVM to the train set 1A
svm1A <- svm(formula = V1~.,
                  data = train1A,
                  type = "C-classification",
                  kernel = "linear", 
                  cost = 1, 
                  cross = 10)

# Predicting the test set results 
predict1A <- predict(svm1A, newdata = test1A[-301]) 

# Making the Confusion Matrix 
confusionMatrix(factor(predict1A), factor(test1A[,301]))

# comment: accuracy: 95.99%




##### a(ii) 4% ##### Repeat the training for a different facial expression
# Load the User A data conditional
data2A <- read.delim("a_conditional_datapoints.txt", header=TRUE, sep=" ") 
target2A <- read.delim("a_conditional_targets.txt", header=FALSE)

# Check the real number of positives and negatives in the dataset
dim(data2A)
table(target2A)

# Add the labels (target dataset) onto the data frames 
data2A <- cbind(data2A, target2A)

# Remove the first column
data2A <- select(data2A, -X0.0) 

# Split the data into a training & testing dataset
set.seed(123) 
split2A <- sample.split(data2A$V1, SplitRatio = 0.75) 
train2A <- subset(data2A, split2A == TRUE) 
test2A <- subset(data2A, split2A == FALSE) 

# Feature scaling
train1A[,-301] <- scale(train1A[,-301])  
test1A[,-301] <- scale(test1A[,-301])  

# Applying Grid Search to find the best parameters
svm2A = train(form = V1 ~ . , 
              data = train2A, 
              method = "svmLinear")

# Show the best parameters
svm2A$bestTune

# Fitting SVM to the train set 2A
svm2A <- svm(formula = V1~.,
             data = train2A,
             type = "C-classification",
             kernel = "linear", 
             cost = 1, 
             cross = 10)

# Predicting the test set results 
predict2A <- predict(svm2A, newdata = test2A[-301]) 

# Making the Confusion Matrix 
confusionMatrix(factor(predict2A), factor(test2A[,301]))

# comment: accuracy: 97.06%




##### a(iii) 8% ##### Extra marks for coding your own implementation of the chosen classifier
# SVM implementation
svm.fit = function(X, y, C=NULL) {
  n.samples = nrow(X)
  n.features = ncol(X)
  K = matrix(rep(0, n.samples*n.samples), nrow=n.samples)
  for (i in 1:n.samples){
    for (j in 1:n.samples){
      K[i,j] = X[i,] %*% X[j,] }}
  Dmat = outer(y,y) * K
  Dmat = as.matrix(nearPD(Dmat)$mat) 
  dvec = rep(1, n.samples)
  Amat = rbind(y, diag(n.samples), -1*diag(n.samples))
  bvec = c(0, rep(0, n.samples), rep(-C, n.samples))
  res = solve.QP(Dmat,dvec,t(Amat),bvec=bvec, meq=1)
  a = res$solution 
  bomega = apply(a*y*X,2,sum)
  return(bomega)
}




