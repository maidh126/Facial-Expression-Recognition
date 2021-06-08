##### c(i) ##### Repeat the analysis by inverting the roles of the user ##### 
## Training on 1B (called 3B)
# Load the User B data topics
data3B <- read.delim("b_topics_datapoints.txt", header=TRUE, sep=" ") 
target3B <- read.delim("b_topics_targets.txt", header=FALSE)

# Add the labels (target dataset) onto the data frames 
data3B <- cbind(data3B, target3B)

# Remove the first column
data3B <- select(data3B, -X0.0) 

# Split the data into a training & testing dataset
set.seed(123) 
split3B <- sample.split(data3B$V1, SplitRatio = 0.75) 
train3B <- subset(data3B, split3B == TRUE) 
test3B <- subset(data3B, split3B == FALSE) 

# Feature scaling
train3B[,-301] <- scale(train3B[,-301]) 
test3B[,-301] <- scale(test3B[,-301]) 

# Fitting SVM to the train set
svm3B <- svm(formula = V1~.,
             data = train3B,
             type = "C-classification",
             kernel = "linear", 
             cost = 1, 
             cross = 10)

# Predicting the test set results 
predict3B <- predict(svm3B, newdata = test3B[-301]) 

# Making the Confusion Matrix 
confusionMatrix(factor(predict3B), factor(test3B[,301]))

# comment: accuracy: 89.72% 


## Test on 1A (called 3A)
# Load the User A data topics
data3A <- read.delim("a_topics_datapoints.txt", header=TRUE, sep=" ") 
target3A <- read.delim("a_topics_targets.txt", header=FALSE)

# Add the labels (target dataset) onto the data frames 
data3A <- cbind(data3A, target3A)

# Remove the first column
data3A <- select(data3A, -X0.0) 

# Feature scaling
data3A[,-301] <- scale(data3A[,-301]) 

# Predicting the results 
predict3A <- predict(svm3B, newdata = data3A[-301]) 

# Making the Confusion Matrix 
confusionMatrix(predict3A, factor(data3A[,301]))

# comment: accuracy: 71.88% 



## Training on 2B (called 4B)
# Load the User B data conditional
data4B <- read.delim("b_conditional_datapoints.txt", header=TRUE, sep=" ") 
target4B <- read.delim("b_conditional_targets.txt", header=FALSE)

# Add the labels (target dataset) onto the data frames 
data4B <- cbind(data4B, target4B)

# Remove the first column
data4B <- select(data4B, -X0.0) 

# Split the data into a training & testing dataset
set.seed(123) 
split4B <- sample.split(data4B$V1, SplitRatio = 0.75) 
train4B <- subset(data4B, split4B == TRUE) 
test4B <- subset(data4B, split4B == FALSE) 

# Feature scaling
train4B[,-301] <- scale(train4B[,-301]) 
test4B[,-301] <- scale(test4B[,-301]) 

# Fitting SVM to the train set 4B
svm4B <- svm(formula = V1~.,
             data = train4B,
             type = "C-classification",
             kernel = "linear", 
             cost = 1, 
             cross = 10)


# Predicting the test set results 
predict4B <- predict(svm4B, newdata = test4B[-301]) 

# Making the Confusion Matrix 
confusionMatrix(factor(predict4B), factor(test4B[,301]))

# comment: accuracy: 91.34%


## Test on 2A (called 4A)
# Load the User A data conditional
data4A <- read.delim("a_conditional_datapoints.txt", header=TRUE, sep=" ") 
target4A <- read.delim("a_conditional_targets.txt", header=FALSE)

# Add the labels (target dataset) onto the data frames 
data4A <- cbind(data4A, target4A)

# Remove the first column
data4A <- select(data4A, -X0.0) 

# Feature scaling
data4A[,-301] <- scale(data4A[,-301]) 

# Predicting the results 
predict4A <- predict(svm4B, newdata = data4A[-301]) 

# Making the Confusion Matrix 
confusionMatrix(predict4A, factor(data4A[,301]))

# comment: accuracy: 63.92%




##### c (ii) ##### Different feature representation ##### 
## PCA with 1A
set.seed(123)
pca_train1A <- prcomp(train1A)

# The percent of the variances in data
variance_explained1A <- as.data.frame(pca_train1A$sdev^2/sum(pca_train1A$sdev^2)) 
variance_explained1A <- cbind(c(1:301), cumsum(variance_explained1A)) 
colnames(variance_explained1A) <- c("NmbrPCs","CumVar") 

plot (variance_explained1A$NmbrPCs, variance_explained1A$CumVar, 
      xlab = "Number of Factors", ylab = "Proportion of Variance Explained", 
      type = "l", col = "red")


# Look at 15th variance
variance_explained1A[15, ]



# Do PCA for creating 15 factors
set.seed(123)
pca1A <- preProcess(x = train1A[-301], method = "pca", pcaComp = 15)
train_pca1A <- predict(pca1A, train1A)
test_pca1A <- predict(pca1A, test1A)
str(train_pca1A[1:5,])
str(test_pca1A[1:5,])

# Building the classification model
svm_pca1A <- svm(formula = V1 ~ .,
                 data = train_pca1A,
                 type = "C-classification",
                 kernel = "linear", 
                 cost = 1, 
                 cross = 10)

# Predicting the test set results
pred_pca1A <-  predict(svm_pca1A, newdata = test_pca1A[-301])

# Making the confusion matrix
confusionMatrix(factor(pred_pca1A), factor(test_pca1A$V1))

# Accuracy 94.21%



## PCA with 1B
# Do PCA for creating 15 factors
pca1B <- preProcess(x = data1B[-301], method = "pca", pcaComp = 15)
data_set1B <- predict(pca1A, data1B)

# Predicting the results
pred_pca1B <-  predict(svm_pca1A, newdata = data_set1B[-301])

# Making the confusion matrix
confusionMatrix(factor(pred_pca1B), factor(data_set1B$V1))

# Accuracy 87.18%



## PCA with 2A
# Do PCA for creating 15 factors
pca2A <- preProcess(x = train2A[-301], method = "pca", pcaComp = 15)
train_pca2A <- predict(pca2A, train2A)
test_pca2A <- predict(pca2A, test2A)

# Building the classification model
svm_pca2A <- svm(formula = V1 ~ .,
                 data = train_pca2A,
                 type = "C-classification",
                 kernel = "linear", 
                 cost = 1, 
                 cross = 10)

# Predicting the test set results
pred_pca2A <-  predict(svm_pca2A, newdata = test_pca2A[-301])

# Making the confusion matrix
confusionMatrix(factor(pred_pca2A), factor(test_pca2A$V1))

# Accuracy 96.65%


## PCA with 2B
pca2B <- preProcess(x = data2B[-301], method = "pca", pcaComp = 15)
data_set2B <- predict(pca2A, data2B)

# Predicting the results
pred_pca2B <-  predict(svm_pca2A, newdata = data_set2B[-301])

# Making the confusion matrix
confusionMatrix(pred_pca2B, factor(data_set2B$V1))

# Accuracy 71.04%




