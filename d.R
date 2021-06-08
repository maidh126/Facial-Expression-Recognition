library(class)
library(caTools)
library(caret)
library(randomForest)
library(readr)
library(lattice)


##### d(i) 8% ##### Training randomforest on 1A expression ##### 
# Load the User A data topics
data1A <- read.delim("a_topics_datapoints.txt", header=TRUE, sep=" ") 
target1A <- read.delim("a_topics_targets.txt", header=FALSE)

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

# Random Forest 
set.seed(123)

# Generate a random sample of "numTrain" indexes
rows1A <- sample(1:nrow(train1A), 500) 

# Random forest model
rf1A <- randomForest(train1A, factor(train1A[,301]), ntree=100)

# Plot
plot(rf1A) 

# Make prediction
predict1A <- predict(rf1A, test1A)

# Confusion Matrix
confusionMatrix(predict1A, factor(test1A[,301]))

# comment: accuracy 98%



##### d(ii) 4% ##### Test on 1B expression ##### 
data1B <- read.delim("b_topics_datapoints.txt", header=TRUE, sep=" ") 
target1B <- read.delim("b_topics_targets.txt", header=FALSE)

# Add the labels (target dataset) onto the data frames 
data1B <- cbind(data1B, target1B)

# Remove the first column
data1B <- select(data1B, -X0.0) 

# Feature scaling
set.seed(123)
data1B[,-301] <- scale(data1B[,-301]) 

# Predicting 
predict1B <- predict(rf1A, data1B) 

# Making the Confusion Matrix 
confusionMatrix(predict1B, factor(data1B[,301]))

# comment: accuracy: 93.86%


##### d(iii) ##### Own implementation of the second classifier ##### 
random_forest <- function(train_data, train_formula, method="class", 
                          feature_per=0.7, cp=0.01, min_split=20, 
                          min_bucket=round(min_split/3), max_depth=30, ntrees = 10) 
  {
  
  target_variable <- as.character(train_formula)[[2]]
  features <- setdiff(colnames(train_data), target_variable)
  n_features <- length(features)
  
  ncores <- detectCores(logical=FALSE)
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  rf_model <- foreach(
    icount(ntrees),
    .packages = c("rpart", "Metrics")
  ) %dopar% {
    bagged_features <- sample(features, n_features * feature_per, replace = FALSE)
    index_bag <- sample(nrow(train_data), replace=TRUE)
    in_train_bag <- train_data[index_bag,]
    out_train_bag <- train_data[-index_bag,]
    trControl <- rpart.control(minsplit = min_split, minbucket = min_bucket, 
                               cp = cp, maxdepth = max_depth)
    tree <- rpart(formula = train_formula, 
                  data = in_train_bag, 
                  control = trControl)
    
    oob_pred <- predict(tree, newdata = out_train_bag, type = "class")
    oob_acc <- accuracy(actual = out_train_bag[, target_variable], predicted = oob_pred)
    
    list(tree=tree, oob_perf=oob_acc)
  }
  stopCluster(cl)
  rf_model
}




##### d(iv) 3% ##### Training RF on conditional 2A expression ##### 
# Load the User A data conditional
data2A <- read.delim("a_conditional_datapoints.txt", header=TRUE, sep=" ") 
target2A <- read.delim("a_conditional_targets.txt", header=FALSE)

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
train2A[,-301] <- scale(train2A[,-301])  
test2A[,-301] <- scale(test2A[,-301])  

## Random Forest 
set.seed(123)

# Generate a random sample of "numTrain" indexes
rows2A <- sample(1:nrow(train2A), 500) 

# Random forest model
rf2A <- randomForest(train2A, factor(train2A[,301]), ntree=100)

# Plot
plot(rf2A) 

# Make prediction
predict2A <- predict(rf2A, test2A)

# Confusion Matrix
confusionMatrix(predict2A, factor(test2A[,301]))

# comment: accuracy 99.16%



##### d(v) 3% ##### Test on 2B conditional expression ##### 
data2B <- read.delim("b_conditional_datapoints.txt", header=TRUE, sep=" ") 
target2B <- read.delim("b_conditional_targets.txt", header=FALSE)

# Add the labels (target dataset) onto the data frames 
data2B <- cbind(data2B, target2B)

# Remove the first column
data2B <- select(data2B, -X0.0) 

# Feature scaling 
set.seed(123)
data2B[,-301] <- scale(data2B[,-301])  

# Predicting 
predict2B <- predict(rf2A, data2B) 

# Making the Confusion Matrix 
confusionMatrix(predict2B, factor(data2B[,301]))

# comment: accuracy 91.64%




##### d(vi) 3% ##### Inverting the roles of the user for B -> A ##### 
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

# Random Forest 
set.seed(123)

# Generate a random sample of "numTrain" indexes
rows3B <- sample(1:nrow(train3B), 500) 

# Random forest model
rf3B <- randomForest(train3B, factor(train3B[,301]), ntree=100)

# Plot
plot(rf3B) 

# Make prediction
predict3B <- predict(rf3B, test3B)

# Confusion Matrix
confusionMatrix(predict3B, factor(test3B[,301]))

# comment: accuracy 97.16%


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
predict3A <- predict(rf3B, data3A) 

# Making the Confusion Matrix 
confusionMatrix(predict3A, factor(data3A[,301]))

# comment: accuracy: 96.1% 



## Training RF on conditional 2B (called 4B) expression
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

## Random Forest 
set.seed(123)

# Generate a random sample of "numTrain" indexes
rows4B <- sample(1:nrow(train4B), 500) 

# Random forest model
rf4B <- randomForest(train4B, factor(train4B[,301]), ntree=100)

# Plot
plot(rf4B) 

# Make prediction
predict4B <- predict(rf4B, test4B)

# Confusion Matrix
confusionMatrix(predict4B, factor(test4B[,301]))

# comment: accuracy 97.64%



## Test on 2A (called 4A) conditional expression
data4A <- read.delim("a_conditional_datapoints.txt", header=TRUE, sep=" ") 
target4A <- read.delim("a_conditional_targets.txt", header=FALSE)

# Add the labels (target dataset) onto the data frames 
data4A <- cbind(data4A, target4A)

# Remove the first column
data4A <- select(data4A, -X0.0) 

# Feature scaling
data4A[,-301] <- scale(data4A[,-301])  

# Predicting 
predict4A <- predict(rf4B, data4A) 

# Making the Confusion Matrix 
confusionMatrix(predict4A, factor(data4A[,301]))

# comment: accuracy: 94.65%




##### d(vii) 3% ##### Different feature representation ##### 
## PCA with 1A
set.seed(123)
pca_train1A <- prcomp(train1A)

# The percent of the variances in data
variance_explained1A <- as.data.frame(pca_train1A$sdev^2/sum(pca_train1A$sdev^2)) 
variance_explained1A <- cbind(c(1:301), cumsum(variance_explained1A)) 
colnames(variance_explained1A) <- c("NmbrPCs","CumVar") 

plot (variance_explained1A$NmbrPCs, variance_explained1A$CumVar, 
      xlab = "Number of Factors 1A dataset", ylab = "Proportion of Variance Explained", 
      type = "l", col = "red")


# Look at 10th variance
variance_explained1A[10, ]


# Do PCA for creating 10 factors
pca1A <- preProcess(x = train1A[-301], method = "pca", pcaComp = 10)
train_pca1A <- predict(pca1A, train1A)
test_pca1A <- predict(pca1A, test1A)
head(train_pca1A)
head(test_pca1A)

# Generate a random sample of "numTrain" indexes
pca_rows1A <- sample(1:nrow(train1A), 500) 

# Random forest model
pca_rf1A <- randomForest(train1A, factor(train1A[,301]), ntree=100)

# Plot
plot(pca_rf1A) 

# Make prediction
pred1A <- predict(pca_rf1A, test1A)

# Confusion Matrix
confusionMatrix(pred1A, factor(test1A[,301]))

# Accuracy 98%



## PCA with 1B
# Do PCA for creating 10 factors
pca1B <- preProcess(x = data1B[-301], method = "pca", pcaComp = 10)
data_set1B <- predict(pca1A, data1B)

# Predicting the results
pred1B <-  predict(pca_rf1A, data1B)

# Making the confusion matrix
confusionMatrix(factor(pred1B), factor(data_set1B$V1))

# Accuracy 93.81% 


## PCA with 2A
set.seed(123)
pca_train2A <- prcomp(train2A)

# The percent of the variances in data
variance_explained2A <- as.data.frame(pca_train2A$sdev^2/sum(pca_train2A$sdev^2)) 
variance_explained2A <- cbind(c(1:301), cumsum(variance_explained2A)) 
colnames(variance_explained2A) <- c("NmbrPCs","CumVar") 

plot (variance_explained2A$NmbrPCs, variance_explained2A$CumVar, 
      xlab = "Number of Factors", ylab = "Proportion of Variance Explained", 
      type = "l", col = "red")


# Look at 10th variance
variance_explained2A[10, ]


# Do PCA for creating 10 factors
pca2A <- preProcess(x = train2A[-301], method = "pca", pcaComp = 10)
train_pca2A <- predict(pca2A, train2A)
test_pca2A <- predict(pca2A, test2A)

# Generate a random sample of "numTrain" indexes
pca_rows2A <- sample(1:nrow(train2A), 500) 

# Random forest model
pca_rf2A <- randomForest(train2A, factor(train2A[,301]), ntree=100)

plot(pca_rf2A) 

# Make prediction
pred2A <- predict(pca_rf2A, test2A)

# Confusion Matrix
confusionMatrix(pred2A, factor(test2A[,301]))

# Accuracy 99.16%


## PCA with 2B
# Do PCA for creating 10 factors
pca2B <- preProcess(x = data1B[-301], method = "pca", pcaComp = 10)
data_set2B <- predict(pca2A, data2B)

# Predicting the results
pred2B <-  predict(pca_rf2A, data2B)

# Making the confusion matrix
confusionMatrix(pred2B, factor(data2B$V1))

# Accuracy 91.79% 



##### d(viii) 3% ##### Use of performance measures other than simple accuracy ##### 
par(mfrow = c(1,4))

# Plot optimal parameter model's performance on training data 1A
rf.roc1A<-roc(train1A$V1,rf1A$votes[,2])
plot(rf.roc1A, main = "Test on topics person A")
auc(rf.roc1A) # auc: 0.9989

# Plot optimal parameter model's performance on data 1B
rf.roc1B<-roc(data1B$V1,as.numeric(predict1B)) 
plot(rf.roc1B, main = "Test on topics person B")
auc(rf.roc1B) # auc: 0.92.43

# Plot optimal parameter model's performance on training data 2A
rf.roc2A<-roc(train2A$V1,rf2A$votes[,2])
plot(rf.roc2A, main = "Test on conditional person A")
auc(rf.roc2A) # auc: 0.999

# Plot optimal parameter model's performance on data 2B
rf.roc2B<-roc(data2B$V1,as.numeric(predict2B)) 
plot(rf.roc2B, main = "Test on conditional person B")
auc(rf.roc2B) # auc: 0.8733





