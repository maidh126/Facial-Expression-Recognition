##### b(i) 10% ##### Test on a single facial expression, using an off-the-shelf classifier, and comment on the results
# Load the User B data topics
data1B <- read.delim("b_topics_datapoints.txt", header=TRUE, sep=" ") 
target1B <- read.delim("b_topics_targets.txt", header=FALSE)

# Check the real number of positives and negatives
dim(data1B)
table(target1B)

# Add the labels (target dataset) onto the data frames 
data1B <- cbind(data1B, target1B)

# Remove the first column
data1B <- select(data1B, -X0.0) 

# Feature scaling
data1B[,-301] <- scale(data1B[,-301]) 

# Predicting the results 
predict1B <- predict(svm1A, newdata = data1B[-301]) 

# Making the Confusion Matrix 
confusionMatrix(predict1B, factor(data1B[,301]))

# comment: accuracy: 74.14%


##### b(ii) 5% ##### Test on a different facial expression, and comment on the difference
# Load the User B data conditional
data2B <- read.delim("b_conditional_datapoints.txt", header=TRUE, sep=" ") 
target2B <- read.delim("b_conditional_targets.txt", header=FALSE)

# Check the real number of positives and negatives
dim(data2B)
table(target2B)

# Add the labels (target dataset) onto the data frames 
data2B <- cbind(data2B, target2B)

# Remove the first column
data2B <- select(data2B, -X0.0) 

# Feature scaling 
data2B[,-301] <- scale(data2B[,-301]) 

# Predicting the results 
predict2B <- predict(svm2A, newdata = data2B[-301]) 

# Making the Confusion Matrix 
confusionMatrix(predict2B, factor(data2B[,301]))

# comment: accuracy: 71.04%


##### b(iii) 5% ##### Use of performance measures other than simple accuracy

# Ploting ROC curve for SVM 
rocplot = function(pred, truth, ...){
  predob = prediction(1-pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}

par(mfrow = c(1,4))


# Plot optimal parameter model's performance on testing 1A
opttest1A <- attributes(predict(svm1A, test1A, 
                                decision.values = TRUE))$decision.values
rocplot(opttest1A, test1A$V1, main = "Test on topics user A")
auc(test1A$V1,opttest1A) # 96.65%

# Plot optimal parameter model's performance on data1B
optdata1B = attributes(predict(svm1A, data1B, 
                               decision.values = TRUE))$decision.values
rocplot(optdata1B, data1B$V1, main = "Test on topics user B")
auc(data1B$V1,optdata1B) # 82.82%

# Plot optimal parameter model's performance on testing 2A
opttest2A <- attributes(predict(svm2A, test2A, 
                                      decision.values = TRUE))$decision.values
rocplot(opttest2A, test2A$V1, main = "Test on conditional user A")
auc(test2A$V1,opttest2A) # 99.42%

# Plot optimal parameter model's performance on data2B
optdata2B = attributes(predict(svm2A, data2B, 
                                     decision.values = TRUE))$decision.values
rocplot(optdata2B, data2B$V1, main = "Test on conditional user B")
auc(data2B$V1,optdata2B) # 83.74%
















