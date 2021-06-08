svm.fit = function(data1A, target1A, C=NULL) {
  n.samples = nrow(data1A)
  n.features = ncol(data1A)
  K = matrix(rep(0, n.samples*n.samples), nrow=n.samples)
  for (i in 1:n.samples){
    for (j in 1:n.samples){
      K[i,j] = data1A[i,] %*% data1A[j,] }}
  Dmat = outer(target1A,target1A) * K
  Dmat = as.matrix(nearPD(Dmat)$mat) 
  dvec = rep(1, n.samples)
  Amat = rbind(target1A, diag(n.samples), -1*diag(n.samples))
  bvec = c(0, rep(0, n.samples), rep(-C, n.samples))
  res = solve.QP(Dmat,dvec,t(Amat),bvec=bvec, meq=1)
  a = res$solution 
  bomega = apply(a*target1A*data1A,2,sum)
  return(bomega)
}






get_pred_svm<-function(train1A,test1A)
{
  
  last.index <- ncol(train1A)
  names(train1A)[last.index] <- "last"
  svm.model <- svm(as.factor(last)~.,data=train1A,probability=TRUE)
  predicted.svm <- predict(svm.model,test1A,probability=TRUE)
  test.true <- test1A[,last.index]
  predicted.svm.prob <- as.data.frame(attr(predicted.svm,"probabilities"))
  predicted.svm.prob.high <-  as.data.frame(predicted.svm.prob[,1])
  colnames(predicted.svm.prob.high) <- "predicted.svm.prob.high"
  svm.result<- as.data.frame(cbind(as.data.frame(predicted.svm.prob.high),as.data.frame(test.true)))
  return(svm.result)
  
}









svm.formula <-
  function (formula, data = NULL, ..., subset, na.action = na.omit, scale = TRUE)
  {
    call <- match.call()
    if (!inherits(formula, "formula"))
      stop("method is only for formula objects")
    m <- match.call(expand.dots = FALSE)
    if (inherits(eval.parent(m$data), "matrix"))
      m$data <- as.data.frame(eval.parent(m$data))
    m$... <- NULL
    m$scale <- NULL
    m[[1L]] <- quote(stats::model.frame)
    m$na.action <- na.action
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0
    x <- model.matrix(Terms, m)
    y <- model.extract(m, "response")
    attr(x, "na.action") <- attr(y, "na.action") <- attr(m, "na.action")
    if (length(scale) == 1)
      scale <- rep(scale, ncol(x))
    if (any(scale)) {
      remove <- unique(c(which(labels(Terms) %in%
                                 names(attr(x, "contrasts"))),
                         which(!scale)
      )
      )
      scale <- !attr(x, "assign") %in% remove
    }
    ret <- svm.default (x, y, scale = scale, ..., na.action = na.action)
    ret$call <- call
    ret$call[[1]] <- as.name("svm")
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
      ret$na.action <- attr(m, "na.action")
    class(ret) <- c("svm.formula", class(ret))
    return (ret)
  }





# Random forest https://stackoverflow.com/questions/45205296/how-to-create-random-forest-from-scratch-in-r-without-the-randomforest-package
library(rpart)
library(Metrics)
library(doParallel)
library(foreach)
library(ggplot2)


random_forest <- function(train_data, train_formula, method="class", feature_per=0.7, cp=0.01, min_split=20, min_bucket=round(min_split/3), max_depth=30, ntrees = 10) {
  
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
    trControl <- rpart.control(minsplit = min_split, minbucket = min_bucket, cp = cp, maxdepth = max_depth)
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

train_formula <- as.formula("Species ~ .")
forest <- random_forest(train_data = iris, train_formula = train_formula)








svm_gradient<- function(x,eta=0.001,R=10000){
  X<- cbind(1,x)#make design matrix
  n <- nrow(X)  #number of sample
  p <- ncol(X) #number of feature+1 (bias)
  w_intial <- rep(0,p)
  W <- matrix(w_intial ,nrow = R+1,ncol = p,byrow = T) #matrix put intial guess and the procedure to do gradient descent
  for(i in 1:R){
    for(j in 1:p)
    {
      W[i+1,j]<- W[i,j]+eta*sum(((y*(X%*%W[i,]))<1)*1 * y * X[,j] )  
    }
  }
  return(W)  
}
getsvm <- function(x){
  w_answer<- svm_gradient(x)[nrow(svm_gradient(x)),]
  return(w_answer )
}

x = data1A
y = target1A
w_answer<- getsvm(x)









