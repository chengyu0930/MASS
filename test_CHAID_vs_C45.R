
dat <- NULL
dat$train<-read.csv("file:///C:/Users/I069311/Documents/R/MASS/house-votes-train.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
dat$test<-read.csv("file:///C:/Users/I069311/Documents/R/MASS/house-votes-test.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
dat$label<-read.csv("file:///C:/Users/I069311/Documents/R/MASS/house-votes-label.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

  
  dat$categoric <- paste("v",1:(length(dat$train)-1),sep="")
  dat$target  <- c("classlabel")
  dat$nlevel <- nlevels(dat$train[,1])
  dat$level <- levels(dat$train[,1])
  
  colnames(dat$train) <- c(dat$target,dat$categoric)
  colnames(dat$test) <- dat$categoric
  colnames(dat$label) <- dat$target


# Missing value imputation
imptrain <- mice(dat$train,seed=1)
dat$train <- complete(imptrain)
imptest <- mice(dat$test, seed=1)
dat$test <- complete(imptest)

  dt <- NULL
  library(rpart)
  library(RWeka)
  # Build the Decision Tree model.
  dt$rpart <- J48(classlabel ~ .,
                    data=dat$train[,c(dat$categoric,dat$target)]
  )
  dt$pr <- predict(dt$rpart, cbind(dat$test, dat$label), type="class")
#dt$pr <- predict(dt$rpart, dat$train[,c(dat$categoric,dat$target)], type="class")
  
  for(i in 1:dat$nlevel) {
    dt$tp[i] <- sum(dat$label==dat$level[i] & t(dt$pr)==dat$level[i])
    dt$fp[i] <- sum(dat$label==dat$level[i] & t(dt$pr)!=dat$level[i])
    dt$tn[i] <- sum(dat$label!=dat$level[i] & t(dt$pr)!=dat$level[i])
    dt$fn[i] <- sum(dat$label!=dat$level[i] & t(dt$pr)==dat$level[i])
    dt$precision[i] <- dt$tp[i]/(dt$tp[i]+dt$fp[i])
    dt$recall[i] <- dt$tp[i]/(dt$tp[i]+dt$fn[i])
    dt$true_neg[i] <- dt$tn[i]/(dt$tn[i]+dt$fp[i])
    dt$accuracy[i] <- (dt$tn[i]+dt$tp[i])/(dt$tn[i]+dt$tp[i]+dt$fn[i]+dt$fp[i])
  }
  dt$precision.mean <- mean(dt$precision)
  dt$recall.mean <- mean(dt$recall)
  dt$accuracy.mean <- mean(dt$accuracy)
  
  dt$result <- cbind(dt$precision, dt$recall, dt$accuracy)
  colnames(dt$result)<-c("precision","recall","accuracy")
  rownames(dt$result)<-dat$level
  
  
  library(partykit)
  library(CHAID)
  chi <- NULL
  # Build the CHAID model.
  chi$rchaid <- chaid(classlabel ~ .,
                      data=dat$train[,c(dat$categoric,dat$target)]
  )
  chi$pr <- predict(chi$rchaid, cbind(dat$test, dat$label))
#chi$pr <- predict(chi$rchaid, dat$train[,c(dat$categoric,dat$target)])  

  for(i in 1:dat$nlevel) {
    chi$tp[i] <- sum(dat$label==dat$level[i] & t(chi$pr)==dat$level[i])
    chi$fp[i] <- sum(dat$label==dat$level[i] & t(chi$pr)!=dat$level[i])
    chi$tn[i] <- sum(dat$label!=dat$level[i] & t(chi$pr)!=dat$level[i])
    chi$fn[i] <- sum(dat$label!=dat$level[i] & t(chi$pr)==dat$level[i])
    chi$precision[i] <- chi$tp[i]/(chi$tp[i]+chi$fp[i])
    chi$recall[i] <- chi$tp[i]/(chi$tp[i]+chi$fn[i])
    chi$true_neg[i] <- chi$tn[i]/(chi$tn[i]+chi$fp[i])
    chi$accuracy[i] <- (chi$tn[i]+chi$tp[i])/(chi$tn[i]+chi$tp[i]+chi$fn[i]+chi$fp[i])
  }
  chi$precision.mean <- mean(chi$precision)
  chi$recall.mean <- mean(chi$recall)
  chi$accuracy.mean <- mean(chi$accuracy)
  
  chi$result <- cbind(chi$precision, chi$recall, chi$accuracy)
  colnames(chi$result)<-c("precision","recall","accuracy")
  rownames(chi$result)<-dat$level
  

  list(CHAID=chi$result, DecisionTree=dt$result)


  if(chi$accuracy.mean > dt$accuracy.mean){
    list("CHAID",chi$accuracy.mean)
  }else{
    list("DecisionTree", dt$accuracy.mean)}


  if(chi$precision.mean > dt$precision.mean){
    list("CHAID",chi$precision.mean)
  }else{
    list("DecisionTree",dt$precision.mean)
  
}

  if(chi$recall.mean > dt$recall.mean){
    list("CHAID", chi$recall.mean)
  }else{
    list("DecisionTree", dt$recall.mean)}
  
