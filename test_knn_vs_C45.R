dat <- NULL
dat$train<-read.csv("file:///C:/Users/I069311/Documents/R/MASS/spambase-train.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
dat$test<-read.csv("file:///C:/Users/I069311/Documents/R/MASS/spambase-test.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
dat$label<-read.csv("file:///C:/Users/I069311/Documents/R/MASS/spambase-label.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

dat$train<-read.csv("file:///C:/Users/I069311/Documents/R/MASS/Iris-train.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
dat$test<-read.csv("file:///C:/Users/I069311/Documents/R/MASS/Iris-test.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
dat$label<-read.csv("file:///C:/Users/I069311/Documents/R/MASS/Iris-label.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")



dat$categoric <- paste("v",1:(length(dat$train)-1),sep="")
dat$target  <- c("classlabel")
dat$nlevel <- nlevels(factor(dat$train[,1]))
dat$level <- levels(factor(dat$train[,1]))

colnames(dat$train) <- c(dat$target,dat$categoric)
colnames(dat$test) <- dat$categoric
colnames(dat$label) <- dat$target


knnclass <- NULL
# library(FNN)
knnclass$pr <- class::knn(dat$train[c(dat$categoric)],dat$test,dat$train[,1], k=5)

for(i in 1:dat$nlevel) {
  knnclass$tp[i] <- sum(dat$label==dat$level[i] & t(knnclass$pr)==dat$level[i])
  knnclass$fp[i] <- sum(dat$label==dat$level[i] & t(knnclass$pr)!=dat$level[i])
  knnclass$tn[i] <- sum(dat$label!=dat$level[i] & t(knnclass$pr)!=dat$level[i])
  knnclass$fn[i] <- sum(dat$label!=dat$level[i] & t(knnclass$pr)==dat$level[i])
  knnclass$precision[i] <- knnclass$tp[i]/(knnclass$tp[i]+knnclass$fp[i])
  knnclass$recall[i] <- knnclass$tp[i]/(knnclass$tp[i]+knnclass$fn[i])
  knnclass$true_neg[i] <- knnclass$tn[i]/(knnclass$tn[i]+knnclass$fp[i])
  knnclass$accuracy[i] <- (knnclass$tn[i]+knnclass$tp[i])/(knnclass$tn[i]+knnclass$tp[i]+knnclass$fn[i]+knnclass$fp[i])
}
knnclass$precision.mean <- mean(knnclass$precision)
knnclass$recall.mean <- mean(knnclass$recall)
knnclass$accuracy.mean <- mean(knnclass$accuracy)

knnclass$result <- cbind(knnclass$precision, knnclass$recall, knnclass$accuracy)
colnames(knnclass$result)<-c("precision","recall","accuracy")
rownames(knnclass$result)<-dat$level

dt <- NULL
library(rpart)
library(RWeka)
# Build the Decision Tree model.

dt$rpart <- J48(classlabel ~ .,
                data=dat$train[,c(dat$categoric,dat$target)]
)
dt$pr <- predict(dt$rpart, cbind(dat$test, dat$label), type="class")

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
  



# library(partykit)
# library(CHAID)
# chi <- NULL
# # Build the CHAID model.
# chi$rchaid <- chaid(classlabel ~ .,
#                     data=dat$train[,c(dat$categoric,dat$target)]
# )
# chi$pr <- predict(chi$rchaid, cbind(dat$test, dat$label))
# #chi$pr <- predict(chi$rchaid, dat$train[,c(dat$categoric,dat$target)])  
# 
# for(i in 1:dat$nlevel) {
#   chi$tp[i] <- sum(dat$label==dat$level[i] & t(chi$pr)==dat$level[i])
#   chi$fp[i] <- sum(dat$label==dat$level[i] & t(chi$pr)!=dat$level[i])
#   chi$tn[i] <- sum(dat$label!=dat$level[i] & t(chi$pr)!=dat$level[i])
#   chi$fn[i] <- sum(dat$label!=dat$level[i] & t(chi$pr)==dat$level[i])
#   chi$precision[i] <- chi$tp[i]/(chi$tp[i]+chi$fp[i])
#   chi$recall[i] <- chi$tp[i]/(chi$tp[i]+chi$fn[i])
#   chi$true_neg[i] <- chi$tn[i]/(chi$tn[i]+chi$fp[i])
#   chi$accuracy[i] <- (chi$tn[i]+chi$tp[i])/(chi$tn[i]+chi$tp[i]+chi$fn[i]+chi$fp[i])
# }
# chi$precision.mean <- mean(chi$precision)
# chi$recall.mean <- mean(chi$recall)
# chi$accuracy.mean <- mean(chi$accuracy)
# 
# chi$result <- cbind(chi$precision, chi$recall, chi$accuracy)
# colnames(chi$result)<-c("precision","recall","accuracy")
# rownames(chi$result)<-dat$level









for(i in 1:dat$nlevel) {
  knnclass$tp[i] <- sum(dat$label==dat$level[i] & t(knnclass$pr)==dat$level[i])
  knnclass$fp[i] <- sum(dat$label==dat$level[i] & t(knnclass$pr)!=dat$level[i])
  knnclass$tn[i] <- sum(dat$label!=dat$level[i] & t(knnclass$pr)!=dat$level[i])
  knnclass$fn[i] <- sum(dat$label!=dat$level[i] & t(knnclass$pr)==dat$level[i])
  knnclass$precision[i] <- knnclass$tp[i]/(knnclass$tp[i]+knnclass$fp[i])
  knnclass$recall[i] <- knnclass$tp[i]/(knnclass$tp[i]+knnclass$fn[i])
  knnclass$true_neg[i] <- knnclass$tn[i]/(knnclass$tn[i]+knnclass$fp[i])
  knnclass$accuracy[i] <- (knnclass$tn[i]+knnclass$tp[i])/(knnclass$tn[i]+knnclass$tp[i]+knnclass$fn[i]+knnclass$fp[i])
}
knnclass$precision.mean <- mean(knnclass$precision)
knnclass$recall.mean <- mean(knnclass$recall)
knnclass$accuracy.mean <- mean(knnclass$accuracy)

knnclass$result <- cbind(knnclass$precision, knnclass$recall, knnclass$accuracy)
colnames(knnclass$result)<-c("precision","recall","accuracy")
rownames(knnclass$result)<-dat$level

list(KNN=knnclass$result, DecisionTree=dt$result)


if(knnclass$accuracy.mean > dt$accuracy.mean){
  list("KNN",knnclass$accuracy.mean)
}else{
  list("DecisionTree", dt$accuracy.mean)}


if(knnclass$precision.mean > dt$precision.mean){
  list("KNN",knnclass$precision.mean)
}else{
  list("DecisionTree",dt$precision.mean)
  
}

if(knnclass$recall.mean > dt$recall.mean){
  list("KNN", knnclass$recall.mean)
}else{
  list("DecisionTree", dt$recall.mean)}

