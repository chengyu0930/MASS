#######################2013.6.21 updated ##################################  
loadFullData<-function(filenumber)
{
  setwd(dir_name)
  dat <- NULL
  if(filenumber == 1)
  {
    dat$dataset<-read.csv("house-votes-train.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
    dat$newdata <- read.csv("house-votes-test.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
    dat$label <- read.csv("house-votes-label.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
  }
  else if(filenumber == 2)
  {
    dat$dataset<-read.csv("Iris-train.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
    dat$newdata <- read.csv("Iris-test.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
    dat$label <- read.csv("Iris-label.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
  }
    else if(filenumber == 3)
  {
    dat$dataset<-read.csv("spambase-train.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
    dat$newdata <- read.csv("spambase-test.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
    dat$label <- read.csv("spambase-label.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
  }
  else
  {
    dat$dataset<-read.csv("market-churn-train-sample.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
    dat$newdata<-read.csv("market-churn-test.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
    dat$label <- read.csv("market-churn-label.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
  }
  
  return(dat);  
}

prepareLabelsNamesData<-function(dat)
{
  if(!is.factor(dat$dataset[,1]))
  {
    dat$dataset[,1] <- as.factor(dat$dataset[,1])  # guarantee the label column is a factor
  }
  
  dat$categoric <- paste("v",1:(length(dat$dataset)-1),sep="")
  dat$target  <- c("classlabel")
  dat$nlevel <- nlevels(dat$dataset[,1])
  dat$level <- levels(dat$dataset[,1])
  colnames(dat$dataset) <- c(dat$target,dat$categoric)
  colnames(dat$newdata) <- c(dat$categoric)
  return(dat);
}

missingValuesImputation<-function(dat)
{
  if(length(which(is.na(dat)))!=0)
  {
    impdata <- mice(dat,seed=1)
    dat <- complete(impdata)
  } 
  return(dat);
}
  
splitDataTrainingTestValidation<-function(dat,perc_train)
{
    dat$seed <- 42 
    set.seed(dat$seed) 
    dat$nobs <- nrow(dat$dataset) 
    dat$train <- sample(nrow(dat$dataset), perc_train*dat$nobs) 
    dat$test <- setdiff(seq_len(nrow(dat$dataset)), dat$train) 

    dat$train <- dat$dataset[dat$train,]
    dat$test <- dat$dataset[dat$test,]
    return(dat)
  }
#####################################2013.6.21 updated and tested ################################## 
replaceNumericalValues <- function(dat)  
{
  dis_data <- dat
#   library(discretization)
#   dis_data <- chiM(dat)
  nb_col<-dim(dat)[2]
  for(i in 2:nb_col)
  {
   if(is.numeric(dat[,i]))
   {
     dis_data[,i]<-cut(as.numeric(dat[,i]),breaks=10)
   }
  } 
  return(dis_data)
}



replaceCategoricalValues<-function(dat)
{
  dat_processed<-dat
  nb_col<-dim(dat)[2]
  for(i in 2:nb_col)
  {
    if(!is.numeric(dat[,i]))
    {
      dat_processed[,i]<-as.numeric(dat[,i])
      #print(paste("categorical values replaced for attribute ",i))
    }
  }
  return(dat_processed)
}


################## Feature selection
# evaluator <- function(subset) 
# {
#   tree <- J48(as.simple.formula(subset, "classlabel"), dat_train)
#   error.rate = sum(dat_validate[,ncol(dat_validate)] != predict(tree, dat_validate, type="class")) / nrow(dat_validate)
#   return(1 - error.rate)
# }

featureSelection <- function(dat)
{
  library(FSelector)
#   weights <- chi.squared(classlabel~., dat$dataset)
#   weights <- linear.correlation(classlabel~., dat$dataset)
#   weights <- information.gain(classlabel~., dat$dataset)
  
  subset <- cfs(classlabel~., dat$train[,c(dat$categoric,dat$target)])
  return(subset)
}

#######################2013.6.21 updated ################################## 
prediction_KPIs<-function(dat,class.pr)
{
  model <- NULL
  for(i in 1:dat$nlevel) {
    dat$label <- dat$test[,1]
    model$tp[i] <- sum(dat$label==dat$level[i] & t(class.pr)==dat$level[i])
    model$fp[i] <- sum(dat$label==dat$level[i] & t(class.pr)!=dat$level[i])
    model$tn[i] <- sum(dat$label!=dat$level[i] & t(class.pr)!=dat$level[i])
    model$fn[i] <- sum(dat$label!=dat$level[i] & t(class.pr)==dat$level[i])
    model$precision[i] <- model$tp[i]/(model$tp[i]+model$fp[i])
    model$recall[i] <- model$tp[i]/(model$tp[i]+model$fn[i])
    model$true_neg[i] <- model$tn[i]/(model$tn[i]+model$fp[i])
    model$accuracy[i] <- (model$tn[i]+model$tp[i])/(model$tn[i]+model$tp[i]+model$fn[i]+model$fp[i])
  }
  model$precision.mean <- mean(model$precision)
  model$recall.mean <- mean(model$recall)
  model$accuracy.mean <- mean(model$accuracy)
  model$result <- cbind(model$precision, model$recall, model$accuracy)
  colnames(model$result)<-c("precision","recall","accuracy")
  rownames(model$result)<-dat$level
  return(model);
  
}

########## DT flow
dt_Prediction<-function(dat)
{
  dt <- NULL
  library(rpart)
  library(RWeka)
  library(rJava)
  # Build the Decision Tree model.
  class.model <- J48(classlabel ~ ., data=dat$train[,c(dat$categoric,dat$target)])
  .jcache(class.model$classifier)
  class.pr <- predict(class.model, dat$test[,c(dat$categoric, dat$target)], type="class")
  dt<-prediction_KPIs(dat,class.pr);
  dt$model <- class.model
  return(dt);
}

DTflow <- function(dat, para_mis=0, para_feaSele=0) 
{
  if(para_mis!= 0)
  {
    dat$dataset <- missingValuesImputation(dat$dataset)
  } 
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6)
  if(para_feaSele != 0)
  {
    dat$categoric <- featureSelection(dat)
  }
  dt<-dt_Prediction(dat)
}


#######################2013.6.23 updated ################################## 
########## CHAID flow
CHAID_Prediction<-function(dat)
{
  library(grid)
  library(partykit)
  library(CHAID)
  chi <- NULL
  # Build the CHAID model.
  class.model <- chaid(classlabel ~ ., data=dat$train[,c(dat$categoric,dat$target)])
  class.pr <- predict(class.model, dat$test[,c(dat$categoric, dat$target)])
  chi<-prediction_KPIs(dat,class.pr);
  chi$model <- class.model
  return(chi)
}

CHAIDflow <- function(dat, para_mis=0, para_discret=1, para_feaSele = 0) 
{
  if(para_mis!= 0)
  {
    dat$dataset <- missingValuesImputation(dat$dataset)
  } 
  if(para_discret!=0)
  {
    j <- 0
    for(i in 2:ncol(dat$dataset))
    {
      if(is.numeric(dat$dataset[,i]))
      {
        j<-1
        break
      }
    }
    if(j!=0)
    {
      dat$dataset <- replaceNumericalValues(dat$dataset[,c(dat$categoric,dat$target)])
      dat$dataset <- dat$dataset[,c(dat$target,dat$categoric)]
    }
  }
  for(i in 2:ncol(dat$dataset)){
    if(!is.factor(dat$dataset[,i]))
    {
      dat$dataset[,i] <- as.factor(dat$dataset[,i])  # guarantee the label column is a factor
    }
  }
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6)  
  if(para_feaSele != 0)
  {
    dat$categoric <- featureSelection(dat)
  }
  chi<-CHAID_Prediction(dat)
}


########## ADA flow
ADA_Prediction<-function(dat)
{
  library(rpart)
  library(ada)
  ADA <- NULL
  # Build the CHAID model.
  class.model <- ada(classlabel ~ .,
                  data=dat$train[,c(dat$categoric,dat$target)]
  )
  class.pr <- predict(class.model, dat$test[,c(dat$categoric, dat$target)])
  ADA<-prediction_KPIs(dat,class.pr);
  ADA$model <- class.model
  return(ADA)
}

ADAflow <- function(dat, para_mis=0, para_discret=0, para_feaSele = 0) 
{
  if(para_mis!= 0)
  {
    dat$dataset <- missingValuesImputation(dat$dataset)
  } 
  if(para_discret!=0)
  {
    j <- 0
    for(i in 2:ncol(dat$dataset))
    {
      if(is.numeric(dat$dataset[,i]))
      {
        j<-1
        break
      }
    }
    if(j!=0)
    {
      dat$dataset <- replaceNumericalValues(dat$dataset[,c(dat$categoric,dat$target)])
      dat$dataset <- dat$dataset[,c(dat$target,dat$categoric)]
    }
  }
#   for(i in 2:ncol(dat$dataset)){
#     if(!is.factor(dat$dataset[,i]))
#     {
#       dat$dataset[,i] <- as.factor(dat$dataset[,i])  # guarantee the label column is a factor
#     }
#   }
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6)  
  if(para_feaSele != 0)
  {
    dat$categoric <- featureSelection(dat)
  }
  ada<-ADA_Prediction(dat)
}


########## SVM flow
SVM_Prediction<-function(dat)
{
  library(kernlab)
  SVM<-NULL
  class.model<-ksvm(classlabel ~ .,
                 data=dat$train[,c(dat$categoric,dat$target)],kernel="rbfdot",prob.model=TRUE)
  class.pr <- predict(class.model, dat$test[,c(dat$categoric, dat$target)])
  SVM<-prediction_KPIs(dat,class.pr);
  SVM$model <- class.model
  return(SVM)
}

SVMflow <- function(dat, para_mis=0, para_discret=0, para_feaSele=1) 
{
  if(para_mis!= 0)
  {
    dat$dataset <- missingValuesImputation(dat$dataset)
  } 
  if(para_discret!=0)
  {
    j <- 0
    for(i in 2:ncol(dat$dataset))
    {
      if(is.numeric(dat$dataset[,i]))
      {
        j<-1
        break
      }
    }
    if(j!=0)
    {
      dat$dataset <- replaceNumericalValues(dat$dataset[,c(dat$categoric,dat$target)])
      dat$dataset <- dat$dataset[,c(dat$target,dat$categoric)]
    }
  }
 
  for(i in 2:ncol(dat$dataset)){
    if(!is.factor(dat$dataset[,i]))
    {
      dat$dataset[,i] <- as.factor(dat$dataset[,i])  # guarantee the label column is a factor
    }
  }
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6)  
  if(para_feaSele != 0)
  {
    dat$categoric <- featureSelection(dat)
  }
  svm<-SVM_Prediction(dat)
}


########## KNN flow
KNN_Prediction<-function(dat)
{ 
  knnclass <- NULL
  k<-5
  knnclass.pr <- class::knn(dat$train[c(dat$categoric)],dat$test[c(dat$categoric)],dat$train[,1], k)
  knnclass<-prediction_KPIs(dat,knnclass.pr);
  knnclass$model<-k
  return(knnclass)
}

KNNflow <- function(dat, para_feaSele=1) 
{
  dat$dataset <- missingValuesImputation(dat$dataset)
  dat$dataset <- replaceCategoricalValues(dat$dataset)
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6)  
  if(para_feaSele != 0)
  {
    dat$categoric <- featureSelection(dat)
  }
  knn<-KNN_Prediction(dat)
}

#######################2013.6.23 updated and tested ################################## 

#######################2013.6.24 updated ################################## 
########## FR flow
RF_Prediction<-function(dat)
{
  library(randomForest)
  RF <- NULL
  # Build the RF model.
  class.model <- randomForest(classlabel ~ .,
                        data=dat$train[,c(dat$categoric,dat$target)],mtry=3,
                        importance=TRUE,
                        proximity=TRUE)
  
  class.pr <- predict(class.model, dat$test[,c(dat$categoric, dat$target)])
  RF<-prediction_KPIs(dat, class.pr);
  RF$model <- class.model
  return(RF)
}

RFflow <- function(dat, para_mis=0, para_discret=1, para_feaSele = 0) 
{
  if(para_mis!= 0)
  {
    dat$dataset <- missingValuesImputation(dat$dataset)
  } 
  if(para_discret!=0)
  {
    j <- 0
    for(i in 2:ncol(dat$dataset))
    {
      if(is.numeric(dat$dataset[,i]))
      {
        j<-1
        break
      }
    }
    if(j!=0)
    {
      dat$dataset <- replaceNumericalValues(dat$dataset[,c(dat$categoric,dat$target)])
      dat$dataset <- dat$dataset[,c(dat$target,dat$categoric)]
    }
  }
#   for(i in 2:ncol(dat$dataset)){
#     if(!is.factor(dat$dataset[,i]))
#     {
#       dat$dataset[,i] <- as.factor(dat$dataset[,i])  # guarantee the label column is a factor
#     }
#   }
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6)  
  if(para_feaSele != 0)
  {
    dat$categoric <- featureSelection(dat)
  }
  rf<-RF_Prediction(dat)
}

# model comparison and choice
modelComparison<-function(models,priority)
{
  nb_models<-length(models)
  if(nb_models>1)
  {  
      if(priority==1) ## 1 : accuracy
      {
        best_model_id<-1
        best_accuracy_mean<-models[[1]]$accuracy.mean
        ##finding the best model who gives the highest accuracy
        for(i in 2:nb_models)
        {
          if(models[[i]]$accuracy.mean>best_accuracy_mean)
          {
            best_accuracy_mean<-models[[i]]$accuracy.mean
            best_model_id<-i
          }
        }
        best_model<-models[[best_model_id]]$model
        save(best_model, file=paste(dir_name, "bestmodel.RData", sep=""))
        print("The best model from the accuracy point of view is model")
        print(best_model)
        print("The accuracy of the model is:")
        print(best_accuracy_mean)
      }else if(priority==2){      ## 2 : precision
        best_model_id<-1
        best_precision_mean<-models[[1]]$precision.mean     
        for(i in 2:nb_models)
        {
          if(models[[i]]$precision.mean>best_precision_mean)
          {
            best_precision_mean<-models[[i]]$precision.mean
            best_model_id<-i
          }
        }
        best_model<-models[[best_model_id]]$model
        save(best_model, file=paste(dir_name, "bestmodel.RData", sep=""))
        print("The best model from the precision point of view is model")
        print(best_model)
        print("The precision of the model is:")
        print(best_precision_mean)
      }
      else{ ## 3 : recall
        best_model_id<-1
        best_recall_mean<-models[[1]]$recall.mean     
        for(i in 2:nb_models)
        {
          if(models[[i]]$recall.mean>best_recall_mean)
          {
            best_recall_mean<-models[[i]]$recall.mean
            best_model_id<-i
          }
        }
        best_model<-models[[best_model_id]]$model
        save(best_model, file=paste(dir_name, "bestmodel.RData", sep=""))
        print("The best model from the recall point of view is model")
        print(best_model)
        print("The recall of the model is:")
        print(best_recall_mean)
      }
  }
  else{
    best_model<-models[[1]]$model
    save(best_model, file=paste(dir_name, "bestmodel.RData", sep=""))
    print("The best model from the accuracy point of view is model")
    
    print(0)
    print("The accuracy of the model is:")
    print(models[[1]]$accuracy.mean)
  }
}
#######################2013.6.24 updated and tested ################################## 







######################## 2013.6.21 Data Preparation ################################
model_list<-NULL
dir_name<<-"C:/Users/I069311/Documents/R/MASS/"
#dir_name<<-"C:/Users/i074677/Documents/R/MASS_PAL/"
######################## 
data_loaded <- loadFullData(1); #loading file number ...
data_processed <- prepareLabelsNamesData(data_loaded)

####################### 2013.6.21 updated and tested ############################### 
####################### 2013.6.25 DT Flow ################################ 
model_list$dt <- DTflow(data_processed, para_mis=1, para_feaSele=0)

####################### 2013.6.25 updated and tested DT Flow ####################### 

####################### 2013.6.22 CHAID Flow ################################ 
#print("Warning: Long time will be consumed for big numerical-type data")
model_list$chi <- CHAIDflow(data_processed,para_mis=0, para_discret=1, para_feaSele=1) 
####################### 2013.6.22 updated and tested CHAID Flow ####################### 

####################### 2013.6.25 SVM Flow ################################ 
model_list$svm <-svm <- SVMflow(data_processed,para_mis=0, para_discret=0, para_feaSele=0)
####################### 2013.6.25 updated and tested SVM Flow ####################### 

####################### 2013.6.23 ADA Flow ################################ 
#=================== with a bug of para_feaSele=1 on dataset 1 and 2
model_list$ada <-ada <- ADAflow(data_processed,para_mis=0, para_discret=1, para_feaSele=0)
####################### 2013.6.22 updated and tested ADA Flow ####################### 

####################### 2013.6.25 KNN Flow ################################ 
model_list$knn <-knn <- KNNflow(data_processed, para_feaSele=1)
####################### 2013.6.25 updated and tested KNN Flow ####################### 

####################### 2013.6.24 RF Flow ################################ 
model_list$rf <-rf <- RFflow(data_processed,para_mis=1, para_discret=0, para_feaSele=0)
####################### 2013.6.24 updated and tested RF Flow ################################

## applying the models that we want

#model_list$dt<-dt_Prediction(missings_imputed);
#model_list$CHAID<-CHAID_Prediction(missings_imputed);
#model_list$KNN<-KNN_Prediction(missings_imputed);


#Comparing the models
modelComparison(model_list,1);  #the number gives which KPI is to be optimized ; 1= accuracy, etc.


#dt_prediction<-dt_Prediction(missings_imputed);
#CHAID_prediction<-CHAID_prediction(missings_imputed);
#KNN_prediction<-KNN_Prediction(missings_imputed);

print(model_list$dt$result);
print(model_list$svm$result);
print(model_list$ada$result);
print(model_list$chi$result);
print(model_list$knn$result);
print(model_list$rf$result);

############## load the best model to new dataset===================

load(file=paste(dir_name, "bestmodel.RData", sep=""))
print(best_model)
real_label <- data_processed$label
pred_label <- predict(best_model, cbind(data_processed$newdata,real_label))

model <- NULL
re_level <- levels(pred_label)
for(i in 1:nlevels(pred_label)) 
{
  model$tp[i] <- sum(as.data.frame(real_label)==re_level[i] & as.data.frame(pred_label)==re_level[i])
  model$fp[i] <- sum(as.data.frame(real_label)==re_level[i] & as.data.frame(pred_label)!=re_level[i])
  model$tn[i] <- sum(as.data.frame(real_label)!=re_level[i] & as.data.frame(pred_label)!=re_level[i])
  model$fn[i] <- sum(as.data.frame(real_label)!=re_level[i] & as.data.frame(pred_label)==re_level[i])
  model$precision[i] <- model$tp[i]/(model$tp[i]+model$fp[i])
  model$recall[i] <- model$tp[i]/(model$tp[i]+model$fn[i])
  model$true_neg[i] <- model$tn[i]/(model$tn[i]+model$fp[i])
  model$accuracy[i] <- (model$tn[i]+model$tp[i])/(model$tn[i]+model$tp[i]+model$fn[i]+model$fp[i])
}
model$precision.mean <- mean(model$precision)
model$recall.mean <- mean(model$recall)
model$accuracy.mean <- mean(model$accuracy)
model$result <- cbind(model$precision, model$recall, model$accuracy)
colnames(model$result)<-c("precision","recall","accuracy")
rownames(model$result)<-re_level