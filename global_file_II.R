#######################2013.6.21 updated ##################################  
loadFullData<-function(filenumber)
{
  dir_name<-"C:/Users/I069311/Documents/R/MASS/"
  #dir_name<-"C:/Users/i074677/Documents/R/MASS_PAL/"
  setwd(dir_name)
  dat <- NULL
  if(filenumber==1)
  {
    dat$dataset<-read.csv("house-votes-84.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
  }
  else if(filenumber==2)
  {
    dat$dataset<-read.csv("Iris-complete.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
  }
    else
  {
    dat$dataset<-read.csv("spambase-train.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
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
  return(dat);
}

missingValuesImputation<-function(dat)
{
  if(length(which(is.na(dat)))!=0)
  {
    library(mice)
    impdata <- mice(dat,seed=1)
    dat <- complete(impdata)
  } 
  return(dat);
}
  
splitDataTrainingTestValidation<-function(dat,perc_train,perc_test)
  {
    dat$seed <- 42 
    set.seed(dat$seed) 
    dat$nobs <- nrow(dat$dataset) 
    dat$train <- sample(nrow(dat$dataset), perc_train*dat$nobs) 
    dat$validate <- sample(setdiff(seq_len(nrow(dat$dataset)), dat$train), perc_test*dat$nobs) 
    dat$test <- setdiff(setdiff(seq_len(nrow(dat$dataset)), dat$train), dat$validate) 
    dat$train <- dat$dataset[dat$train,]
    dat$validate <- dat$dataset[dat$validate,]
    dat$test <- dat$dataset[dat$test,]
    return(dat)
  }
#####################################2013.6.21 updated and tested ################################## 
replaceNumericalValues <- function(dat)  
{
  dis_data <- dat
  library(discretization)
  dis_data <- chiM(dat)
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
  
  subset <- cfs(classlabel~., rbind(dat$train,dat$validate))
  return(subset)
}

#######################2013.6.21 updated ################################## 
prediction_KPIs<-function(dat,model)
{
  for(i in 1:dat$nlevel) {
    dat$label <- dat$test[,1]
    model$tp[i] <- sum(dat$label==dat$level[i] & t(model$pr)==dat$level[i])
    model$fp[i] <- sum(dat$label==dat$level[i] & t(model$pr)!=dat$level[i])
    model$tn[i] <- sum(dat$label!=dat$level[i] & t(model$pr)!=dat$level[i])
    model$fn[i] <- sum(dat$label!=dat$level[i] & t(model$pr)==dat$level[i])
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
  # Build the Decision Tree model.
  dt$c45 <- J48(classlabel ~ ., data=dat$train[,c(dat$categoric,dat$target)])
  dt$pr <- predict(dt$c45, dat$test[,c(dat$categoric, dat$target)], type="class")
  dt<-prediction_KPIs(dat,dt);
  return(dt);
}

DTflow <- function(dat, para_mis=0, para_feaSele=0) 
{
  if(para_mis!= 0)
  {
    dat$dataset <- missingValuesImputation(dat$dataset)
  } 
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6, perc_test=0.2)
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
  chi$rchaid <- chaid(classlabel ~ ., data=dat$train[,c(dat$categoric,dat$target)])
  chi$pr <- predict(chi$rchaid, dat$test[,c(dat$categoric, dat$target)])
  chi<-prediction_KPIs(dat,chi);
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
      dat$dataset <- replaceNumericalValues(dat$dataset[,c(dat$categoric,dat$target)])$Disc.data
      dat$dataset <- dat$dataset[,c(dat$target,dat$categoric)]
    }
  }
  for(i in 2:ncol(dat$dataset)){
    if(!is.factor(dat$dataset[,i]))
    {
      dat$dataset[,i] <- as.factor(dat$dataset[,i])  # guarantee the label column is a factor
    }
  }
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6, perc_test=0.2)  
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
  ADA$rada <- ada(classlabel ~ .,
                  data=dat$train[,c(dat$categoric,dat$target)]
  )
  ADA$pr <- predict(ADA$rada, dat$test[,c(dat$categoric, dat$target)])
  ADA<-prediction_KPIs(dat,ADA);
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
      dat$dataset <- replaceNumericalValues(dat$dataset[,c(dat$categoric,dat$target)])$Disc.data
      dat$dataset <- dat$dataset[,c(dat$target,dat$categoric)]
    }
  }
#   for(i in 2:ncol(dat$dataset)){
#     if(!is.factor(dat$dataset[,i]))
#     {
#       dat$dataset[,i] <- as.factor(dat$dataset[,i])  # guarantee the label column is a factor
#     }
#   }
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6, perc_test=0.2)  
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
  SVM$rsvm<-ksvm(classlabel ~ .,
                 data=dat$train[,c(dat$categoric,dat$target)],kernel="rbfdot",prob.model=TRUE)
  SVM$pr <- predict(SVM$rsvm, dat$test[,c(dat$categoric, dat$target)])
  SVM<-prediction_KPIs(dat,SVM);
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
      dat$dataset <- replaceNumericalValues(dat$dataset[,c(dat$categoric,dat$target)])$Disc.data
      dat$dataset <- dat$dataset[,c(dat$target,dat$categoric)]
    }
  }
 
  for(i in 2:ncol(dat$dataset)){
    if(!is.factor(dat$dataset[,i]))
    {
      dat$dataset[,i] <- as.factor(dat$dataset[,i])  # guarantee the label column is a factor
    }
  }
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6, perc_test=0.2)  
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
  knnclass$pr <- class::knn(dat$train[c(dat$categoric)],dat$test[c(dat$categoric)],dat$train[,1], k=5)
  knnclass<-prediction_KPIs(dat,knnclass);
  return(knnclass)
}

KNNflow <- function(dat, para_feaSele=1) 
{
  dat$dataset <- missingValuesImputation(dat$dataset)
  dat$dataset <- replaceCategoricalValues(dat$dataset)
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6, perc_test=0.2)  
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
  RF$rf <- randomForest(classlabel ~ .,
                        data=dat$train[,c(dat$categoric,dat$target)],mtry=3,
                        importance=TRUE,
                        proximity=TRUE)
  
  RF$pr <- predict(RF$rf, dat$test[,c(dat$categoric, dat$target)])
  RF<-prediction_KPIs(dat,RF);
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
      dat$dataset <- replaceNumericalValues(dat$dataset[,c(dat$categoric,dat$target)])$Disc.data
      dat$dataset <- dat$dataset[,c(dat$target,dat$categoric)]
    }
  }
#   for(i in 2:ncol(dat$dataset)){
#     if(!is.factor(dat$dataset[,i]))
#     {
#       dat$dataset[,i] <- as.factor(dat$dataset[,i])  # guarantee the label column is a factor
#     }
#   }
  dat <- splitDataTrainingTestValidation(dat, perc_train=0.6, perc_test=0.2)  
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
        best_model<-1
        best_accuracy_mean<-models[[1]]$accuracy.mean
        ##finding the best model who gives the highest accuracy
        for(i in 2:nb_models)
        {
          if(models[[i]]$accuracy.mean>best_accuracy_mean)
          {
            best_accuracy_mean<-models[[i]]$accuracy.mean
            best_model<-i
          }
        }
        print("The best model from the accuracy point of view is model")
        print(best_model)
        print("The accuracy of the model is:")
        print(best_accuracy_mean)
      }else if(priority==2){      ## 2 : precision
        best_model<-1
        best_precision_mean<-models[[1]]$precision.mean     
        for(i in 2:nb_models)
        {
          if(models[[i]]$precision.mean>best_precision_mean)
          {
            best_precision_mean<-models[[i]]$precision.mean
            best_model<-i
          }
        }
        print("The best model from the precision point of view is model")
        print(best_model)
        print("The precision of the model is:")
        print(best_precision_mean)
      }
      else{ ## 3 : recall
        best_model<-1
        best_recall_mean<-models[[1]]$recall.mean     
        for(i in 2:nb_models)
        {
          if(models[[i]]$recall.mean>best_recall_mean)
          {
            best_recall_mean<-models[[i]]$recall.mean
            best_model<-i
          }
        }
        print("The best model from the recall point of view is model")
        print(best_model)
        print("The recall of the model is:")
        print(best_recall_mean)
      }
  }
  else{
    print("The best model from the accuracy point of view is model")
    print(0)
    print("The accuracy of the model is:")
    print(models[[1]]$accuracy.mean)
  }
}
#######################2013.6.24 updated and tested ################################## 







######################## 2013.6.21 Data Preparation ################################
model_list<-NULL

######################## 
data_loaded <- loadFullData(2); #loading file number ...
data_processed <- prepareLabelsNamesData(data_loaded)

####################### 2013.6.21 updated and tested ############################### 
####################### 2013.6.25 DT Flow ################################ 
model_list$dt <- DTflow(data_processed, para_mis=1, para_feaSele=1)

####################### 2013.6.25 updated and tested DT Flow ####################### 

####################### 2013.6.22 CHAID Flow ################################ 
#print("Warning: Long time will be consumed for big numerical-type data")
model_list$chi <- CHAIDflow(data_processed,para_mis=1, para_discret=1, para_feaSele=1) 
####################### 2013.6.22 updated and tested CHAID Flow ####################### 

####################### 2013.6.25 SVM Flow ################################ 
model_list$svm <-svm <- SVMflow(data_processed,para_mis=1, para_discret=0, para_feaSele=1)
####################### 2013.6.25 updated and tested SVM Flow ####################### 

####################### 2013.6.23 ADA Flow ################################ 
#=================== with a bug of para_feaSele=1 on dataset 1 and 2
model_list$ada <-ada <- ADAflow(data_processed,para_mis=1, para_discret=0, para_feaSele=0)
####################### 2013.6.22 updated and tested ADA Flow ####################### 

####################### 2013.6.25 KNN Flow ################################ 
model_list$knn <-knn <- KNNflow(data_processed, para_feaSele=1)
####################### 2013.6.25 updated and tested KNN Flow ####################### 

####################### 2013.6.24 RF Flow ################################ 
model_list$rf <-rf <- RFflow(data_processed,para_mis=1, para_discret=0, para_feaSele=1)
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
