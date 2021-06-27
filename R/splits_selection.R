splits_selection <-
  function(data,split_ratio = 0.7,split_seed = 1,feature_model='gbm',imbalance = FALSE,nfolds = 5,RAN_type='both',
           RAN.seed=1,smote.seed=1,xcol_enter = NULL,distribution ='AUTO' ) {
    message('\nthe first column is used as response variable')
    message('\ninitialing...')
    data.hex <- as.h2o(data)
    if(distribution=='bernoulli') data.hex[,1] <- as.factor(data.hex[,1])
    
    splits <- h2o.splitFrame(data.hex,ratios =split_ratio,seed =split_seed )
    traindata <- h2o.assign(splits[[1]],'train.hex')
    traindata0 <- as.data.frame(traindata)
    testdata <- h2o.assign(splits[[2]],'test.hex')
    newData=as.data.frame(traindata)
    if(imbalance=="SMOTE"){
      f <- stats::formula(paste0(names(newData)[1],'~.'))
      categories <- NULL
      for (i in 1:ncol(newData)) {
        if(length(unique(newData[,i]))==2){
          categories=c(categories,i)}
      }
      newData[,categories] <- lapply(newData[,categories],as.factor)
      set.seed(smote.seed)
      newData <- smote(f,data=newData, perc.over=table(newData[,1])[1]/table(newData[,1])[2]*100,perc.under=100)
    }
    newData1 <- newData
    if(RAN_type=='binominal'){
      set.seed(RAN.seed)
      newData$RAN <- stats::rbinom(nrow(newData),1,prob = 0.5)
      newData$RAN <- as.factor(newData$RAN)
    }else if(RAN_type=='normal'){
      set.seed(RAN.seed)
      newData$RAN <- stats::rnorm(nrow(newData))
    }else if(RAN_type=='both'){
      set.seed(RAN.seed)
      newData$RAN1 <- stats::rbinom(nrow(newData),1,prob = 0.5)
      newData$RAN1 <- as.factor(newData$RAN1)
      set.seed(RAN.seed)
      newData$RAN2 <- stats::rnorm(nrow(newData))
    }
    traindata <- as.h2o(newData)
    if(feature_model=='gbm'){
      fmodel <-h2o.gbm(x=2:ncol(traindata), y=1, distribution=distribution,balance_classes=isTRUE(imbalance),
                       training_frame=traindata,ntrees=200,nfolds=3,learn_rate=0.01,
                       sample_rate=0.7,col_sample_rate=0.7, max_depth=5,seed=666)
    }
    if(feature_model=='rf'){
      fmodel <-h2o.randomForest(
        x=2:ncol(traindata), y=1, training_frame=traindata,ntrees=200,nfolds=3,
        sample_rate=0.7,col_sample_rate_per_tree=0.7, max_depth=5,seed=666)
    }
    fimp=NULL
    fimp=h2o.varimp(fmodel)
    if(RAN_type=='both'){
      num <- NULL
      for (i in 1:ncol(newData)) {
        if(length(unique(newData[,i]))>2){
          num=c(num,i)}
      }
      categories <- NULL
      for (i in 1:ncol(newData)) {
        if(length(unique(newData[,i]))<=2){
          categories=c(categories,i)}
      }
      med=data.frame(factor=fimp$variable,ctr=fimp$relative_importance)
      
      xcol1 <- as.character(med$factor[med$ctr>med[which(med$factor=="RAN1"),2]])
      
      xcol1 <- xcol1[xcol1%in%names(newData)[categories]]
      xcol2 <- as.character(med$factor[med$ctr>med[which(med$factor=="RAN2"),2]])
      xcol2 <- xcol2[xcol2%in%names(newData)[num]]
      
      xcol=c(xcol_enter,xcol1,xcol2)
    }else{
      med=data.frame(factor=fimp$variable,ctr=fimp$relative_importance)
      xcol=NULL
      xcol=as.character(med$factor[med$ctr>med[which(med$factor=="RAN"),2]])
      xcol=c(xcol_enter,xcol)
    }
    xcol=unique(xcol)
    if(feature_model=='gbm'){
      fmodel <-h2o.gbm(x=xcol, y=1, distribution=distribution,balance_classes=isTRUE(imbalance),
                       training_frame=traindata,ntrees=200,nfolds=3,learn_rate=0.01,
                       sample_rate=0.7,col_sample_rate=0.7, max_depth=5,seed=666)
    }
    if(feature_model=='rf'){
      fmodel <-h2o.randomForest(
        x=xcol, y=1, training_frame=traindata,ntrees=200,nfolds=3,distribution=distribution,balance_classes=isTRUE(imbalance),
        sample_rate=0.7,col_sample_rate_per_tree=0.7, max_depth=5,seed=666)
    }
    fimp=NULL
    fimp=h2o.varimp(fmodel)
    fimp <- fimp[fimp$relative_importance>0,]
    traindata=as.data.frame(traindata)
    traindata=
      results <- list(importance=fimp,train_data=newData1,test_data=as.data.frame(testdata),raw_traindata=traindata0)
    return(results)
  }
