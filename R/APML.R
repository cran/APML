APML <- function(model='gbm',AUC_stopping=F,xcol,traindata,testdata,hyper,distribution='bernoulli',imbalance=F,sort_by='auc',
                 extra_data=NULL,stopping_metric='AUTO'
){
  search_criteria=NULL
  if(AUC_stopping==T) search_criteria = list(strategy = 'RandomDiscrete',stopping_metric = "AUC",stopping_rounds = 3)
  traindata=as.h2o(traindata)
  testdata=as.h2o(testdata)
  if(!is.null(extra_data))extra_data=as.h2o(extra_data)
  if(model=="gbm"){
    grid1=NULL
    grid1=h2o.grid("gbm", distribution=distribution,x=xcol, y=1, training_frame=traindata,nfolds=5,seed=666,
                   sample_rate=0.7,col_sample_rate=0.7,fold_assignment = "AUTO",balance_classes=isTRUE(imbalance),
                   hyper_params=hyper,search_criteria = search_criteria,stopping_metric=stopping_metric
    )
  }
  if(model=="rf"){
    grid1=NULL
    grid1=h2o.grid("randomForest",x=xcol, y=1, training_frame=traindata,nfolds=5,seed=666,balance_classes=isTRUE(imbalance),
                   sample_rate=0.7,col_sample_rate_per_tree=0.7,fold_assignment = "AUTO",distribution=distribution,
                   hyper_params=hyper,search_criteria = search_criteria,stopping_metric=stopping_metric)
  }
  
  if(sort_by=='auc'){
    gridperf1=NULL
    gridperf1 <- h2o.getGrid(grid_id = grid1@grid_id,
                             sort_by = sort_by,
                             decreasing = TRUE)
    best_gd <- h2o.getModel(gridperf1@model_ids[[1]])
    best_perf1=h2o.performance(model = best_gd,newdata = testdata)
    out=data.frame(
      train_AUC=best_gd@model$training_metrics@metrics$AUC,
      train_prAUC=best_gd@model$training_metrics@metrics$pr_auc,
      cv_AUC=best_gd@model$cross_validation_metrics@metrics$AUC,
      cv_prAUC=best_gd@model$cross_validation_metrics@metrics$pr_auc,
      test_AUC=best_perf1@metrics$AUC,
      test_prAUC=best_perf1@metrics$pr_auc
    )
    OUT=data.frame(gridperf1@summary_table)

    para=list(metrics=out,grid_summary=OUT)
    train_metrics <- h2o.metric(h2o.performance(model = best_gd,newdata = traindata))
    test_metrics <- h2o.metric(h2o.performance(model = best_gd,newdata = testdata))
    train0_metrics=NULL
    if(!is.null(extra_data)) train0_metrics <- h2o.metric(h2o.performance(model = best_gd,newdata = extra_data))
    result <- list(bestmodel=best_gd,train_metrics=train_metrics,test_metrics=test_metrics,summary=para,extra_metrics=train0_metrics)
    
  }else{
    gridperf1=NULL
    if(sort_by=='r2'){
      gridperf1 <- h2o.getGrid(grid_id = grid1@grid_id,
                               sort_by = sort_by,
                               decreasing = TRUE)
    }else{
      gridperf1 <- h2o.getGrid(grid_id = grid1@grid_id,
                               sort_by = sort_by,
                               decreasing = FALSE)
    }
    
    best_gd <- h2o.getModel(gridperf1@model_ids[[1]])
    best_perf1=h2o.performance(model = best_gd,newdata = testdata)
    out=data.frame(
      train_MSE=best_gd@model$training_metrics@metrics$MSE,
      train_RMSE=best_gd@model$training_metrics@metrics$RMSE,
      train_R2=best_gd@model$training_metrics@metrics$r2,
      cv_MSE=best_gd@model$cross_validation_metrics@metrics$MSE,
      cv_RMSE=best_gd@model$cross_validation_metrics@metrics$RMSE,
      cv_R2=best_gd@model$cross_validation_metrics@metrics$r2,
      test_MSE=best_perf1@metrics$MSE,
      test_RMSE=best_perf1@metrics$RMSE,
      test_R2=best_perf1@metrics$r2
    )
    OUT=data.frame(gridperf1@summary_table)
    para=list(metrics=out,grid_summary=OUT)
    train_metrics <- h2o.performance(model = best_gd,newdata = traindata)
    test_metrics <- h2o.performance(model = best_gd,newdata = testdata)
    train0_metrics=NULL
    if(!is.null(extra_data)) train0_metrics <- h2o.metric(h2o.performance(model = best_gd,newdata = extra_data))
    result <- list(bestmodel=best_gd,train_metrics=train_metrics,test_metrics=test_metrics,summary=para,extra_metrics=train0_metrics)
  }
  return(result)
}



