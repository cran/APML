\name{APML}
\alias{APML}
\title{
Develop models with grid search
}
\description{
Develop models with the optional parameters identified through the grid search and return model performance metrics. In order to run properly, the response column must be the first column and of a type of either numeric for "gaussian" or factor for "bernoulli" or "multinomial".
}
\usage{
APML(model,AUC_stopping, xcol, traindata,testdata, hyper, 
distribution,imbalance, sort_by, extra_data,stopping_metric)
}
\arguments{
  \item{model}{The model to be used. Currently, only allow "gbm" (default) for the gradient boosted tree, and "rf" for the random forest.}
  \item{AUC_stopping}{Logical. If TRUE, the combinations of the hyperparameters will be randomly searched with AUC metric-based early stopping. Default:FALSE.}
  \item{xcol}{A vector containing the names or indices of the predictors to be used.}  
  \item{traindata}{The training dataset.}
  \item{testdata}{The testing dataset.}
  \item{hyper}{List of hyper parameters (i.e., list(ntrees=c(1,2), max_depth=c(5,7)))}
  \item{distribution}{Distribution of the outcome: "bernoulli" (default), "bernoulli", "quasibinomial", "multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace", "quantile", "huber" or "custom".}
  \item{imbalance}{Logical. If true, balancing the case numbers in the training data via over/under-sampling when developing the model. Default:FALSE}
  \item{sort_by}{Select the best model in the grid space by sorting with a metric. Choices are "logloss", "residual_deviance", "mse", "auc", "accuracy", "precision", "recall", "f1", etc}
  \item{extra_data}{Extra dataset for evaluating model performance.}
  \item{stopping_metric}{Metric to use for early stopping (AUTO: logloss for classification, deviance for regression and anonomaly_score for Isolation Forest). Must be one of: "AUTO", "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE", "AUC", "AUCPR", "lift_top_group", "misclassification", "mean_per_class_error", "custom", "custom_increasing". Defaults to AUTO.}
}
\details{
This function uses the grid search technique to tune models' parameters and return the optimal model.
}
\value{
  \item{bestmodel}{Best H2o model via grid search}
  \item{train_metrics}{Model performance metrics based on the training data.}
  \item{test_metrics}{Model performance metrics based on the testing data. }
  \item{summary}{Summary of model performance.}
  \item{extra_metrics}{Model performance metrics based on extra data. Only available when "model_metric" is used.}
}

\note{
This function is based on h2o package. In order to run this function, we need to run h2o.init() before using this function.
The response variable should be the first column.
}

\references{
LeDell E, Gill N, Aiello S, Fu A, Candel A, Click C, et al. 2019. h2o: R Interface for “H2O.”
Zhang W, Du Z, Zhang D, Yu S, Hao Y. 2016a. Boosted regression tree model-based assessment of the impacts of meteorological drivers of hand, foot and mouth disease in Guangdong, China. Sci Total Environ 553; doi:10.1016/j.scitotenv.2016.02.023.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\donttest{library(h2o)
data(iris)
attach(iris)
h2o.init()
hyper <- list(ntrees=c(2,3,5))
iris <- iris[1:100,c(5,1:4)]
idx <- sample(100,50)
traindata <- iris[idx,]
testdata <- iris[-idx,]
xcol <- names(iris)[2:5]
results <- APML(xcol=xcol,hyper=hyper,
                traindata=traindata,testdata=testdata,
                sort_by ='auc',distribution = 'bernoulli')
h2o.shutdown(prompt=FALSE)
Sys.sleep(2)}

}

