\name{splits_selection}
\alias{splits_selection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split dataset and select variables
}
\description{
Split dataset into training data and testing data and select variables based on relative importance.
}
\usage{
splits_selection(data,split_ratio,split_seed,
feature_model,imbalance,nfolds,
RAN_type,RAN.seed,smote.seed,
xcol_enter,distribution)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data.frame used to build models}
  \item{split_ratio}{A numeric value indicating the ratio of total rows contained in each split. Must less than 1}
  \item{split_seed}{Random seed for splitting}
  \item{feature_model}{Name of model for feature selection. Currently, only allow "gbm" for gradient boosted tree, and "rf" for random forest}
  \item{imbalance}{Logical or "SMOTE"(for categorical response). True for balancing training data class counts via over/under-sampling when building the model. "SMOTE" for applying SMOTE and returning SMOTE training data.}
  \item{nfolds}{Number of folds for K-fold cross-validation. Default:5.}
  \item{RAN_type}{"both", "binominal" or "normal". "both" for generating both binominal and normal random terms for feature selection. "binominal" or "normal" only generate one specific type of random term. Categorical or continuous variables with relative importance greater than corresponding random term(s) will be selected.}
  \item{RAN.seed}{Random seed for random term(s)}
  \item{smote.seed}{Random seed for SMOTE. Only used if argument "imbalance"="SMOTE"}
  \item{xcol_enter}{A character vector of variables are required to enter the model, also called "forced entry". If xcol_enter contains all independent variables' names, it will not use random terms to select variables.}
  \item{distribution}{Distribution type. Must be one of: "AUTO", "bernoulli", "quasibinomial", "multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace", "quantile", "huber", "custom". Defaults to AUTO.}
}
\details{
This function applys a technique to use random term to select variables. We consider variables with relative importance greater than random term as truly important variables.
}
\value{
  \item{importance}{A data.frame containing the relative importance scores of selected variables.}
  \item{train_data}{Training dataset. If "imbalance"="SMOTE", it returns the SMOTE training set.}
  \item{test_data}{Testing dataset.}
  \item{raw_traindata}{Same training dataset. If "imbalance"="SMOTE", it returns the original training set before SMOTE.}

}

\note{
This function is based on h2o package. In order to run this function, we need to run h2o.init() before using this function.
The response variable should be the first column.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\donttest{
library(survival)
library(h2o)
library(performanceEstimation)
data("lung")
attach(lung)
data <- datatrans(lung,factor_dummy = 'dummy',rescale = TRUE)
data <- data[,c(3,1,2,4:14)]
h2o.init()
selection <- splits_selection(data,imbalance = 'SMOTE')
h2o.shutdown(prompt=FALSE)
Sys.sleep(2)}
}
