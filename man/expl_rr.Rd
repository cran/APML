\name{expl_rr}
\alias{expl_rr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Explore the Risk Ratio with Cubic Spline
}
\description{
plot the changes of risk ratio of a risk factor in relation to the outcome using predictions from a general model, and identify the threshold.
}
\usage{
expl_rr(data,formula,low=0.01,high=0.99,ref = 'min')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data.frame containing the risk factor and the outcome prediction probability from a model.}
  \item{formula}{Formula. Specify the outcome prediction and risk factor like pred~x}
  \item{low}{Set x scale limits}
  \item{high}{Set x scale limits}
  \item{ref}{Set reference. Using the smallest("min"), mean("mean"), median("median") or customized value of loess prediction}
}
\details{
For health data, if it is a cohor study, it will calculate the Risk Ratio. The risk ratio is calculated through following approaches. First, fit the risk factor and outcome prediction with loess regression, and get the smallest(mean, median, customized) value of loess prediction. Using the smallest(mean, median, customized) value from loess prediction as reference, the risk ratio = outcome prediction/the smallest value. Plot the risk factor and risk ratio with cubic spline.
}
\value{
\item{p}{Cubic Spline plot.}
\item{pred}{Loess model prediction.}
\item{min_pred}{Reference value. The smallest value from loess prediction.}
\item{threshold}{Threshold value for the plot. Identified by youden index}

}

\note{
For health data, if it is a cohor study, it will calculate the Risk Ratio.
}


\examples{
\donttest{
library(h2o)
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
data <- as.h2o(iris)
pred<- h2o.predict(results$bestmodel,newdata=data)
data <- h2o.cbind(data,pred)
data <- as.data.frame(data)
plots <- expl_rr(data,setosa~Sepal.Length,ref = 'mean')
plots$p
h2o.shutdown(prompt=FALSE)
Sys.sleep(2)}
}

