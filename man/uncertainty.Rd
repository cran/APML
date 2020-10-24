\name{uncertainty}
\alias{uncertainty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the Uncertainty (95 percent confidence interval) of Risk Ratio Based on Threshold.
}
\description{
Calculate the uncertainty (95 percent confidence interval) of risk ratio with prediction from general models based on threshold.
}
\usage{
uncertainty(x,y,th,ref=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A vector representing the risk factor.}
  \item{y}{A vector representing the outcome prediction probability.}
  \item{th}{Threshold for dividing the risk facot into two groups.}
  \item{ref}{Reference indicator. If 0, set the group lower than threshold as reference. Default:0}
}
\details{
This is used to calculate the risk ratio, but not for odd ratio.
}
\value{
Uncertainty (95 percent confidence interval) of risk ratio.
}
\references{
Díaz-Francés, E., Rubio, F.J. On the existence of a normal approximation to the distribution of the ratio of two independent normal random variables. Stat Papers 54, 309-C323 (2013).
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
uncertainty(data$Sepal.Length,data$setosa,plots$threshold)
h2o.shutdown(prompt=FALSE)
Sys.sleep(2)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
