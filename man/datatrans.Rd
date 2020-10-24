\name{datatrans}
\alias{datatrans}
\title{
Data Cleaning
}
\description{
This function can help transform a bunch of numeric variables into factors or dummy variables, and change the reference for dichotomous variables. It can also drop constant colunms, variables with a great number of missing values and categorical variables with number of minority less than this ratio of number of target minority. For numeric variables, it can rescale the values. 
}
\usage{
datatrans(data,class_number,rescale,factor_dummy,ref, target,drop_ratio,missing_rate)
}
\arguments{
  \item{data}{A data.frame representing raw data needed for cleaning.}
  
  \item{class_number}{A integer representing numbers of unique categories for distinguishing categorical variables and continuous variables. Every variable with unique value greater than the number will be treated as continuous variable. Default:5}
  
  \item{rescale}{Logical. Whether or not to rescale continuous variables with Z-score scaling method. Default:False}
  
  \item{factor_dummy}{A character which could be "factor", "NULL" or "dummy". If "factor", categorical varaibles will be transformed into factors. If "dummy", it will create dummy variables for categorical variables. If "NULL", do nothing. Default: NULL}
  
  \item{ref}{Could be a number, "s" or "b". For dichotomous variables, this specifys the reference category. If a number, it will set the number as 0, the other as 1. If "s", it sets the smaller value as 0. If "b", it sets the bigger one as 0. Default:NULL, no changes.}
  
  \item{target}{A character representing the target variable. If give the target name, it will drop categorical variables with number of minority less than certian ratio of number of target minority.}
  
  \item{drop_ratio}{A number specifying the ratio for dropping categorical variables with number of minority less than this ratio of number of target minority. Only used if argument target is given. Default:0, not dropping.}
  
  \item{missing_rate}{A number specifying what ratio of missings in a variable, which should be dropped. Default:0.5.}

}
\details{
datatrans is only used for cleaning raw data. Raw data shouldn't contain any characters. Only numbers are permitted. Character information should be converted into numbers before use this function.
}
\value{
A cleaned data.frame.
}

\note{
After the data is cleaned, it is ready for modelling.
}

\seealso{
splits_selection, APML
}
\examples{
library(survival)
data(lung)
attach(lung)
data = datatrans(lung,rescale=TRUE,factor_dummy = 'factor')
head(data)
str(data)

}

