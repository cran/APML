\name{outCI}
\alias{outCI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
output CI in specific format.
}
\description{
Output CI in specific format.
}
\usage{
outCI(x,l,h,n=2,type='OR')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{OR or RR.}
  \item{l}{lower or left side of confidence interval.}
  \item{h}{higher or right side of confidence interval.}
  \item{n}{number of digits.}
  \item{type}{output OR/RR or ER(excess risk).}
}

\value{
A character
}

\examples{
outCI(8.601581,4.678212,12.524951)

##"8.60(4.68,12.52)"
}

