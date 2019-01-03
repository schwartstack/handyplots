\name{fakedata}
\alias{fakedata}
\title{
Fake Data
}
\description{
A quick way to cook up some fake data.
}
\usage{
fakedata(formula, s = 0.25)
}
\arguments{
  \item{formula}{
A formula which describes the relationship you wish your fake data to have to an existing numeric vector. For example, if you have a numeric vector \code{x}, if you want your fake data to have a perfect 1-to-1 linear relationship with \code{x}, the formula would simply be \code{x}.
}
  \item{s}{
A numeric value which describes the amount of variablity you want your fake data to have. If \code{s} = 0, then the data will have no variablity at all. (i.e. the residuals will have mean 0 and variance 0). If \code{s} > 1, the data will look very scattered and random, the correlation between your existing vector and your fake data will be low.
}
}
\details{
Quickly cooking up fake data may be useful for experimenting with differnt plotting functions in R with data that you can control. You can control the relationship between your data and an existing vector, and you can control the variablity of the data, i.e. how closely correlated the fake data is to the existing vector. You also know that the residuals are normally distributed with mean 0, which satisfies a major assumption of linear regression.
}
\value{
The function returns a numeric vector.
}
\author{
Jonathan Schwartz
}
\seealso{
\code{\link{rnorm}},
\code{\link{plot}},
\code{\link{lm}}
}
\examples{
x=sample(0:1000,100)
y=fakedata(3*x+10) #y is a vector of fake data which will have a linear relationship with x
plot(x,y)
cor(x,y) #x and y are very highly correlated
y2=fakedata(3*x+10,1) #increasing the value of s decreases the correlation
plot(x,y2)
cor(x,y2) #x and y2 are not as highly correlated

##you can also, of course do non-linear relationships
y3=fakedata(sqrt(1/x))
plot(x,y3)
}