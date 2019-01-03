\name{ciplot}
\alias{ciplot}
\title{
Confidence Interval Plot
}
\description{
given two numeric vectors of equal length, plot a scatter plot of the data, the regression line, and a confidence interval for the mean of a new observation or the prediction interval for a single new observation.
}
\usage{
ciplot(x, y, x0 = NULL, int = c("p","c"), level = 0.95, 
relationship = c("linear","quadratic","cubic","sqrt","exponential","reciprocal","log"), 
show.range = TRUE, user.xlim = NULL, user.ylim = NULL)
}
\arguments{
  \item{x}{
a numeric vector of length > 3
}
  \item{y}{
a numeric vector of length > 3 (equal in length to \code{x})
}
  \item{x0}{
the x value at which you wish to make a prediction (\code{NULL} by default. If left blank, the plot will not show a prediction at a particular x value.)
}
  \item{int}{
interval type. \code{"prediction"} by default (can be abbreviated), which will plot the prediction interval for a single new observation. If specified with \code{"confidence"} (can be abbreviated), it will show the confidence interval for the mean value of a new observation.
}
  \item{level}{
the confidence level at which you wish to predict. \code{0.95} by default. If you wish to specify a confidence level, it must be a numerical value greater than 0 and less than 1.
}
  \item{relationship}{
the type of relationship that the two vectors share. \code{"linear"} by default. May specify a different type of relationship with \code{"quadratic"} (may be abbreviated to \code{"quad"}), \code{"cubic"}, \code{"sqrt"}, \code{"exponential"} (may be abbreviated to \code{"exp"}), \code{"reciprocal"} (may be abbreviated to \code{"recip"}), or \code{"logarithmic"} (may be abbreviated to \code{"log"}). Specifying a different type of relationship will change the shape of the regression line.
}
  \item{show.range}{
logical. If \code{TRUE} (the default), dotted red lines will show the confidence or prediction interval along the entire plot. If \code{FALSE}, it will only show the confidence/prediction interval at a specified x value (if \code{x0} is set).
}
  \item{user.xlim}{
the interval of x values the user wishes to display in the plot. If left unspecified, it will be \code{NULL} and the default x limits will be plotted (which will be the entire range of \code{x}, including \code{x0}).
}
  \item{user.ylim}{
the interval of y values the user wishes to display in the plot. If left unspecified, it will be \code{NULL} and the default y limits will be plotted (which will be the entire range of \code{y}, including the predicted y value at \code{x0}).
}
}
\references{
Montgomery, D. C., Peck, E. A., Vining, G. G. (2013), Introduction to Linear Regression Analysis, Hoboken, NJ: John Wiley & Sons, Inc.
}
\author{
Jonathan Schwartz, Mengqi Yin
}
\section{Warning}{
If \code{x0} is outside the domain of \code{x}, ciplot will extrapolate the data and predict a value of \code{yhat} for the given \code{x0}. This may be dangerous, depending on how your data behaves outside the existing domain.
}
\seealso{
\code{\link{plot}},
\code{\link{lm}},
\code{\link{predict}}
}
\examples{
##predicting the mean of the width of an iris's petal whose petal length is 2.5
ciplot(iris$Petal.Length,iris$Petal.Width,x0=2.5,int="conf")

##predicting the value of a single observation of the width of an iris's petal whose petal length is 2.5
ciplot(iris$Petal.Length,iris$Petal.Width,x0=2.5,int="pred")

##extrapolating the data to predict the mean of the width of an iris's petal whose petal length is 8
ciplot(iris$Petal.Length,iris$Petal.Width,x0=8,int="conf")

##zooming in to the previous graph and removing the dotted red lines
ciplot(iris$Petal.Length,iris$Petal.Width,x0=8,int="conf",show.range=FALSE,
user.xlim=c(7.5,8.5),user.ylim=c(2.6,3.2))
}