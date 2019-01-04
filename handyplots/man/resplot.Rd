\name{resplot}
\alias{resplot}
\title{
Residual Plot
}
\description{
Plot the fitted values vs the studentized or standardized residuals for a \code{glm} or \code{lm} object.
}
\usage{
resplot(model, zoom = NULL, highlight.outliers = FALSE, 
  residuals = c("student","standard"))
}
\arguments{
  \item{model}{
a regression model with any number of predictors. Must be a \code{glm} or \code{lm} object.
}
  \item{zoom}{
what range of residuals you wish to show in your plot. By default, zoom is \code{NULL}, and the residual plot will show all residuals. If you set zoom to a numeric value > 0, resplot will only show residuals which are at most that many standard deviations away from 0.
}
  \item{highlight.outliers}{
logical. If \code{FALSE} (the default), outliers will not be highlighted. If \code{TRUE}, every residual which is more than 3 standard deviations from 0 will be circled in red.
}
  \item{residuals}{
which type of residuals to use. Studentized residuals are used by default, but can be specified with \code{"student"}, \code{"rstudent"}, or \code{"studentized"}. Standardized residuals can be specified with \code{"standard"}, \code{"rstandard"}, or \code{"standardized"} 
}
}
\details{
A residual plot shows the fitted values of the response variable on the x-axis and the studentized or standardized residuals on the y-axis. It can be used to check for correlated residuals or non-constant variance of the residuals, both of which would violate the residual assumptions of a linear model. It can also be used to check for outliers, as a value below -3 or above 3 would indicate a residual which is more than 3 standard deviations from the mean of 0.
}
\references{
Montgomery, D. C., Peck, E. A., Vining, G. G. (2013), Introduction to Linear Regression Analysis, Hoboken, NJ: John Wiley & Sons, Inc.
}
\author{
Jonathan Schwartz
}
\seealso{
\code{\link{plot}},
\code{\link{abline}},
\code{\link{lm}},
\code{\link{glm}},
\code{\link{predict}},
\code{\link{rstudent}},
\code{\link{rstandard}}
}
\examples{
##plot a residual plot to check the model assumptions for a linear
##model of iris petal length as a predicted by iris petal width
model<-lm(iris$Petal.Length~iris$Petal.Width)
resplot(model)

##highlight the one outlier
resplot(model,highlight.outliers=TRUE)

##zoom in to only show the residuals between -1 and 1
resplot(model,zoom=1)
}