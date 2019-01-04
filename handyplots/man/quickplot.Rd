\name{quickplot}
\alias{quickplot}
\title{
Quick Plot
}
\description{
If you have two numeric vectors of equal length you can use quickplot to quickly look at the potential relationship between them in four graphs at once.

Quickplot will show you a scatter plot with a regression line, a qq-plot to check the normality of the residuals, a residual plot to check the constancy and correlation of the residuals, and a boxplot for a quick overview of the spread of the two vectors, and two historgrams to see the distributions of the two vectors.
}
\usage{
quickplot(x, y)
}
\arguments{
  \item{x}{
A numeric vector of length > 3
}
  \item{y}{
A numeric vector of length > 3 (equal in length to \code{x})
}
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
\code{\link{qqnorm}},
\code{\link{qqline}},
\code{\link{resplot}},
\code{\link{boxplot}}
}
\examples{
##quickly looking at the relationship between iris petal length and iris petal width
quickplot(iris$Petal.Length,iris$Petal.Width)
}