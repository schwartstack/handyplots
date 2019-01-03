\name{colID}
\alias{colID}
\title{
Column ID
}
\description{
A quick way to see the name and class of every colum of a data frame
}
\usage{
colID(df)
}
\arguments{
  \item{df}{
A data frame you wish to look at
}
}
\value{
Returns a data frame where column 1 is the names of the columns of the original data frame, and column 2 is the class of the column of the original data frame. 
}
\author{
Jonathan Schwartz
}
\seealso{
\code{\link{data.frame}},
\code{\link{class}},
\code{\link{colnames}}
}
\examples{
colID(iris)
}