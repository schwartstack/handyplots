\name{wordcount}
\alias{wordcount}
\title{
Word Count
}
\description{
The function takes a text file or text string and outputs a \code{\link{barplot}} of the most frequently occuring words.
}
\usage{
wordcount(file = "", n, decreasing = T, text)
}
\arguments{
  \item{file}{
A text file whose location is interpreted relative to the current working directory (given by \code{\link{getwd}}). Can be left blank, in which case the text must be inputted as the argument \code{text}.
}
  \item{n}{
The number of words to show in the \code{\link{barplot}}. Should be an integer greater than 0 and less than the number of unique words in the text. For example, if \code{n}=5, then the \code{\link{barplot}} will show the 5 most frequently occuring words.
}
  \item{decreasing}{
If \code{TRUE} (the default), the words in the \code{\link{barplot}} will show from most frequent on the left to least frequent on the right. If \code{FALSE}, the most frequently occuring word will be on the right hand side of the \code{\link{barplot}}.
}
  \item{text}{
If you wish to enter text as an inline argument rather than as a file on your computer, you can enter your text as this argument and leave \code{file} blank.
}
}
\author{
Jonathan Schwartz
}
\seealso{
\code{\link{scan}},
\code{\link{barplot}}
}
\examples{
write("Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal. Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this. But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us -- that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion -- that we here highly resolve that these dead shall not have died in vain -- that this nation, under God, shall have a new birth of freedom -- and that government of the people, by the people, for the people, shall not perish from the earth.",file="wordcounttest.txt")
wordcount("wordcounttest.txt",5)

##or text can be entered inline
wordcount(text="Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal. Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this. But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us -- that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion -- that we here highly resolve that these dead shall not have died in vain -- that this nation, under God, shall have a new birth of freedom -- and that government of the people, by the people, for the people, shall not perish from the earth.",n=5)
}