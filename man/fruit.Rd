\name{fruit}
\alias{fruit}
\docType{data}
\title{
Synthetic respondent data set: consumers' preferences for fruits
}
\description{
Data set artificially created for an example based on a BIBD. This example illustrates consumers' preferences for seven fruits: apple, orange, grapes, banana, peach, melon, and pear.
}
\usage{
data(fruit)
}
\format{
  A data frame with 100 respondents on the following 15 variables.
  \describe{
    \item{\code{ID}}{Identification number of respondents.}
    \item{\code{B1}}{Item selected as the best in question 1.}
    \item{\code{W1}}{Item selected as the worst in question 1.}
    \item{\code{B2}}{Item selected as the best in question 2.}
    \item{\code{W2}}{Item selected as the worst in question 2.}
    \item{\code{B3}}{Item selected as the best in question 3.}
    \item{\code{W3}}{Item selected as the worst in question 3.}
    \item{\code{B4}}{Item selected as the best in question 4.}
    \item{\code{W4}}{Item selected as the worst in question 4.}
    \item{\code{B5}}{Item selected as the best in question 5.}
    \item{\code{W5}}{Item selected as the worst in question 5.}
    \item{\code{B6}}{Item selected as the best in question 6.}
    \item{\code{W6}}{Item selected as the worst in question 6.}
    \item{\code{B7}}{Item selected as the best in question 7.}
    \item{\code{W7}}{Item selected as the worst in question 7.}
  }
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link{bws.dataset}}, \code{\link[crossdes]{find.BIB}}
}
\examples{
# The following BIBD is generated using find.BIB()
# in the crossdes package:
#  set.seed(123)
#  find.BIB(trt = 7, k = 4, b = 7)
sets.fruit <- cbind(
  c(1,2,2,1,1,3,1),
  c(4,3,4,2,3,5,2),
  c(6,4,5,5,4,6,3),
  c(7,6,7,6,5,7,7)) 
items.fruit <- c(
  "Apple",
  "Orange",
  "Grapes",
  "Banana",
  "Peach",
  "Melon",
  "Pear")
bws.questionnaire(
  choice.sets = sets.fruit,
  design.type = 2,
  item.names = items.fruit)
data(fruit)
data.fruit <- bws.dataset(
  respondent.dataset = fruit,
  response.type = 1,
  choice.sets = sets.fruit,
  design.type = 2,
  item.names = items.fruit)
count.fruit <- bws.count(data = data.fruit)
count.fruit
}
\keyword{datasets}
