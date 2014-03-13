\name{mfa}
\alias{mfa}
\docType{data}
\title{
Synthetic respondent data set: citizens' preferences for the multifunctionality of agriculture
}
\description{
Data set artificially created for an example based on a two-level OMED. This example illustrates citizens' preferences for the multifunctionality of agriculture: landscape, biodiversity, water use, land conservation, flood control, rural viability, food security, animal welfare, and cultural heritage.
}
\usage{
data(mfa)
}
\format{
  A data frame with 100 respondents on the following 25 variables.
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
    \item{\code{B8}}{Item selected as the best in question 8.}
    \item{\code{W8}}{Item selected as the worst in question 8.}
    \item{\code{B9}}{Item selected as the best in question 9.}
    \item{\code{W9}}{Item selected as the worst in question 9.}
    \item{\code{B10}}{Item selected as the best in question 10.}
    \item{\code{W10}}{Item selected as the worst in question 10.}
    \item{\code{B11}}{Item selected as the best in question 11.}
    \item{\code{W11}}{Item selected as the worst in question 11.}
    \item{\code{B12}}{Item selected as the best in question 12.}
    \item{\code{W12}}{Item selected as the worst in question 12.}
  }
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link{bws.dataset}}, \code{\link[DoE.base]{oa.design}}
}
\examples{
# The following OA is generated using oa.design()
# in the DoE.base package:
#  set.seed(123)
#  oa.design(nfactors = 9, nlevels = 2)
sets.mfa <- cbind(
  c(1,2,1,2,2,1,2,2,1,1,1,2),
  c(2,1,2,1,2,1,2,2,1,1,2,1),
  c(1,2,1,1,2,1,2,1,2,2,2,1),
  c(1,2,2,2,1,2,2,1,1,1,2,1),
  c(2,2,2,1,1,1,2,1,2,1,1,2),
  c(1,1,2,2,1,1,2,2,2,2,1,1),
  c(2,1,1,2,2,2,2,1,2,1,1,1),
  c(2,1,1,2,1,1,2,1,1,2,2,2),
  c(2,2,1,1,1,2,2,2,1,2,1,1))
items.mfa <- c(
  "Landscape",
  "Biodiversity",
  "Water use",
  "Land conservation",
  "Flood control",
  "Rural viability",
  "Food security",
  "Animal welfare",
  "Cultural heritage")
bws.questionnaire(
  choice.sets = sets.mfa,
  design.type = 1,
  item.names = items.mfa)
data(mfa)
data.mfa <- bws.dataset(
  respondent.dataset = mfa,
  response.type = 1,
  choice.sets = sets.mfa,
  design.type = 1,
  item.names = items.mfa)
count.mfa <- bws.count(data = data.mfa)
count.mfa
}
\keyword{datasets}
