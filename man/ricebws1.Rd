\name{ricebws1}
\alias{ricebws1}
\docType{data}
\title{
Consumers' preferences for rice characteristics
}
\description{
This dataset contains responses to Case 1 BWS questions about consumers' preferences for rice characteristics.
}
\usage{
data(ricebws1)
}
\format{
  A data frame with 90 respondents on the following 18 variables.
  \describe{
    \item{\code{id}}{Identification number of respondents.}
    \item{\code{b1}}{Item selected as the best in BWS question 1.}
    \item{\code{w1}}{Item selected as the worst in BWS question 1.}
    \item{\code{b2}}{Item selected as the best in BWS question 2.}
    \item{\code{w2}}{Item selected as the worst in BWS question 2.}
    \item{\code{b3}}{Item selected as the best in BWS question 3.}
    \item{\code{w3}}{Item selected as the worst in BWS question 3.}
    \item{\code{b4}}{Item selected as the best in BWS question 4.}
    \item{\code{w4}}{Item selected as the worst in BWS question 4.}
    \item{\code{b5}}{Item selected as the best in BWS question 5.}
    \item{\code{w5}}{Item selected as the worst in BWS question 5.}
    \item{\code{b6}}{Item selected as the best in BWS question 6.}
    \item{\code{w6}}{Item selected as the worst in BWS question 6.}
    \item{\code{b7}}{Item selected as the best in BWS question 7.}
    \item{\code{w7}}{Item selected as the worst in BWS question 7.}
    \item{\code{age}}{Respondents' age: 1 = <40; 2 = 40-<60; 3 = >=60}
    \item{\code{hp}}{Highest price of rice per 5 kg that respondents have purchased for the last six months: 1 = < 1600 JPY; 2 = 1600-<2100; 3 = >=2100}
    \item{\code{chem}}{Respondents' valuation of rice grown with low-chemicals: 1 if respondents value low-chemical rice and 0 otherwise}
  }
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link{bws.dataset}}, \code{\link[crossdes]{find.BIB}}
}
\examples{
# Respondents were asked to select their most and least important
# characteristics of rice when purchasing rice. Rice characteristics
# were assumed to be place of origin, variety, price, taste, safety,
# wash-free rice, and milling date. BWS questions were created from
# a balanced incomplete block design (BIBD) with seven treatments
# (items), four columns (four items per question), and seven rows
# (seven questions).

# Generate the BIBD using find.BIB() in the crossdes package:
require("crossdes")
set.seed(8041)
bibd.ricebws1 <- find.BIB(trt = 7, b = 7, k = 4)
isGYD(bibd.ricebws1)
bibd.ricebws1

# Store rice characteristics used in the survey to items.ricebws1:
items.ricebws1 <- c(
  "Place_of_origin",
  "Variety",
  "Price",
  "Taste",
  "Safety",
  "Washfree_rice",
  "Milling_date")

# Convert the BIBD into the BWS questions:
bws.questionnaire(bibd.ricebws1, design.type = 2,
  item.names = items.ricebws1)

# Load the dataset ricebws1 containing the responses to 
# the BWS questions:
data("ricebws1", package = "support.BWS")
dim(ricebws1)
names(ricebws1)

# Create the dataset for the analysis:
data.ricebws1 <- bws.dataset(
  respondent.dataset = ricebws1,
  response.type = 1,
  choice.sets = bibd.ricebws1,
  design.type = 2,
  item.names = items.ricebws1)

# Calculate BW scores:
count.ricebws1 <- bws.count(data = data.ricebws1)
count.ricebws1
}
\keyword{datasets}
