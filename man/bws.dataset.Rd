\name{bws.dataset}
\alias{bws.dataset}
\title{
Creating a data set suitable for BWS analysis on the basis of counting and modeling approaches
}
\description{
This function creates a data set used for \code{bws.count()} in \pkg{support.BWS} and \code{clogit()} in \pkg{survival}.
}
\usage{
bws.dataset(respondent.dataset, response.type = 1,
            choice.sets, design.type = 1,
            item.names = NULL)
}
\arguments{
  \item{respondent.dataset}{
A data frame containing a respondent data set.
}
  \item{response.type}{
A value describing the format of the response variables: \code{1} if the response variables follow a row number format, and \code{2} if they follow an item number format.
}
  \item{choice.sets}{
A data frame or matrix containing choice sets.
}
  \item{design.type}{
A value describing how to design the choice sets: \code{1} if the design assigned to \code{choice.sets} is a two-level OMED, and \code{2} if it is a BIBD.
}
  \item{item.names}{
A vector containing the names of items: if it takes \code{NULL}, default item names (i.e., \code{ITEM1}, \code{ITEM2}, ...) are used in the resultant data set.
}
}
\details{
The respondent data set, in which each row corresponds to a respondent, has to be organized by users. The data set must contain the id variable in the first column, denoting the respondent's identification number, and the response variables in the subsequent columns, each indicating which items are selected as the best and the worst for each question. Although the names of the id and response variables are up to the discretion of the user, the response variables must be constructed such that the best alternates with the worst by question. For example, when there are seven BWS questions, the variables are \code{B1 W1 B2 W2 ... B7 W7}; here, \code{Bi} and \code{Wi} show which items are selected as the best and the worst in the \code{i}th question.

There are two types of data format related to response variables: one is a row number format, and the other is an item number format. In the former, the row numbers of the items selected as the best and the worst are stored in the response variables. In the latter, item numbers are stored in the response variables.

The arguments \code{choice.sets}, \code{design.type}, and \code{item.names} are the same as those in the \code{bws.questionnaire()} function. However, \code{item.names} can take \code{NULL} (default), when default item names (i.e., \code{ITEM1}, \code{ITEM2}, ...) are used in the resultant data set. Further, the order of questions in the choice sets has to be the same as that in the respondent data set.
}
\value{
The function returns a data set containing the following variables:
\item{ID}{A respondent's identification number, assigned according to the id variable in the respondent data set.}
\item{Q}{A serial number of BWS questions.}
\item{PAIR}{A serial number of possible pairs of the best and worst items for each question.}
\item{BEST}{An item number treated as the best in the possible pairs of the best and worst items for each question.}
\item{WORST}{An item number treated as the worst in the possible pairs of the best and worst items for each question.}
\item{RES.B}{An item number selected as the best by respondents.} 
\item{RES.W}{An item number selected as the worst by respondents.}
\item{ITEMj}{State variables related to the possible pairs of the best and worst items: \code{1} if item \code{j} is treated as the best in the possible pair, \code{-1} if item \code{j} is treated as the worst in the possible pair, and \code{0} otherwise.}
\item{RES}{Responses to BWS questions: \code{TRUE} if a possible pair of the best and worst items is selected by respondents and \code{FALSE} otherwise. This variable is used as a dependent variable in the model \code{formula} of \code{clogit()} in \pkg{survival} when analyzing responses to BWS questions.}
\item{STR}{A stratification variable identifying each combination of respondent and question. This variable is also used in \code{formula} of \code{clogit()}.}
}
\author{
Hideo Aizaki
}
\seealso{
\code{\link[support.BWS]{support.BWS-package}}, \code{\link[DoE.base]{oa.design}}, \code{\link[crossdes]{find.BIB}}, \code{\link[crossdes]{isGYD}}, \code{\link[survival]{clogit}}
}
\examples{
## load packages
require(DoE.base) # include oa.design() used to generate a two-level OMED
require(crossdes) # include find.BIB() used to generate a BIBD
require(survival) # include clogit() used to analyze responses

## case 1: BWS using a two-level OMED
## suppose that ten respondents answered twelve BWS questions valuing nine items

# create a two-level OMED with nine factors
set.seed(123) # set seed for random number generator
des1 <- oa.design(nfactors = 9, nlevels = 2)
des1 # resultant design with twelve rows, nine columns, and level values of 1 and 2

# set item names, in which the order of elements corresponds to 
#  the order of columns in des1
items1 <- LETTERS[1:9] # item names are "A", "B", ..., "I"

# create questions for BWS
bws.questionnaire(
 choice.sets = des1,
 design.type = 1, # OMED
 item.names = items1)

# set a respondent data set in a row number format
res1 <- data.frame(
 ID  = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), # id variable
 B1  = c(1, 1, 1, 1, 3, 5, 1, 1, 1, 1),  # best item in question 1
 W1  = c(5, 5, 3, 5, 4, 1, 4, 2, 4, 3),  # worst item in question 1
 B2  = c(1, 3, 1, 4, 1, 2, 3, 2, 1, 2),  # best item in question 2
 W2  = c(5, 5, 5, 5, 3, 5, 1, 4, 4, 5),  # worst item in question 2
 B3  = c(1, 2, 1, 2, 4, 1, 1, 3, 1, 3),  # best item in question 3
 W3  = c(4, 3, 3, 3, 3, 4, 4, 2, 3, 4),  # worst item in question 3
 B4  = c(2, 1, 3, 5, 2, 3, 1, 1, 2, 5),  # best item in question 4
 W4  = c(4, 4, 5, 3, 5, 5, 3, 5, 4, 3),  # worst item in question 4
 B5  = c(2, 3, 3, 2, 2, 2, 2, 1, 3, 2),  # best item in question 5
 W5  = c(4, 4, 4, 4, 3, 4, 4, 4, 4, 3),  # worst item in question 5
 B6  = c(2, 1, 1, 3, 3, 3, 1, 1, 1, 1),  # best item in question 6
 W6  = c(1, 2, 3, 2, 1, 2, 3, 2, 2, 3),  # worst item in question 6
 B7  = c(3, 3, 1, 1, 3, 6, 1, 2, 1, 7),  # best item in question 7
 W7  = c(9, 6, 8, 4, 8, 2, 6, 5, 4, 6),  # worst item in question 7
 B8  = c(2, 1, 2, 2, 2, 1, 1, 3, 1, 1),  # best item in question 8
 W8  = c(3, 3, 3, 3, 4, 4, 4, 4, 3, 4),  # worst item in question 8
 B9  = c(2, 1, 3, 1, 4, 2, 3, 4, 1, 1),  # best item in question 9
 W9  = c(3, 2, 2, 3, 3, 3, 2, 2, 3, 4),  # worst item in question 9
 B10 = c(1, 1, 1, 1, 1, 1, 1, 4, 3, 3),  # best item in question 10
 W10 = c(4, 2, 2, 4, 4, 3, 4, 2, 4, 4),  # worst item in question 10
 B11 = c(2, 1, 3, 3, 3, 2, 1, 2, 2, 4),  # best item in question 11
 W11 = c(1, 4, 4, 1, 1, 4, 4, 4, 1, 1),  # worst item in question 11
 B12 = c(2, 2, 1, 1, 1, 1, 3, 2, 1, 2),  # best item in question 12
 W12 = c(3, 3, 2, 3, 3, 2, 2, 3, 3, 3))  # worst item in question 12

# create a data set for analysis 
#  by combining the choice sets and respondent data set
dat1 <- bws.dataset(
 respondent.dataset = res1,
 response.type = 1, # row number format
 choice.sets = des1,
 design.type = 1)   # OMED

# analyze responses to BWS questions
# counting approach
bws.count(dat1)
# modeling approach
# note: ITEM5 is excluded from fr1 to normalize its coefficient to zero
fr1 <- RES ~ ITEM1 + ITEM2 + ITEM3 + ITEM4 + ITEM6 + ITEM7 +
             ITEM8 + ITEM9 + strata(STR)
clogit(formula = fr1, data = dat1)


## case 2: BWS using a balanced incomplete block design
## suppose that ten respondents answered seven BWS questions valuing seven items

# create a BIBD with seven items, four items per question, and seven questions
set.seed(123) # set seed for random number generator
des2 <- find.BIB(trt = 7, k = 4, b = 7)
isGYD(des2)  # check whether the design is a BIBD
des2 # resultant design with seven rows, four columns, and level values ranging from 1 to 7

# set item names, in which the order of element corresponds to 
#  the order of level values in des2
items2 <- LETTERS[1:7]

# create questions for BWS
bws.questionnaire(
 choice.sets = des2,
 design.type = 2, # BIBD
 item.names = items2)

# set a respondent data set in a row number format
res2 <- data.frame(
 ID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), # id variable
 B1 = c(2, 1, 2, 4, 2, 2, 2, 1, 2, 1),  # best item in question 1
 W1 = c(3, 4, 3, 3, 1, 3, 3, 2, 3, 3),  # worst item in question 1
 B2 = c(4, 3, 3, 3, 3, 1, 1, 2, 1, 1),  # best item in question 2
 W2 = c(3, 2, 4, 1, 2, 3, 3, 4, 2, 4),  # worst item in question 2
 B3 = c(3, 1, 1, 1, 1, 1, 1, 1, 2, 1),  # best item in question 3
 W3 = c(1, 4, 2, 2, 4, 4, 2, 3, 3, 3),  # worst item in question 3
 B4 = c(2, 2, 1, 3, 2, 2, 2, 2, 4, 1),  # best item in question 4
 W4 = c(4, 4, 3, 4, 1, 3, 4, 1, 2, 4),  # worst item in question 4
 B5 = c(1, 3, 2, 1, 3, 2, 1, 1, 1, 1),  # best item in question 5
 W5 = c(3, 1, 4, 4, 1, 4, 3, 2, 4, 3),  # worst item in question 5
 B6 = c(2, 1, 1, 3, 2, 4, 4, 3, 3, 3),  # best item in question 6
 W6 = c(3, 2, 3, 4, 3, 2, 2, 4, 4, 2),  # worst item in question 6
 B7 = c(2, 1, 3, 1, 3, 2, 3, 3, 2, 2),  # best item in question 7
 W7 = c(4, 4, 4, 4, 4, 1, 4, 1, 4, 4))  # worst item in question 7

# create a data set for analysis
#  by combining the choice sets and respondent data set
dat2 <- bws.dataset(
 respondent.dataset = res2,
 response.type = 1,   # row number format
 choice.sets = des2,
 design.type = 2,     # BIBD
 item.names = items2) # state variables are labeled using item names

# analyze responses to BWS questions
# counting approach
bws.count(dat2)
# modeling approach
# note: D is excluded from fr2 to normalized its coefficient to zero
fr2 <- RES ~ A + B + C + E + F + G + strata(STR)
clogit(fr2, data = dat2)
}
\keyword{ datagen }
\keyword{ survival }

