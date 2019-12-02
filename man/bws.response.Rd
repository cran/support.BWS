\name{bws.response}
\alias{bws.response}
\title{
Generating artificial responses to Case 1 best-worst scaling questions
}

\description{
This function synthesizes responses to Case 1 best-worst scaling (BWS) questions on the basis of a maximum difference model.
}

\usage{
bws.response(design, item.names = NULL, b, n, detail = FALSE,
             seed = NULL)
}

\arguments{
\item{design}{
A matrix or data frame containing a balanced incomplete block design (BIBD).
}
\item{item.names}{
A character vector containing the names of items: if \code{NULL} (default), default names (i.e., \code{ITEM1}, \code{ITEM2}, ...) are used in the resultant dataset. The argument is valid for the dataset in a detailed format.
}
\item{b}{
A vector containing parameters of independent variables in the model. The vector is used to calculate utilities for alternatives.
}
\item{n}{
An integer value showing the number of respondents in the resultant dataset.
}
\item{detail}{
A logical variable: if \code{TRUE}, the dataset is returned in a detailed format; and if \code{FALSE} (default), the dataset is returned in a simple format.
}
\item{seed}{
Seed for a random number generator.
}
}

\details{
This function synthesizes responses to Case 1 BWS questions on the basis of a maximum difference model. The model assumes that there are \eqn{m} items to be evaluated, and that \eqn{k} (\eqn{< m}) items exist in a choice set (a question). The number of possible pairs where item \eqn{i} is selected as the best and item \eqn{j} is selected as the worst (\eqn{i \neq j}) from \eqn{k} items is given by \eqn{k \times (k - 1)}. The model also assumes that the respondents select item \eqn{i} as the best and item \eqn{j} as the worst because the difference in utility between items \eqn{i} and \eqn{j} is the highest among all of the \eqn{k \times (k - 1)} differences in utility. The systematic component of the utility is assumed to be a linear additive function of the item variables. If the error component of the utility is assumed to be an independently, identically distributed type I extreme value, the probability of selecting item \eqn{i} as the best and item \eqn{j} as the worst is expressed as a conditional logit model.

Given the parameter values assigned to the argument \code{b}, and the choice sets assigned to the argument \code{design}, the function \code{bws.response} calculates the utility for the items. The parameter values assigned to the argument \code{b} are set as a numerical vector where the elements correspond to the parameters of item variables. For example, assume that seven items A, B, C, D, E, F, and G exist in the list for evaluation, and their corresponding dummy-coded item variables \eqn{D_{A}}, \eqn{D_{B}}, \eqn{D_{C}}, \eqn{D_{D}}, \eqn{D_{E}}, and \eqn{D_{G}} (item F is assumed to be the reference item) have parameter values of 0.5, 1.2, 1.6, 1.8, 2.1, and 0.9, respectively. A vector assigned to the argument \code{b} is \code{c(0.5, 1.2, 1.6, 1.8, 2.1, 0, 0.9)}, where the sixth element (i.e., item F) corresponds to the reference level, and thus has a value of \code{0}. After calculating the utility values (by adding the calculated values of the systematic component of the utility and random numbers generated from a type I extreme value distribution), the function \code{bws.response} finds the pair with the highest difference in utility from the \eqn{k \times (k - 1)} differences in utility.
}

\value{
The function \code{bws.response} returns a data frame that contains synthesized responses to Case 1 BWS questions, in either a detailed or a simple format. The detailed format dataset contains the following variables, as well as independent variables according to the argument \code{item.names}.
\item{id}{An identification number of artificial respondents.}
\item{Q}{A serial number of questions.}
\item{PAIR}{A serial number of possible pairs of the best and worst items for each question.}
\item{BEST}{An item number treated as the best in the possible pairs of the best and worst items for each question.}
\item{WORST}{An item number treated as the worst in the possible pairs of the best and worst items for each question.}
\item{RES}{Responses to BWS questions, taking the value of 1 if a possible pair of the best and worst items is selected by the synthesized respondents and 0 otherwise.}
\item{STR}{A stratification variable used to identify each combination of respondent and question.}

The simple format dataset contains the following variables.
\item{id}{An identification number of artificial respondents.}
\item{Bi}{A variable describing the row number of the item that is selected as the best in the \eqn{i}-th BWS question (see the help for \code{\link[support.BWS]{bws.dataset}} for a row number format). The serial number of questions is appended to the tail of the variable name (e.g., \code{B1} for the first question, \code{B2} for the second question, and \code{B3} for the third question).}
\item{Wi}{A variable describing the row number of the item that is selected as the worst in the \eqn{i}-th BWS question (see the help for \code{\link[support.BWS]{bws.dataset}} for a row number format). The serial number of questions is appended to the tail of the variable name (e.g., \code{W1} for the first question, \code{W2} for the second question, and \code{W3} for the third question).}

The detailed format dataset includes a dependent variable and independent variables for the analysis, and thus is available for discrete choice analysis functions such as the function \code{clogit} in the \bold{survival} package. On the other hand, the simple format dataset only contains variables that correspond to responses to BWS questions, as well as \code{id} variable. It must be converted using the function \code{bws.dataset} in the package for the analysis. For details, see the Example section.
}

\seealso{
\code{\link[support.BWS]{support.BWS-package}}, \code{\link[support.BWS]{bws.dataset}}
}

\examples{
# The following lines of code synthesize responses to Case 1 BWS questions,
# return them in detailed and simple format, and then fit the models using
# the function clogit in the survival package. The questions evaluate seven
# items. The choice sets consist of seven questions with four items each.
# The function find.BIB in the crossdes package creates the corresponding
# BIBD with seven treatments, seven blocks, and size four. The systematic
# component of the utility for items is the same as that explained in the
# Details section.

\dontrun{
# Load packages
library(survival)
library(crossdes)

# Generate BIBD
set.seed(123)
bibd <- find.BIB(trt = 7, b = 7, k = 4)
isGYD(bibd)
bibd

# Synthesize responses to Case 1 BWS questions
b <- c(0.5, 1.2, 1.6, 1.8, 2.1, 0, 0.9)
items = c("A", "B", "C", "D", "E", "F", "G")
dat.detail <- bws.response(
  design = bibd, item.names = items,
  b = b, n = 100,
  detail = TRUE, seed = 123)
str(dat.detail)
dat.simple <- bws.response(
  design = bibd,
  b = b, n = 100,
  detail = FALSE, seed = 123)
str(dat.simple)

# Convert dat.simple into dataset for the analysis
response.vars <- colnames(dat.simple)[-1]
dat.simple.md <- bws.dataset(
  respondent.dataset = dat.simple,
  response.type = 1,
  choice.sets = bibd,
  design.type = 2,
  item.names = items,
  id = "id",
  response = response.vars,
  model = "maxdiff")

# Fit conditional logit models
mf <- RES ~ A + B + C + D + E + G + strata(STR)
out.detail <- clogit(mf, dat.detail)
out.simple <- clogit(mf, dat.simple.md)
out.simple
all.equal(coef(out.detail), coef(out.simple))
}
}

\keyword{ datagen }

