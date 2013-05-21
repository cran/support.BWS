\name{bws.count}
\alias{bws.count}
\alias{print.bws.count}
\title{
Calculating count-based BW scores
}
\description{
This function calculates various BW scores on the basis of a counting approach.
}
\usage{
bws.count(data)

\method{print}{bws.count}(x, digits = max(3, getOption("digits") - 3), scientific = FALSE, ...)
}
\arguments{
  \item{data}{
A data frame containing the output from \code{bws.dataset()}.
}
  \item{x}{
An object of the S3 class \dQuote{bws.count}.
}
  \item{digits}{
The number of significant digits. See the \code{format()} function.
}
  \item{scientific}{
Scores are encoded in scientific format. See the \code{format()} function.
}
  \item{\dots}{
Arguments passed to the \code{format()} function.
}
}
\details{
The counting approach calculates several types of scores on the basis of the number of times (the frequency or count) item \eqn{i} is selected as the best (\eqn{B_{in}}) and the worst (\eqn{W_{in}}) across all the questions for respondent \eqn{n}. These scores are roughly divided into two categories: disaggregated (individual-level) scores and aggregated (total-level) scores (Finn and Louviere, 1992; Lee et al., 2007; Cohen, 2009; Mueller and Rungie, 2009).

The first category includes a disaggregated BW score and its standardized score:
\deqn{
BW_{in} = B_{in} - W_{in},
}
\deqn{
std.BW_{in} = \frac{BW_{in}}{r},
}
where \eqn{r} is the frequency with which item \eqn{i} appears across all questions.

The frequency with which item \eqn{i} is selected as the best across all questions for \eqn{N} respondents is defined as \eqn{B_{i}}. Similarly, the frequency with which item \eqn{i} is selected as the worst is defined as \eqn{W_{i}} (i.e., \eqn{B_{i} = \sum_{n=1}^{N} B_{in}}, \eqn{W_{i} = \sum_{n=1}^{N} W_{in}}). The second category includes the aggregated versions of \eqn{BW_{in}} and \eqn{std.BW_{in}}, as well as the square root of the ratio of \eqn{B_{i}} to \eqn{W_{i}} and its standardized score:
\deqn{
BW_{i} = B_{i} - W_{i},
}
\deqn{
std.BW_{i} = \frac{BW_{i}}{Nr},
}
\deqn{
sqrt.BW_{i} = \sqrt{\frac{B_{i}}{W_{i}}},
}
\deqn{
std.sqrt.BW_{i} = \frac{sqrt.BW_{i}}{max.sqrt.BW_{i}},
}
where \eqn{max.sqrt.BW_{i}} is the maximum value of \eqn{sqrt.BW_{i}}. 
}
\value{
The output from \code{bws.count()} is an object of the S3 class \dQuote{bws.count}, containing three components:

A list \code{disaggregate} contains five objects related to disaggregated scores.
\item{ID}{A vector showing the respondent's identification number.}
\item{B}{A matrix showing the number of times item \eqn{i} is selected as the best by each respondent.}
\item{W}{A matrix showing the number of times item \eqn{i} is selected as the worst by each respondent.}
\item{BW}{A matrix showing the difference between \code{B} and \code{W} for item \eqn{i} per respondent.}
\item{stdBW}{A matrix showing standardized \code{BW}.}

A data frame \code{aggregate} contains aggregated scores across all respondents.
\item{B}{A variable showing the number of times item \eqn{i} is selected as the best across all respondents.}
\item{W}{A variable showing the number of times item \eqn{i} is selected as the worst across all respondents.}
\item{BW}{A variable showing the difference between \code{B} and \code{W} for item \eqn{i} across all respondents.}
\item{stdBW}{A variable showing standardized \code{BW}.}
\item{sqrtBW}{A variable showing the square root of the ratio of \code{B} to \code{W} for item \eqn{i} across all respondents.}
\item{std.sqrtBW}{A variable showing the standardized \code{sqrtBW}.}

A list \code{information} contains basic information related to the BWS questions.
\item{nrespondents}{A variable showing the number of respondents.}
\item{nitems}{A variable showing the number of items.}
\item{fitem}{A variable showing the frequency of each item in the choice sets.}
\item{vnames}{A variable showing the names of each item.}
}
\references{
Cohen E (2009) Applying best-worst scaling to wine marketing. International Journal of Wine Business Research, 21(1), 8--23.

Finn A, Louviere JJ (1992) Determining the appropriate response to evidence of public concern: The case of food safety. Journal of Public Policy & Marketing, 11(1), 12--25.

Lee JA, Soutar GN, Louviere J (2007) Measuring values using best-worst scaling: The LOV example. Psychology & Marketing, 24(12), 1043--1058.

Mueller S, Rungie C (2009) Is there more information in best-worst choice data?: Using the attitude heterogeneity structure to identify consumer segments. International Journal of Wine Business Research, 21(1), 24--40.
}
\author{
Hideo Aizaki
}
\seealso{
\code{\link[support.BWS]{bws.dataset}}
}
\examples{
## See examples in bws.dataset()
}
\keyword{ datagen }
\keyword{ print }

