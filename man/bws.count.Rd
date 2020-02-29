\name{bws.count}
\alias{bws.count}
\alias{print.bws.count}
\alias{plot.bws.count2}
\alias{barplot.bws.count2}
\alias{sum.bws.count2}
\alias{summary.bws.count2}
\alias{print.summary.bws.count2}
\title{
Calculating count-based BW scores
}

\description{
This function calculates various BW scores on the basis of a counting approach.
}

\usage{
bws.count(data, cl = 1)

\method{print}{bws.count}(x, digits = max(3, getOption("digits") - 3), scientific = FALSE, ...)

\method{plot}{bws.count2}(x, score = c("bw", "b", "w"), pos = 1, xlab = NULL, ylab = NULL, ...)

\method{barplot}{bws.count2}(height, score = c("bw", "b", "w"), mfrow = NULL, mean = FALSE, 
error.bar = NULL, conf.level = 0.95, ...)

\method{sum}{bws.count2}(x, ...)

\method{summary}{bws.count2}(object, ...)

\method{print}{summary.bws.count2}(x, digits = max(3, getOption("digits") - 3), scientific = FALSE, ...)
}
\arguments{
  \item{data}{
A data frame containing the output from \code{bws.dataset()}.
}
  \item{cl}{
A value describing the S3 class of the object created by this function: \code{1} for the S3 class \code{"bws.count"}, and \code{2} for the S3 class \code{"bws.count2"}.
}
  \item{x, height, object}{
An object of the S3 class \code{"bws.count"} or \code{"bws.count2"}.
}
  \item{digits}{
The number of significant digits. See the \code{\link[base]{format}} function.
}
  \item{scientific}{
Scores are encoded in scientific format. See the \code{\link[base]{format}} function.
}
  \item{score}{
A character showing a type of the output from this function: \code{"b"} is assigned to this argument when the output is based on best scores, \code{"w"} is assigned when it is based on worst scores, or \code{"bw"} is assigned when it is based on best-minus-worst scores.
}
  \item{pos}{
A value showing a position of labels for points in the plot. See the argument \code{pos} of the \code{\link[graphics]{text}} function.
}
  \item{xlab}{
A character showing a label for the x axis.
}
  \item{ylab}{
A character showing a label for the y axis.
}
  \item{mfrow}{
A two-element vector \code{c(nr, nc)}: bar plots will be drawn in an \code{nr}-by-\code{nc} array on the device by row.
}
  \item{mean}{
A logical value denoted by \code{TRUE} when drawing a bar plot of the aggregated standardized BW scores or \code{FALSE} (default) when drawing bar plots of B, W, or BW scores.
}
  \item{error.bar}{
A character shoiwng a type of error bar adding on the bar plot of the aggregated standardized BW scores: \code{"sd"} for the standard deviation; \code{"se"} for the standard error; \code{"ci"} for the confidence interval; and \code{NULL} (default) for none.
}
  \item{conf.level}{
A value showing the confidence level when adding the confidence interval on the bar plot using \code{error.bar}.
}
  \item{\dots}{
Arguments passed to a function used internally.
}
}

\details{
This function calculates various BW scores on the basis of the counting approach. For details on the scores, refer to \code{\link[support.BWS]{support.BWS-package}}. 

When using this function with the argument \code{cl = 1}, it returns an object of the S3 class \code{"bws.count"}, containing disaggregated scores and aggregated scores in list format. The first category includes disaggregated best (B), worst (W), best-minus-worst (BW), and standardized BW scoers. The second category includes aggregated B, W, BW, and standardized BW scores as well as the square root of the ratio of the aggregated B to the aggregated W and its standardized scores. The generic function \code{print()} is available for the S3 class \code{"bws.count"}. The \code{print()} shows a summary of disaggregated scores and a table of aggregated scores.

When using this function with the argument \code{cl = 2}, it returns an object of the S3 class \code{"bws.count2"}, which inherits from the S3 class \code{"data.frame"}, including disaggregated B, W, BW, and standardized BW scores, respondent identification number variable, and respondent characteristic variables. The generic functions such as \code{plot()}, \code{barplot()}, \code{sum()}, and \code{summary()} are available for the S3 class \code{"bws.count2"}. The \code{plot()} draws the relationship between means and standard deviations of B, W, or BW scores. The \code{barplot()} draws the bar plot of the aggregated standardized BW scores or the bar plots of B, W, or BW scores. The \code{sum()} returns the aggregated B, W, BW, and standardized BW scores in data frame format. The \code{summary()} calculates (1) aggregated B, W, BW, and standardized BW scores, (2) item ranks based on the BW score, (3) means of B, W, BW, and standardized BW scores, and (4) the square root of the aggregated B to the aggregated W and its standardized scores. 

}

\value{
The output from \code{bws.count()} with the argument \code{cl = 1} is an object of the S3 class \code{"bws.count"}, containing three components:

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

The output from \code{bws.count()} with the argument \code{cl = 2}, which is an object of the S3 class \code{"bws.count2"}, is a data frame containing respondent identification number variable, B score variables, W score variables, BW score variables, standardized BW score variables, and respondent characteristic variables. These scores are calculated by each respondent. 

Note that the S3 class \code{"bws.count"} would be replaced by the S3 class \code{"bws.count2"} in future.

}
\author{
Hideo Aizaki
}
\seealso{
\code{\link[support.BWS]{support.BWS-package}}, \code{\link[support.BWS]{bws.dataset}}
}
\examples{
## See examples in bws.dataset()
}
\keyword{ univar }

