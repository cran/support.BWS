\name{bws.sp}
\alias{bws.sp}
\title{
Calculating shares of preference
}
\description{
This function calculates shares of preference for each item based on the conditional logit model choice rule.
}
\usage{
bws.sp(object, base, coef = NULL, order = FALSE, decreasing = FALSE, ...)
}
\arguments{
  \item{object}{
An output from the function \code{\link[survival]{clogit}} in \pkg{survival} or vector/matrix/data frame containing estimates.
}
  \item{base}{
A character showing the base item.
}
  \item{coef}{
A vector containing the names of item variables used in the model.
}
  \item{order}{
A logical value denoted by \code{TRUE} when the resultant shares are sorted or \code{FALSE} when not doing so.
}
  \item{decreasing}{
A logical value denoted by \code{TRUE} when the sort order is decreasing, or \code{FALSE} when it is increasing.
}
  \item{...}{
Arguments passed to a function used internally.
}

}
\details{
This function calculates a share of preference for item \eqn{i}. For details on the shares of preference, refer to \code{\link[support.BWS]{support.BWS-package}}. Althoug this function is developed for the function \code{\link[survival]{clogit}} in the \pkg{survival}, it may be available for other functions regarding discrete choice models. This function assumes a simple model without covariates.
}
\value{
A vector or data frame containing the calculated shares of preference for each item is returned.
}
\author{
Hideo Aizaki
}
\seealso{
\code{\link[support.BWS]{support.BWS-package}}, \code{\link[support.BWS]{bws.dataset}}, \code{\link[survival]{clogit}}
}
\examples{
## See examples in bws.dataset()
}
\keyword{ univar }

