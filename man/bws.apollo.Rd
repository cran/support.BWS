\name{bws.apollo}
\alias{bws.apollo}

\title{
Converting a dataset into that for Apollo
}

\description{
This function converts a dataset generated using \code{bws.dataset} into that in a format suitable for a modeling analysis using the \pkg{apollo} package.
}

\usage{
bws.apollo(data, detail = FALSE)
}

\arguments{
  \item{data}{
A data frame containing the output from the function \code{\link[support.BWS]{bws.dataset}}.
}
  \item{detail}{
A logical value: if \code{TRUE}, the dataset is returned in a detailed format; and if \code{FALSE} (default), the dataset is returned in a simple format.
}
}

\details{
This function converts a dataset generated using \code{\link[support.BWS]{bws.dataset}} into that in a format suitable for the modeling analysis using the \pkg{apollo} package. The \pkg{apollo} is a comprehensive package for choice modelling. The format of a dataset suitable for \pkg{apollo} differs from that for \code{\link[survival]{clogit}} in \pkg{survival}, \code{\link[mlogit]{mlogit}} in \pkg{mlogit}, and \code{\link[gmnl]{gmnl}} in \pkg{gmnl}. The former assumes that a row corresponds to a question, while the latter assumes that a row corresponds to an alternative. Therefore, the output from \code{\link[support.BWS]{bws.dataset}}, which is suitable for \code{\link[survival]{clogit}}, \code{\link[mlogit]{mlogit}}, and \code{\link[gmnl]{gmnl}}, has to be converted into a dataset for the modeling analysis using \pkg{apollo}.
}

\value{
The function \code{bws.apollo} returns a data frame that contains variables \code{id}, \code{Q}, and \code{RES}, as well as independent variables in a simple format. While the variable \code{id} is assigned to \code{indivID} used in the section \dQuote{Definition of core settings} of Apollo's model estimation script, the variable \code{RES} is assigned to \code{choiceVar} used in the section \dQuote{Likelihood definition}. The serial number of alternatives is appended to the tail of the independent variable name: e.g., \code{Item1.1} for the first alternative, \code{Item1.2} for the second alternative, and \code{Item1.3} for the third alternative. The independent variables are also used in the section \dQuote{Likelihood definition}. The detailed format dataset includes variables that are not used for the analysis with Apollo. For details on Apollo's model estimation script, refer to the vignette (\href{https://cran.r-project.org/package=apollo}{https://cran.r-project.org/package=apollo}) and the the user manual (Hess and Palma 2019b).
}

\author{
Hideo Aizaki
}

\references{
Hess S, Palma D (2019a) Apollo: a flexible, powerful and customisable freeware package for choice model estimation and application. \emph{Journal of Choice Modelling}, \bold{32}, 100170. \url{https://doi.org/10.1016/j.jocm.2019.100170}.

Hess S, Palma D (2019b) Apollo version 0.0.9, user manual, \url{http://www.apollochoicemodelling.com/}.
}

\seealso{
\code{\link[support.BWS]{support.BWS-package}}, \code{\link[support.BWS]{bws.dataset}}, \code{\link[survival]{clogit}}, \code{\link[mlogit]{mlogit}}, \code{\link[gmnl]{gmnl}}, \code{\link[apollo]{apollo_estimate}}, \code{\link[apollo]{apollo_mnl}}
}

\keyword{ manip }

