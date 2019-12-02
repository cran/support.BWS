\name{support.BWS-package}
\alias{support.BWS-package}
\alias{support.BWS}
\docType{package}
\title{
Tools for Case 1 best-worst scaling
}
\description{
The package provides three basic functions that support an implementation of object case (Case 1) best-worst scaling: one for converting a two-level orthogonal main-effect design/balanced incomplete block design into questions; one for creating a data set suitable for analysis; and one for calculating count-based scores.
}
\details{
Object case (or Case 1) best-worst scaling (BWS), or maximum difference scaling (MaxDiff) (Finn and Louviere 1992) is a stated preference method. After listing the items (objects) evaluated by respondents, a number of different subsets of the items are constructed from the list using the design of experiments. Each of the subsets is presented as a choice set to respondents, who are then asked to select the best (or most important) item and the worst (or least important) item in the choice set. This question is repeated until all the subsets are evaluated.

There are two methods to construct choice sets for object case BWS: one uses a two-level orthogonal main-effect design (OMED) (Finn and Louviere, 1992) and the other uses a balanced incomplete block design (BIBD) (Auger et al., 2007). The first method uses a two-level OMED with \eqn{T} columns, where \eqn{T} is the total number of items evaluated: each column corresponds to an item and each row corresponds to a question. There are two values in the two-level OMEDs (e.g., 1 and 2): one value is interpreted as an item being \dQuote{absent} from the corresponding column and the other as being \dQuote{present.} In this way, we can decide which items are assigned to each question: for example, if a row in a two-level OMED contains a value of 2 (which means \dQuote{present} here ) in the 1st, 5th, and 8th columns, and a value of 1 in the other columns, these three items are presented in a question corresponding to the row.

The second method uses a BIBD, which is a category of designs in which a subset of treatments is assigned to each block. The features of a BIBD are expressed by \dQuote{number of treatments (items),} \dQuote{size of a block (number of items per question),} \dQuote{number of blocks (questions),} \dQuote{number of replications of each treatment (item),} and \dQuote{frequency that each pair of treatments (items) appears in the same block (question).} Each row corresponds to a question; the number of columns is equal to the number of items per question; and the level values correspond to item identification numbers. For example, assume that there are seven items, ITEM1, ITEM2, ..., and ITEM7, and a BIBD with seven rows, four columns, and seven level values (1, 2, ..., 7). Under these assumptions, if a row in the BIBD contains values of 1, 4, 6, and 7,  a set containing ITEM1, ITEM4, ITEM6, and ITEM7 is presented in a question corresponding to the row.

There are two approaches to analyzing responses to object case BWS questions: a counting approach and a modeling approach. The counting approach calculates several types of scores on the basis of the number of times (the frequency or count) item \eqn{i} is selected as the best (\eqn{B_{in}}) and the worst (\eqn{W_{in}}) among all the questions for respondent \eqn{n}. These scores are roughly divided into two categories: disaggregated (individual-level) scores and aggregated (total-level) scores (Finn and Louviere, 1992; Lee et al., 2007; Cohen, 2009; Mueller and Rungie, 2009).

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

The modeling approach uses discrete choice models to analyze responses. In the package, this approach is based on the understanding of respondents' behavior in the following situation (Finn and Louviere, 1992; Auger et al., 2007). Suppose that \eqn{m} items exist in a choice set (a question). The number of possible pairs in which item \eqn{i} is selected as the best and item \eqn{j} is selected as the worst (\eqn{i \neq j}) from \eqn{m} items is \eqn{m \times (m - 1)}. Respondents are assumed to have a utility (\eqn{v}) for each item. Further, they are assumed to select item \eqn{i} as the best and item \eqn{j} as the worst because the difference in utility between \eqn{i} and \eqn{j} represents the greatest utility difference (the maxdiff model). Under these assumptions, the probability of selecting item \eqn{i} as the best and item \eqn{j} as the worst is expressed as a conditional logit model:
\deqn{
Pr(i, j) = \frac{\exp(v_{i} - v_{j})}{\sum_{k=1}^{m}\sum_{l=1, k \neq l}^{m}\exp(v_{k} - v_{l})}.
}

A share of preference for item \eqn{i} (\eqn{SP_{i}}) based on the conditional logit model choice rule is as follows (Cohen, 2003; Cohen and Neira, 2004; Lusk and Briggeman, 2009):
\deqn{
SP_{i} = \frac{\exp(v_{i})}{\sum_{t=1}^{T}\exp(v_{t})}.
}

Version 0.2-0 and later versions of the package are also available for the marginal and marginal sequential models (Hensher et al., 2015, Appendix 6B) in the modeling approach.

\bold{Acknowledgments}

This work was supported by JSPS KAKENHI Grant Numbers JP25450341 and JP16K07886.
}

\author{
Hideo Aizaki
}
\references{
Aizaki H, Nakatani T, Sato K (2014) \emph{Stated Preference Methods Using R}. CRC Press. \url{https://doi.org/10.1201/b17292}.

Auger P, Devinney TM, Louviere JJ (2007) Using best-worst scaling methodology to investigate consumer ethical beliefs across countries. \emph{Journal of Business Ethics}, \bold{70}, 299--326. \url{https://doi.org/10.1007/s10551-006-9112-7}.

Cohen E (2009) Applying best-worst scaling to wine marketing. \emph{International Journal of Wine Business Research}, \bold{21}(1), 8--23. \url{https://doi.org/10.1108/17511060910948008}.

Cohen SH (2003) Maximum difference scaling: Improved measures of importance and preference for segmentation. \emph{Sawtooth Software Research Paper Series}, 1--17. \url{https://www.sawtoothsoftware.com/support/technical-papers/maxdiff-best-worst-scaling/maximum-difference-scaling-improved-measures-of-importance-and-preference-for-segmentation-2003}.

Cohen S, Neira L (2004) Measuring preference for product benefits across countries: Overcoming scale usage bias with maximum difference scaling. \emph{Excellence in International Research}, 1--22.

Finn A, Louviere JJ (1992) Determining the appropriate response to evidence of public concern: The case of food safety. \emph{Journal of Public Policy & Marketing}, \bold{11}(2), 12--25. \url{https://doi.org/10.1177/074391569201100202}.

Hensher DA, Rose JM, Greene WH (2015) \emph{Applied Choice Analysis}. 2nd edition. Cambridge University Press. \url{https://doi.org/10.1017/CBO9781316136232}.

Lee JA, Soutar GN, Louviere J (2007) Measuring values using best-worst scaling: The LOV example. \emph{Psychology & Marketing}, \bold{24}(12), 1043--1058. \url{https://doi.org/10.1002/mar.20197}.

Lusk JL, Briggeman BC (2009) Food values. 
\emph{American Journal of Agricultural Economics}, \bold{91}(1), 184--196. 

Louviere JJ, Flynn TN, Marley AAJ (2015) \emph{Best-Worst Scaling: Theory, Methods and Applications}. Cambridge University Press. \url{https://doi.org/10.1017/CBO9781107337855}.

Mueller S, Rungie C (2009) Is there more information in best-worst choice data?: Using the attitude heterogeneity structure to identify consumer segments. \emph{International Journal of Wine Business Research}, \bold{21}(1), 24--40. \url{https://doi.org/10.1108/17511060910948017}.
}
\keyword{ package }

