\name{support.BWS-package}
\alias{support.BWS-package}
\alias{support.BWS}
\docType{package}
\title{
Basic functions for supporting an implementation of best-worst scaling
}
\description{
The package provides three basic functions that support an implementation of object case (Case 1) best-worst scaling: one for converting a two-level orthogonal main-effect design/balanced incomplete block design into questions; one for creating a data set suitable for analysis; and one for calculating count-based scores.
}
\details{
Object case (or Case 1) best-worst scaling (BWS), or maximum difference scaling (Finn and Louviere 1992) is a stated preference method. After listing the items (objects) evaluated by respondents, a number of different subsets of the items are constructed from the list using the design of experiments. Each of the subsets is presented as a choice set to respondents, who are then asked to select the best (or most important) item and the worst (or least important) item in the choice set. This question is repeated until all the subsets are evaluated.

There are two methods to construct choice sets for object case BWS: one uses a two-level orthogonal main-effect design (OMED) (Finn and Louviere, 1992) and the other uses a balanced incomplete block design (BIBD) (Auger et al., 2007). The first method uses a two-level OMED with \eqn{m} columns, where \eqn{m} is the total number of items evaluated: each column corresponds to an item and each row corresponds to a question. There are two values in the two-level OMEDs (e.g., 1 and 2): one value is interpreted as an item being \dQuote{absent} from the corresponding column and the other as being \dQuote{present.} In this way, we can decide which items are assigned to each question: for example, if a row in a two-level OMED contains a value of 2 (which means \dQuote{present} here ) in the 1st, 5th, and 8th columns, and a value of 1 in the other columns, these three items are presented in a question corresponding to the row.

The second method uses a BIBD, which is a category of designs in which a subset of treatments is assigned to each block. The features of a BIBD are expressed by \dQuote{number of treatments (items),} \dQuote{size of a block (number of items per question),} \dQuote{number of blocks (questions),} \dQuote{number of replications of each treatment (item),} and \dQuote{frequency that each pair of treatments (items) appears in the same block (question).} Each row corresponds to a question; the number of columns is equal to the number of items per question; and the level values correspond to item identification numbers. For example, assume that there are seven items, ITEM1, ITEM2, ..., and ITEM7, and a BIBD with seven rows, four columns, and seven level values (1, 2, ..., 7). Under these assumptions, if a row in the BIBD contains values of 1, 4, 6, and 7,  a set containing ITEM1, ITEM4, ITEM6, and ITEM7 is presented in a question corresponding to the row.

There are two approaches to analyzing responses to object case BWS questions: a counting approach and a modeling approach. The counting approach calculates several types of scores on the basis of the number of times (the frequency or count) item \eqn{i} is selected as the best (\eqn{B_{in}}) and the worst (\eqn{W_{in}}) among all the questions for respondent \eqn{n}. These scores are roughly divided into two categories: disaggregated (individual-level) scores and aggregated (total-level) scores (Finn and Louviere, 1992; Lee et al., 2007; Cohen, 2009; Mueller and Rungie, 2009).

The modeling approach uses discrete choice models to analyze responses. In the package, this approach is based on the understanding of respondents' behavior in the following situation (Finn and Louviere, 1992; Auger et al., 2007). Suppose that \eqn{m} items exist in a choice set (a question). The number of possible pairs in which item \eqn{i} is selected as the best and item \eqn{j} is selected as the worst (\eqn{i \neq j}) from \eqn{m} items is \eqn{m \times (m - 1)}. Respondents are assumed to have a utility (\eqn{v}) for each item. Further, they are assumed to select item \eqn{i} as the best and item \eqn{j} as the worst because the difference in utility between \eqn{i} and \eqn{j} represents the greatest utility difference. Under these assumptions, the probability of selecting item \eqn{i} as the best and item \eqn{j} as the worst is expressed as a conditional logit model:
\deqn{
Pr(i, j) = \frac{exp(v_{i} - v_{j})}{\sum_{k=1}^{m}\sum_{l=1, k \neq l}^{m}exp(v_{k} - v_{l})}.
}}

\author{
Hideo Aizaki
}
\references{
Auger P, Devinney TM, Louviere JJ (2007) Using best-worst scaling methodology to investigate consumer ethical beliefs across countries. Journal of Business Ethics, 70, 299--326.

Cohen E (2009) Applying best-worst scaling to wine marketing. International Journal of Wine Business Research, 21(1), 8--23.

Finn A, Louviere JJ (1992) Determining the appropriate response to evidence of public concern: The case of food safety. Journal of Public Policy & Marketing, 11(1), 12--25.

Lee JA, Soutar GN, Louviere J (2007) Measuring values using best-worst scaling: The LOV example. Psychology & Marketing, 24(12), 1043--1058.

Mueller S, Rungie C (2009) Is there more information in best-worst choice data?: Using the attitude heterogeneity structure to identify consumer segments. International Journal of Wine Business Research, 21(1), 24--40.
}
\keyword{ package }

