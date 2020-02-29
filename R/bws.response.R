bws.response <- function(
  design,
  item.names = NULL,
  b,
  n,
  detail = FALSE, 
  seed = NULL)
{
# Check arguments
  if (!is.null(item.names)) {
    if (length(b) != length(item.names)) {
      stop("length of item.names should be equal to that of b")
    }
  }

# Set variables
  design.dm <- data.matrix(design) 
  nR  <- n
  nI  <- length(table(design.dm))
  nPP <- ncol(design.dm) * (ncol(design.dm) - 1)

# Create design matrix for a respondent
  D <- bws.design.matrix(choice.sets = design.dm)

# Calculate utilities for items (alternatives)
  X <- kronecker(X = matrix(rep(1, times = nR), nrow = nR, ncol = 1),
                 Y = data.matrix(D))
  colnames(X) <- colnames(D)
  Xb <- sweep(x = X[, 5:(nI + 4)], MARGIN = 2, STATS = b, FUN = "*") 
  V <- rowSums(Xb)
  if (!is.null(seed)) set.seed(seed)
  e <- -log(-log(runif(n = length(V))))
  U <- V + e

# Search best and worst items according to U
  # a row corresponds to all pairs in a question
  Umat <- matrix(data = U, ncol = nPP, byrow = TRUE) 
  columns.max <- max.col(Umat)
  selected.cells <- cbind(R = 1:nrow(Umat), C = columns.max)
  resMat <- matrix(0L, nrow = nrow(Umat), ncol = ncol(Umat))
  resMat[selected.cells] <- 1L
  RES <- as.vector(t(resMat))

# return detailed dataset
  id  <- rep(1:nR, each = nrow(D))
  dataset <- data.frame(id = id, X, RES = RES)
  if (detail == TRUE) {
    if(!is.null(item.names)) {
      colnames(dataset)[6:(5 + length(item.names))] <- item.names
    }
    dataset$STR <- 100 * dataset$id + dataset$Q
    return(dataset)
  }

# construct simple dataset
  simple.dataset <- dataset[dataset$RES == 1, c("id", "Q", "BEST", "WORST")]
  colnames(simple.dataset)[c(3, 4)] <- c("B", "W")

# convert response var (B and W) format
  bib_all <- kronecker(X = matrix(rep(1, times = nR), nrow = nR, ncol = 1),
                       Y = design.dm)
  colB <- t(t(simple.dataset[, "B"]))
  tmpB <- sweep(x = bib_all, MARGIN = 1, STATS = colB, FUN = "==")
  tmpB <- which(tmpB == TRUE, arr.ind = TRUE)
  B <- tmpB[order(tmpB[, 1]), ]
  B <- B[, 2]
  colW <- t(t(simple.dataset[, "W"]))
  tmpW <- sweep(x = bib_all, MARGIN = 1, STATS = colW, FUN = "==")
  tmpW <- which(tmpW == TRUE, arr.ind = TRUE)
  W <- tmpW[order(tmpW[, 1]), ]
  W <- W[, 2]
  simple.dataset <- cbind(simple.dataset[, c(1, 2)], B = B, W = W)

# return simple dataset
  rtn <- reshape(simple.dataset, v.names = c("B", "W"), idvar = "id",
                 timevar = "Q", sep = "", direction = "wide")
  rtn <- data.frame(rtn)
  row.names(rtn) <- NULL

  return(rtn)
}

##########################################################################

bws.design.matrix <- function(choice.sets) 
{

# Set variables
  choicesets        <- data.matrix(choice.sets)
  numQuestions      <- nrow(choicesets)
  numItems          <- length(table(choicesets))
  frequencyItem     <- c(table(choicesets))
  itemsInSet        <- vector("list", numQuestions)
  for(i in 1:numQuestions) {
    itemsInSet[[i]] <- choicesets[i, ]
  }
  numItemsInSet         <- sapply(itemsInSet, length)
  numPossiblePairsInSet <- numItemsInSet * (numItemsInSet - 1)

# make dataset for maxdiff model
  PAIR <- 1:numPossiblePairsInSet[1]
  Q    <- rep(1, numPossiblePairsInSet[1])
  for (i in 2:numQuestions) {
    PAIR <- c(PAIR, (1:numPossiblePairsInSet[i]))
    Q    <- c(Q, rep(i, numPossiblePairsInSet[i]))
  }

# make design matrix from choice sets
  # initial set
  designMatrix <- matrix(0,
                         nrow = sum(numPossiblePairsInSet),
                         ncol = 4 + numItems)
  variableNames <- paste("ITEM", 1:numItems, sep = "")
  colnames(designMatrix) <- c("Q", "PAIR", "BEST", "WORST", variableNames)
  lastRow <- 0
 
  # create Q, PAIR, BEST, and WORST variables
  for(i in 1:numQuestions) {
    # create all combinations of items in choice set
    temp <- expand.grid(WORST = itemsInSet[[i]], BEST = itemsInSet[[i]])
    # exclude combinations of same item
    temp <- subset(temp, temp$BEST != temp$WORST)
    # combine Q and PAIR with possible pairs
    temp <- cbind(i, c(1:nrow(temp)), temp$BEST, temp$WORST)
    # store design matrix corresponding to i-th question in designMatrix
    designMatrix[(1 + lastRow):(lastRow + nrow(temp)), 1:4] <- temp
    lastRow <- lastRow + nrow(temp)
  }
 
  # assign values to ITEMj variables according to values of BEST and WORST:
  #  ITEMj = 1 if BEST = j; -1 if WORST = j; and 0 otherwise
  for(i in 1:nrow(designMatrix)) {
    designMatrix[i,
                 c(designMatrix[i, 3] + 4,
                   designMatrix[i, 4] + 4)] <- c(1, -1)
  }

  as.data.frame(designMatrix)
}

