bws.dataset <- function(
  respondent.dataset,
  response.type = 1,
  choice.sets,
  design.type = 1,
  item.names = NULL,
  row.renames = TRUE,
  id = NULL,
  response = NULL,
  model = "maxdiff",
  delete.best = FALSE,
  version = NULL)
{

  model.original <- model
  if (isTRUE(delete.best)) {
    warning("Argument delete.best is deprecated. Please use argument model. Argument model was set as 'sequential'")
    model <- "sequential"
  }
  if (model == "sequential") {
    model.original <- "sequential"
    model <- "marginal"
    delete.best <- TRUE
  }


  data <- respondent.dataset
  design <- choice.sets

  if (is.null(version)) {
    nversions <- 1
  } else {
    col.version.respondent <- which(colnames(data)   == version)
    col.version.design     <- which(colnames(design) == version)
    tab.version.respondent <- table(data[, version])
    tab.version.design     <- table(design[, version])
    nversions              <- length(tab.version.respondent)

    if (!isTRUE(length(tab.version.design) == nversions)) {
      stop("Number of versions in respondent dataset should be the same as that in choice sets")
    }

    if (!isTRUE(all.equal(1L:nversions, as.integer(names(tab.version.respondent))))) {
      stop("Values of version variable in respondent dataset should be serial integers starting from 1")
    }

    if (!isTRUE(all.equal(1L:nversions, as.integer(names(tab.version.design))))) {
      stop("Values of version variable in choice sets should be serial integers starting from 1")
    }

    if (nversions > 1) {
      freq <- table(design[design[, col.version.design] == 1, -col.version.design])
      for (i in 2:nversions) {
        tmp <- table(design[design[, col.version.design] == i, -col.version.design])
        if (!isTRUE(all.equal(freq, tmp))) {
          stop("Frequency of item i in a version should be the same as that in the other version(s)")
        }
      }
    }
  }

  rtn <- NULL

  if (nversions == 1) {
    if (!is.null(version)) {
      design <- design[, -col.version.design]
    }
    rtn <- bws.dataset.base(
             respondent.dataset = data,
             response.type      = response.type,
             choice.sets        = design,
             design.type        = design.type,
             item.names         = item.names,
             row.renames        = row.renames,
             id                 = id, 
             response           = response,
             model              = model,
             delete.best        = delete.best)
  } else {
    for (i in 1:nversions) {
      sub.respondent <- data[data[, col.version.respondent] == i, ]
      sub.design     <- design[design[, col.version.design] == i, -col.version.design]
      sub.bwsdataset <- bws.dataset.base(
             respondent.dataset = sub.respondent,
             response.type      = response.type,
             choice.sets        = sub.design,
             design.type        = design.type,
             item.names         = item.names,
             row.renames        = row.renames,
             id                 = id, 
             response           = response,
             model              = model,
             delete.best        = delete.best)
    rtn <- rbind(rtn, sub.bwsdataset)
    }
  attributes(rtn)$nrespondents <- nrow(data)
  attributes(rtn)$data <- data
  }

  if (model.original == "sequential") attributes(rtn)$model <- "sequential"

  rtn

}

###########################################################

bws.dataset.base <- function(
  respondent.dataset,
  response.type = 1,
  choice.sets,
  design.type = 1,
  item.names = NULL,
  row.renames = TRUE,
  id = NULL,
  response = NULL,
  model = "maxdiff",
  delete.best = FALSE)
{

# set variables, vectors, and matrices

  na.fail(respondent.dataset)
  respData <- respondent.dataset

  if (is.null(id)) {
    original.id.name <- colnames(respData)[1]
    colnames(respData)[1] <- "ID"
  } else {
    original.id.name <- id
    colnames(respData)[which(colnames(respData) == id)] <- "ID"
  }

  choicesets        <- data.matrix(choice.sets)
  numQuestions      <- nrow(choicesets)
  numRespondents    <- nrow(respData)

  if(design.type == 1) { # OMED
    choicesets           <- choicesets - 1
    numItems             <- ncol(choicesets)
    frequencyItem        <- apply(choicesets, 2, table)[2, ]
    names(frequencyItem) <- c(1:length(frequencyItem))
  } else {               # BIBD
    numItems      <- length(table(choicesets))
    frequencyItem <- c(table(choicesets))
  }

  itemsInSet <- vector("list", numQuestions)
  if(design.type == 1) { # OMED
    for(i in 1:numQuestions) {
      itemsInSet[[i]] <- which(choicesets[i, ] == 1)
    }
  } else {               # BIBD
    for(i in 1:numQuestions) {
      itemsInSet[[i]] <- choicesets[i, ]
    }
  }

  numItemsInSet         <- sapply(itemsInSet, length)
  numPossiblePairsInSet <- numItemsInSet*(numItemsInSet - 1)

# reshape respondent dataset ('wide' -> 'long')

  if (is.null(response)) {
    covariate.names <- colnames(respData)[-c(1:(1 + 2 * numQuestions))]
    varying.names   <- colnames(respData)[c(2:(1 + 2 * numQuestions))]
  } else {
    covariate.names <-
      colnames(respData)[!(colnames(respData) %in% c("ID", response))]
    varying.names   <- response
  }

  longRespData <-
    reshape(respData,
            idvar = "ID",
            varying = varying.names,
            sep = "",
            direction = "long")
  temp <- which(colnames(longRespData) == "time")
  storage.mode(longRespData$time) <- "integer"
  colnames(longRespData)[temp:(temp + 2)] <- c("Q", "RES.B", "RES.W")

  if(response.type == 1) {
    for (i in 1:nrow(longRespData)) {
      longRespData[i, c(temp + 1, temp + 2)] <-
        itemsInSet[[longRespData[i, temp]]][as.numeric(longRespData[i,
                                                       c(temp + 1, temp + 2)])]
    }
  }

# make dataset for maxdiff model

  if (model == "maxdiff"){
    PAIR <- 1:numPossiblePairsInSet[1]
    Q    <- rep(1, numPossiblePairsInSet[1])
    for (i in 2:numQuestions) {
      PAIR <- c(PAIR, (1:numPossiblePairsInSet[i]))
      Q    <- c(Q, rep(i, numPossiblePairsInSet[i]))
    }

    temp <- data.frame(
      ID   = rep(respData$ID,
                 each = sum(numPossiblePairsInSet)),
      Q    = rep(Q, times = numRespondents),
      PAIR = rep(PAIR, times = numRespondents))

    longRespData <- merge(temp, longRespData, by = c("ID", "Q"))
    longRespData <-
      longRespData[order(longRespData$ID,
                         longRespData$Q,
                         longRespData$PAIR), ]

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
 
    designMatrix <- as.data.frame(designMatrix)

    item.names.temp  <- colnames(designMatrix)[c(5:ncol(designMatrix))]

    # merge respondent data set with design matrix
    dataset <- merge(longRespData, designMatrix, by = c("Q", "PAIR"))
    dataset <- dataset[order(dataset$ID, dataset$Q, dataset$PAIR), ]

    # create RES variable: dependent variable in clogit()
    TRUEorFALSE.B <- dataset$RES.B == dataset$BEST
    TRUEorFALSE.W <- dataset$RES.W == dataset$WORST
    dataset$RES   <- (TRUEorFALSE.B + TRUEorFALSE.W) == 2

    # create STR variable: stratification variable in clogit()
    dataset$STR <- dataset$ID * 100 + dataset$Q

####
    if (is.null(response)) { # dataset in version 0.1-x
      # change order of variables
      dataset <- dataset[, c("ID", "Q", "PAIR", 
                             "BEST", "WORST", "RES.B", "RES.W", "RES",
                             item.names.temp, "STR", covariate.names)]
####
      # relabel item variables
      if(is.null(item.names) == FALSE) {
        colnames(dataset)[9:(8 + numItems)] <- item.names
      } else {
        item.names <- item.names.temp
      }
    } else { # dataset in version 0.2-x
      # change order of variables
      dataset <- dataset[, c("ID", "Q", "PAIR", "BEST", "WORST",
                             "RES.B", "RES.W", "RES",
                             item.names.temp, "STR",
                             covariate.names)]

      # relabel item variables
      if(is.null(item.names) == FALSE) {
        colnames(dataset)[9:(8 + numItems)] <- item.names
      } else {
        item.names <- item.names.temp
      }
    }

# make dataset for marginal model

  } else {
    ALT <- 1:numItemsInSet[1]
    Q   <- rep(1, numItemsInSet[1])
    for (i in 2:numQuestions) {
      ALT <- c(ALT, (1:numItemsInSet[i]))
      Q   <- c(Q, rep(i, numItemsInSet[i]))
    }

    temp <- data.frame(
      ID  = rep(respData$ID,
                each = sum(numItemsInSet)),
      Q   = rep(Q, times = numRespondents),
      ALT = rep(ALT, times = numRespondents))

    longRespData <- merge(temp, longRespData, by = c("ID", "Q"))

  # make design matrix from choice sets

    # initial set
    designMatrix <- matrix(0,
                           nrow = sum(2 * numItemsInSet),
                           ncol = 4 + numItems)
    variableNames <- paste("ITEM", 1:numItems, sep = "")
    colnames(designMatrix) <- c("Q", "ALT", "BW", "Item", variableNames)
 
    # create Q, BW, and Item variables
    lastRow <- 0
    for(i in 1:numQuestions) {
      # create ALT variable
      ALT <- c(c(1:numItemsInSet[[i]]), c(1:numItemsInSet[[i]]))
      # create BW variable
      BW <- rep(c(1, -1), each = numItemsInSet[[i]])
      # create Item variable
      Item <- c(itemsInSet[[i]], itemsInSet[[i]])
      # combine Q, ALT, BW, and Item variables
      temp <- cbind(i, ALT, BW, Item)
      # store design matrix corresponding to i-th question in designMatrix
      designMatrix[(1 + lastRow):(lastRow + nrow(temp)), 1:4] <- temp
      lastRow <- lastRow + nrow(temp)
    }
 
    # assign values to ITEMj variables according to values of BEST and WORST:
    #  ITEMj = 1 if BEST = j; -1 if WORST = j; and 0 otherwise
    for(i in 1:nrow(designMatrix)) {
      designMatrix[i, c(designMatrix[i, "Item"] + 4)] <- designMatrix[i, "BW"]
    }
 
    designMatrix <- as.data.frame(designMatrix)

    item.names.temp  <- colnames(designMatrix)[c(5:ncol(designMatrix))]

    # merge respondent data set with design matrix
    dataset <- merge(longRespData, designMatrix, by = c("Q", "ALT"))

    # create RES variable: dependent variable in clogit()
    dataset$RES <- (dataset$RES.B == dataset$Item) * (dataset$BW ==  1) +
                   (dataset$RES.W == dataset$Item) * (dataset$BW == -1)
    dataset$RES <- dataset$RES == 1

    # create STR variable: stratification variable in clogit()
    dataset$STR <- dataset$ID * 1000 +
                   dataset$Q * 10 +
                   (dataset$BW == 1) +
                   (dataset$BW == -1) * 2
    dataset <- dataset[order(dataset$STR, dataset$ALT), ]

    # data set for marginal sequential model
    if (delete.best == TRUE) {
      select <- !(dataset$BW == -1 & dataset$Item == dataset$RES.B)
      dataset <- dataset[select, ]
    }

    # change order of variables
    dataset <- dataset[, c("ID", "Q", "ALT", "BW", "Item",
                           "RES.B", "RES.W", "RES",
                           item.names.temp, "STR",
                           covariate.names)]

    # relabel item variables
    if(is.null(item.names) == FALSE) {
      colnames(dataset)[9:(8 + numItems)] <- item.names
    } else {
      item.names <- item.names.temp
    }

  }

# return data set for analysis

  # change row names
  if(row.renames == TRUE) {
    rownames(dataset) <- c(1:nrow(dataset))
  }

  colnames(dataset)[which(colnames(dataset) == "ID")] <- original.id.name

  # assign attributes to data set
  attributes(dataset)$nitems       <- numItems
  attributes(dataset)$nrespondents <- numRespondents
  attributes(dataset)$fitem        <- frequencyItem
  attributes(dataset)$vnames       <- item.names
  attributes(dataset)$model        <- model
  attributes(dataset)$id           <- original.id.name
  attributes(dataset)$respondent.characteristics <- covariate.names
  attributes(dataset)$data         <- respondent.dataset

  return(dataset)

}

