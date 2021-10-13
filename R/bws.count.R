##############################################
bws.count <- function(
  data,
  cl = 1)
{

  if (cl == 1){
    bws.count1(data = data)
  } else {
    if (attributes(data)$model == "sequential") {
      attributes(data)$model = "marginal"
    }
    bws.count2(data = data)
  }

}

##############################################
bws.count1 <- function(
  data)
{

# set variables, vectors, and matrices

  colnames(data)[which(colnames(data) == attributes(data)$id)] <- "ID"

  numItems       <- attributes(data)$nitems
  freqencyItem   <- attributes(data)$fitem
  variableNames  <- attributes(data)$vnames
  id             <- c(subset(data, data$RES == TRUE, select = "ID"))
  uniqueId       <- unique(id[[1]])
  numRespondents <- length(uniqueId)

# create BEST matrix (B) and WORST matrix (W)
 
  B <- data.matrix(subset(data, data$RES == TRUE, select = variableNames))
  rownames(B) <- NULL
  W <- B

  B[which(B == -1, arr.ind = TRUE)] <- 0
  W[which(W ==  1, arr.ind = TRUE)] <- 0
  W[which(W == -1, arr.ind = TRUE)] <- 1

  colnames(B) <- variableNames
  colnames(W) <- variableNames

  B <- cbind(ID = id[[1]], B)
  W <- cbind(ID = id[[1]], W)
 
# calculate various BW scores

  # disaggregated scores
  disaggreB <- do.call(rbind, by(B[, 2:(1 + numItems)], B[, "ID"], colSums))
  disaggreW <- do.call(rbind, by(W[, 2:(1 + numItems)], W[, "ID"], colSums))
  diffBW     <- disaggreB - disaggreW
  std.diffBW <- diffBW / freqencyItem
 
  # aggregated scores
  aggreB  <- colSums(disaggreB)
  aggreW  <- colSums(disaggreW)
  aggreBW     <- aggreB - aggreW
  std.aggreBW <- aggreBW / (numRespondents * freqencyItem)
  sqrt.aggreBW     <- sqrt(aggreB / aggreW)
  std.sqrt.aggreBW <- sqrt.aggreBW / max(sqrt.aggreBW)

# format and return output

  rtn <- list(disaggregate = list(ID           = uniqueId,
                                  B            = disaggreB,
                                  W            = disaggreW,
                                  BW           = diffBW,
                                  stdBW        = std.diffBW),

              aggregate    = data.frame(
                                  B            = aggreB,
                                  W            = aggreW,
                                  BW           = aggreBW,
                                  stdBW        = std.aggreBW,
                                  sqrtBW       = sqrt.aggreBW,
                                  std.sqrtBW   = std.sqrt.aggreBW),

              information  = list(nrespondents = numRespondents,
                                  nitems       = numItems,
                                  fitem        = freqencyItem,
                                  vnames       = variableNames))

  names(rtn$disaggregate)[which(names(rtn$disaggregate) == "ID")] <-
    attributes(data)$id

  class(rtn) <- "bws.count"

  return(rtn)

}


##############################################
bws.count2 <-
function (
  data,
  ...)
{

  variableNames <- attributes(data)$vnames
  freqencyItem  <- attributes(data)$fitem
  id.variable   <- attributes(data)$id
  model         <- attributes(data)$model
  dataset       <- data

  if (model == "marginal") {
    B <- subset(dataset,
                dataset$BW == 1 & dataset$RES == 1,
                select = c(id.variable, "Q", "BW", variableNames))

    W <- subset(dataset,
                dataset$BW == -1 & dataset$RES == 1,
                select = c(id.variable, "Q", "BW", variableNames))
    W1 <- W[, c(id.variable, "Q", "BW")]
    W2 <- W[, c(variableNames)]
    W2[which(W2 == -1, arr.ind = TRUE)] <- 1
    W <- cbind(W1, W2)
  } else {
    B <- subset(dataset,
                dataset$RES == 1,
                select = c(id.variable, "Q", variableNames))
    B1 <- B[, c(id.variable, "Q")]
    B2 <- B[, c(variableNames)]
    B2[which(B2 == -1, arr.ind = TRUE)] <- 0
    B <- cbind(B1, B2)

    W <- subset(dataset,
                dataset$RES == 1,
                select = c(id.variable, "Q", variableNames))
    W1 <- W[, c(id.variable, "Q")]
    W2 <- W[, c(variableNames)]
    W2[which(W2 ==  1, arr.ind = TRUE)] <- 0
    W2[which(W2 == -1, arr.ind = TRUE)] <- 1
    W <- cbind(W1, W2)
  }

  disaggreB <- do.call(rbind,
                       by(B[, c(id.variable, variableNames)],
                          B[, id.variable],
                          colSums))
  disaggreB[, id.variable] <- as.numeric(row.names(disaggreB))
  disaggreW <- do.call(rbind,
                       by(W[, c(id.variable, variableNames)],
                          W[, id.variable],
                          colSums))
  disaggreW[, id.variable] <- as.numeric(row.names(disaggreW))
  disaggreB <- data.frame(disaggreB)
  disaggreW <- data.frame(disaggreW)
  if (!all.equal(row.names(disaggreB), row.names(disaggreW))) stop()

  diffBW <- disaggreB - disaggreW
  diffBW[, id.variable] <- disaggreB[, id.variable]
  IDvar <- disaggreB[, id.variable]
  std.diffBW <- diffBW/freqencyItem
  std.diffBW[, id.variable] <- disaggreB[, id.variable]

  b.names   <- paste("b",   names(disaggreB)[-1],  sep = ".")
  w.names   <- paste("w",   names(disaggreW)[-1],  sep = ".")
  bw.names  <- paste("bw",  names(diffBW)[-1],     sep = ".")
  sbw.names <- paste("sbw", names(std.diffBW)[-1], sep = ".")
  names(disaggreB)[-1]  <- b.names
  names(disaggreW)[-1]  <- w.names
  names(diffBW)[-1]     <- bw.names
  names(std.diffBW)[-1] <- sbw.names
  rtn <- merge(x = disaggreB, y = disaggreW, by = id.variable)
  rtn <- merge(x = rtn, y = diffBW, by = id.variable)
  rtn <- merge(x = rtn, y = std.diffBW, by = id.variable)

  if(!isTRUE(all.equal(length(attributes(data)$respondent.characteristics),0))){
    original.data <- attributes(data)$data
    resp.cha.vars <- attributes(data)$respondent.characteristics
    rtn <- merge(
      x = rtn,
      y = original.data[, c(id.variable, resp.cha.vars)],
      by = id.variable)
  }

  attributes(rtn)$nrespondents <- length(IDvar)
  attributes(rtn)$nquestions   <- nrow(rtn)
  attributes(rtn)$nitems       <- attributes(data)$nitems
  attributes(rtn)$fitem        <- freqencyItem
  attributes(rtn)$vnames       <- variableNames
  attributes(rtn)$b.names      <- b.names
  attributes(rtn)$w.names      <- w.names
  attributes(rtn)$bw.names     <- bw.names
  attributes(rtn)$sbw.names    <- sbw.names

  class(rtn) <- c("bws.count2", "data.frame")

  return(rtn)

}


##############################################
barplot.bws.count2 <-function(
  height,
  score = c("bw", "b", "w", "sbw"),
  mfrow = NULL,
  mean = FALSE,
  error.bar = NULL,
  conf.level = 0.95,
  ...)
{

  score  <- match.arg(score)

  data <- height

  n <- nrow(data)

  if (isTRUE(mean)) {

    if (score == "sbw") {
      sub.var.names <- attributes(data)$sbw.names
      xlabel <- "Standardized best-worst score"
    } else if (score == "bw") {
      sub.var.names <- attributes(data)$bw.names
      xlabel <- "Best-worst score"
    } else if (score == "b") {
      sub.var.names <- attributes(data)$b.names
      xlabel <- "Best score"
    } else {
      sub.var.names <- attributes(data)$w.names
      xlabel <- "Worst score"
    }

    subdata <- data[, sub.var.names]
    mean    <- colMeans(subdata)
    order   <- order(mean)

    sd      <- apply(X = subdata, MARGIN = 2, FUN = sd)
    se      <- sd/sqrt(n)
    t       <- qt(p = 1 - (1- conf.level)/2, df = n - 1)

    if (is.null(error.bar)) {
      upper <- mean
      lower <- mean
    } else if (error.bar == "ci") {
      upper <- mean + t * se
      lower <- mean - t * se
    } else if (error.bar == "se") {
      upper <- mean + se
      lower <- mean - se
    } else if (error.bar == "sd") {
      upper <- mean + sd
      lower <- mean - sd
    }

    rtn <- data.frame(mean, sd, se, upper, lower, order)

    max.upper <- max(upper)
    min.lower <- min(lower)

    if (score == "bw" || score == "sbw") {
      xlimit <- c(min.lower * 1.1, max.upper * 1.1)
    } else {
      xlimit <- c(0, max.upper * 1.1)
    }

    y <- barplot(height = mean[order], horiz = TRUE,
                 xlim = xlimit,
                 xlab = xlabel,
                 ...)

    if (!is.null(error.bar)) {
      arrows(x0 = lower[order], y0 = y, x1 = upper[order], y1 = y,
             angle = 90, code = 3, length = 0.1)
    }

    invisible(rtn)

  } else {
    if (score == "sbw") {
      stop(message = "'sbw' is valid only when mean = TRUE")
    }

    SCOREtable <- bws.table(x = data, score = score)

    if (is.null(mfrow)) {
      mfrow <- c(3, ceiling(length(attributes(data)$fitem)/3))  
    }

    par(mfrow = mfrow)

    for(i in 1:length(SCOREtable)){
      barplot(height = SCOREtable[[i]],
              main = names(SCOREtable)[i],
              xlab = "Score",
              ylab = "Respondents",
              ylim = c(0, max(unlist(SCOREtable))),
              ...)
    }

    invisible(SCOREtable)
  }
}


##############################################
bws.table <- function(
  x,
  score = c("bw", "b", "w"),
  ...)
{

  score <- match.arg(score)

  if (score == "bw") {
    SCORE <- x[, attributes(x)$bw.names]
  } else if (score == "b") {
    SCORE <- x[, attributes(x)$b.names]
  } else {
    SCORE <- x[, attributes(x)$w.names]
  } 

  freq.levels <- attributes(x)$fitem
  num.levels <- length(freq.levels)

  for (i in 1:num.levels) {
    SCORE[, i] <-
      factor(SCORE[, i],
             levels = if (score == "bw") {
                        c(-freq.levels[i]:freq.levels[i])
                      } else {c(0:freq.levels[i])})
  }

  SCOREtable <- lapply(SCORE, table)

  return(SCOREtable)

}


##############################################
plot.bws.count2 <- function(
  x,
  score = c("bw", "b", "w"),
  pos = 1,
  xlab = NULL,
  ylab = NULL,
  ...)
{

  score <- match.arg(score)

  if (score == "bw") {
    SCORE <- x[, attributes(x)$bw.names]
  } else if (score == "b") {
    SCORE <- x[, attributes(x)$b.names]
  } else {
    SCORE <- x[, attributes(x)$w.names]
  }

  meanSCORE <- colMeans(SCORE)
  std.SCORE <- apply(SCORE, 2, sd)

  if(is.null(xlab)) xlab = "Mean"
  if(is.null(ylab)) ylab = "Standard Deviation"

  plot(x = meanSCORE, y = std.SCORE, xlab = xlab, ylab = ylab, ...)
  if (!is.null(pos)) {
    text(x = meanSCORE,
         y = std.SCORE,
         pos = pos,
         labels = names(meanSCORE))
  }

  invisible(list(mean = meanSCORE, sd = std.SCORE))

}


##############################################
sum.bws.count2 <- function(
  x,
  ...)
{

  B <- colSums(x[, attributes(x)$b.names])
  W <- colSums(x[, attributes(x)$w.names])
  names.B <- sub("b.", "", names(B))
  names.W <- sub("w.", "", names(W))

  if (!isTRUE(all.equal(names.B, names.W))) {
    stop("Names of B scores are different from those of W scores")
  }

  BW <- B - W
  rownames <- names.B

  rtn <- data.frame(B = B,
                    W = W,
                    BW = BW)

  row.names(rtn) <- rownames

  return(rtn)

}


############################################# Added Oct 2021
mean.bws.count2 <- function(
  x,
  ...)
{
  sums <- sum(x = x)

  item.names <- row.names(sums)

  nr      <- nrow(x)
  mB      <- sums$B/nr
  mW      <- sums$W/nr
  mBW     <- sums$BW/nr
  m.stdBW <- sums$BW/(nr * attributes(x)$fitem)

  rtn <- data.frame(B = mB,
                    W = mW,
                    BW = mBW,
                    stdBW = m.stdBW)

  row.names(rtn) <- item.names

  return(rtn)
}


##############################################
summary.bws.count2 <- function(
  object,
  ...)
{

  sums <- sum(x = object)

  item.names <- rownames(sums)

  nr    <- attributes(object)$nrespondents
  B     <- sums$B
  W     <- sums$W
  BW    <- sums$BW
  rank  <- rank(-BW, na.last = TRUE, ties.method = "min")

  mB      <- B/nr
  mW      <- W/nr
  mBW     <- BW/nr
  m.stdBW <- BW/(nr * attributes(object)$fitem)

  sqBW    <- sqrt(B/W)
  stdsqBW <- sqBW/max(sqBW)

  rtn <- data.frame(
    B = B,
    W = W,
    BW = BW,
    Rank = rank,
    meanB = mB,
    meanW = mW,
    meanBW = mBW,
    mean.stdBW = m.stdBW,
    sqrtBW = sqBW,
    std.sqrtBW = stdsqBW)

  attributes(rtn)$nrespondets = nr

  rownames(rtn) <- item.names

  class(rtn) <- c("summary.bws.count2", "data.frame")

  return(rtn)

}


##############################################
print.summary.bws.count2 <- function(
  x,
  digits = max(3, getOption("digits") - 3),
  scientific = FALSE,
  ...)
{

  cat("Number of respondents =", attributes(x)$nrespondets, "\n\n")
  base::print.data.frame(x, digits = digits, scientific = scientific)

  invisible(x)

}

