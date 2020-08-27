bws.sp <- function(
  object,
  base,
  coef = NULL,
  order = FALSE,
  decreasing = FALSE,
  ...)
{

  sop <- function(x) {exp(x)/sum(exp(x))}

  if (is.vector(object)) {
    if (is.null(coef)) {
      b <- c(object, 0)
    } else {
      b <- c(object[coef], 0)
    }
    names(b)[length(b)] <- base

    rtn <- sop(x = b)

    if (isTRUE(order)) {
      rtn <- rtn[order(rtn, decreasing = decreasing, ...)]
    }

    return(rtn) 
  }

  if (is.matrix(object) || is.data.frame(object)) {
    objectDF <- data.frame(object)
    if (is.null(coef)) {
      b <- merge(objectDF, 0)
    } else {
      b <- merge(objectDF[, coef], 0)
    }
    names(b)[length(b)] <- base

    rtn <- t(apply(X = b, MARGIN = 1, FUN = sop))

    if (dim(object)[1] == 1) {
      var.names <- colnames(rtn)
      rtn <- as.vector(data.matrix(rtn))
      names(rtn) <- var.names
    }

    if (isTRUE(order)) {
      if (dim(object)[1] == 1) {
        rtn <- rtn[order(rtn, decreasing = decreasing, ...)]
      } else {
        warning(message = "argument order is ignored")
      }
    }

    return(rtn)
  }

  if (is.null(coef)) {
    b <- c(coef(object), 0)
  } else {
    b <- c(coef(object)[coef], 0)
  }
  names(b)[length(b)] <- base

  rtn <- sop(x = b)

  if (isTRUE(order)) {
    rtn <- rtn[order(rtn, decreasing = decreasing, ...)]
  }

  return(rtn)

}

