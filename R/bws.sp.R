bws.sp <- function(
  object,
  base,
  coef = NULL,
  order = FALSE,
  decreasing = FALSE,
  ...)
{

  if (is.null(coef)) {
    b <- c(coef(object), 0)
  } else {
    b <- c(coef(object)[coef], 0)
  }

  names(b)[length(b)] <- base

  rtn <- exp(b)/sum(exp(b))

  if (isTRUE(order)) {
    rtn <- rtn[order(rtn, decreasing = decreasing, ...)]
  }

  rtn

}

