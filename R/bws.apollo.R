bws.apollo <- function(
  data,
  detail = FALSE) 
{

  itemVars <- attributes(data)$vnames

  if (attributes(data)$model == "maxdiff") {
    nPairs <- length(table(data$PAIR))

    rtn <- reshape(
      data, timevar = "PAIR", idvar = "STR",
      v.names = c("RES.B", "RES.W", "BEST", "WORST", "RES", itemVars),
      direction = "wide")

    rtn$RES <- eval(parse(text = paste("rtn$RES.", 1:nPairs, "*", 1:nPairs, 
                                       sep = "", collapse = "+")))

    if (!isTRUE(detail)) {
      deleteVars <- c(paste0("RES.B.", 1:nPairs), paste0("RES.W.", 1:nPairs),
                       paste0("RES.", 1:nPairs),
                       paste0("BEST.", 1:nPairs), paste0("WORST.", 1:nPairs),
                       "STR")
      rtn <- rtn[, !c(colnames(rtn) %in% deleteVars)]
    }

  } else if (attributes(data)$model == "marginal") {
    nALT <- length(table(data$ALT))

    rtn <- reshape(
      data, timevar = "ALT", idvar = "STR",
      v.names = c("Item", "RES.B", "RES.W", "RES", itemVars),
      direction = "wide")

    rtn$RES <- eval(parse(text = paste("rtn$RES.", 1:nALT,
                                       "*", 1:nALT, 
                                       sep = "", collapse = "+")))

    if (!isTRUE(detail)) {
      deleteVars <- c(paste0("RES.B.", 1:nALT), paste0("RES.W.", 1:nALT),
                       paste0("RES.", 1:nALT), paste0("Item.", 1:nALT),
                       "BW", "STR")
      rtn <- rtn[, !c(colnames(rtn) %in% deleteVars)]
    }

  } else {  # sequential
    nALT <- length(table(data$ALT))

    rtn <- reshape(
      data, timevar = "ALT", idvar = "STR",
      v.names = c("Item", "RES.B", "RES.W", "RES", itemVars),
      direction = "wide")

    eval(parse(text = paste("rtn$availALT.", 1:nALT,
                            "<- as.integer(!is.na(rtn$RES.", 1:nALT, "))",
                            sep = "")))
    eval(parse(text = paste("rtn$RES.", 1:nALT,
                            "[is.na(rtn$RES.", 1:nALT, ")] <- 0",
                            sep = "")))
    rtn$RES <- eval(parse(text = paste("rtn$RES.", 1:nALT,
                                       "*", 1:nALT,
                                       sep = "", collapse = "+")))

    if (!isTRUE(detail)) {
      deleteVars <- c(paste0("RES.B.", 1:nALT), paste0("RES.W.", 1:nALT),
                       paste0("RES.", 1:nALT), paste0("Item.", 1:nALT),
                       "BW", "STR")
      rtn <- rtn[, !c(colnames(rtn) %in% deleteVars)]
    }

  }

  idVar <- attributes(data)$id
  apolloVars <- colnames(rtn)
  positionVars <- apolloVars %in% c(idVar, "Q", "RES")
  rtn <- rtn[, c(apolloVars[positionVars], apolloVars[!positionVars])]
  rownames(rtn) <- NULL

  return(rtn)
}

