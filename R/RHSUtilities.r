############################ RHS Utility Functions ###########################

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getNameAsString <<
#FFFFFFFFFFFFFFFFFFFFFFF
#' Returns a character string of the input variable's name
#'
#' @param pVarName A variable or some named object.
#'
#' @return A character string of the name.
#' @export
getNameAsString <- function(pVarName){
  deparse(substitute(pVarName))
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> replaceNaNWithNA <<
#______________________________________________________________________________
#' Substitutes NA for NaN in a data.table
#'
#' If input is a data.frame, it will be converted to a data.table (which is
#' also a data frame)
#'
#' @param pInTB Any tibble or data.frame
#' @return Returns the input tibble or df with NaNs replaced by NAs
#' @export
replaceNaNWithNA <- function(pInTB){
  #'@import dplyr
  if (!is.tbl(pInTB) & !is.data.frame(pInTB)){
    stop("In replaceNANWithNA, input is not a tbl or dataframe. Aborting...")
  }
  pInTB[pInTB == "NaN"]<-NA
  return(pInTB)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> DeltaNet <<
#______________________________________________________________________________
#' Given a list of networks representing time ordered observations of a
#' network, calculate aggregate change statistics from one observation to
#' the next.
#'
#' @param n A list of networks as, e.g., output by makeNetworkSet()
#' @return A table characterizing network changes.
#' @export
DeltaNet <- function(n) {
  ord <- rownames(n[[1]])
  for(i in seq(n)) {
    n[[i]] <- n[[i]][ord,ord]
    n[[i]][0] <- ''
  }
  res <- list()
  for(i in seq(n)[-1])
    res[[i-1]] <- table( paste(n[[i-1]], "->", n[[i]]) )
  res
}
