############################## SAOM OBJECT CREATION ###########################


#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> datToSiena <<
# ___________________
#' Generate a "siena" data object from a set of properly formed data frames
#' ("cx" and "vx") and a list of adjacency matrices ordered by wave.
#'
#'
#'
#' @param cx Data frame of constant covariates with id field == pidf.
#' @param vx List of data frames, one for each wave, containing identically
#'   named columns representing time-specific variables (including
#'   dependent behavioral variables).
#' @param net Named list of lists of sparse matrices of type "dgTMatrix".
#'   The names given to each list are important and will be used as reference.
#'   If the list is unnamed, it is assumed to be a list of waves of one network.
#' @param pidf Field name common to cx and vx data frames with participant
#'   identifiers. Default is "SID"
#' @param pid Vector of IDs to retain. If NA, then retain all.
#' @param depvar String vector of time varying variables that will be used
#'   as dependent behavioral variables.
#' @param concov String vector of constant covariate field names to retain.
#'   If NA, then retain all.
#' @param varcov Vector of time varying covariate field names to retain. If NA,
#'   then retain all.
#' @param RSiena Should the output be formed into a data object that RSiena can
#'   work with? Otherwise, subsets are applied and data frames are returned
#' @param cextra Fields in cx to be removed from the constant covariates, but
#'          still returned for later use (e.g., house id)
#'
#' @details An ideal use of this function is in the context of RSiena data
#' preparation. At the least, the user supplies a data frame ("cx") containing
#' all constant actor covariates; a list of data frames ("vx") containing
#' wave-specific time-varying actor covariates--one data frame per wave with
#' identical column names; a string ("pidf") referring to the name of the
#' participant id field common to all data frames (normally "SID". Optionally,
#' the user may supply a vector of participant ids ("pid") for subsetting all
#' output data; a string vector of field names ("concov" and "varcov") that
#' identify fields of interest from the input data frames (NA implies all are
#' of interest); and a logical ("RSiena") indicating whether the result should
#' be returned as a "siena" data object.
#'
#' @return If RSiena==TRUE, a list with three elements: \cr
#'   RSienaObj: a list of 'coCovar' and 'varCovar' objects amenable to coercion
#'   to a 'siena' object. \cr
#'   pid: a vector of pid values that were used to subset and order the output
#'   data \cr
#'   extra: a data frame of variables identified with the 'cextra' parameter
#'   that were not included in the RSienaObj, but may yet be needed
#'   subsetted and ordered as objects in the RSienaObj \cr
#'
#' @author Nathan J. Doogan \email{doogan.1@@osu.edu}
#' @export
datToSiena <- function(cx=NULL, vx=NULL, net=NULL, pidf="SID", pid=NULL,
                       depvar=NULL, concov=NULL, varcov=NULL, RSiena=T,
                       nodeSet='Actors', hid=NULL, vDyadCov=NULL, compChange=NULL,
                       allowOnlyBeh=T, allowOnlyNet=T) {
  ## pid and net must be supplied (hint: get from the output of makeSparse)
  if(is.null(pid)) stop("Name of subject ID column must be supplied")
  if(is.null(net)) stop("Network input list must be supplied")

  ## first get cx and concov in shape
  if(!is.null(cx)) {
    # so long as it looks right, assign rownames, delete pidf cols, and order
    if(is.data.frame(cx)) {
      if(!pidf %in% names(cx))
        stop(paste(pidf,"is not a column of cx"))
      rownames(cx) <- cx[,pidf]
      cx[,pidf] <- NULL
      cx <- cx[pid,,drop=F]
      # if concov was not supplied, it is all cx columns (minus pidf)
      if(is.null(concov))
        concov <- names(cx)
    }else{
      stop("cx is not a data frame")
    }
  }else{
    # in case concov was supplied, but cx was NULL
    concov <- NULL
  }

  ## then get vx and varcov in order
  if(!is.null(vx)) {
    # so long as it looks right, assign rownames, delete pidf cols, and order
    if(is.list(vx) & !is.data.frame(vx) & all(sapply(vx,is.data.frame))) {
      for(i in seq(vx)) {
        rownames(vx[[i]]) <- vx[[i]][,pidf]
        vx[[i]][,pidf] <- NULL
        vx[[i]] <- vx[[i]][pid,,drop=F]
      }
      # check that all col names found in vx[[1]] are present in vx[[*]]
      tmp <- sapply(vx, function(x) length(setdiff(names(vx[[1]]), names(x))))
      if( !all(tmp) == 0 )
        stop("vx[[1]] contains columns that are not found in at least one other
             data frame in the vx list")
      # if varcov is NA, then fill in with non-depvar field names
      if(is.null(varcov))
        varcov <- setdiff(names(vx[[1]]), depvar)
    }
  }else{
    # in case varcov was supplied, but vx was NULL
    varcov <- NULL
  }

  ## get networks in order (nets are assumed to be supplied)
  ## net should be a list with one element per relation
  ## each relation should be a list, one element per wave
  for(i in names(net)) {
    net[[i]] <- CompMod(net[[i]], hid, test=NA, value=10)
    net[[i]] <- CompMod(net[[i]], hid, test=-1, value=NA)
    net[[i]] <- addStrZeros.1(net[[i]], hid)
    net[[i]] <- addStrZeros.2(net[[i]], hid)
    net[[i]] <- array(
      unlist(lapply(net[[i]], function(x) as.matrix(x[pid,pid]))),
      dim=c(length(pid),length(pid),length(net[[i]])))
    dimnames(net[[i]]) <- list(pid,pid,NULL)
  }

  ## get varying dyadic covariates in order
  for(i in names(vDyadCov)[1:(length(vDyadCov)-1)]) {
    vDyadCov[[i]] <- array(
      unlist(lapply(vDyadCov[[i]], function(x) as.matrix(x[pid,pid]))),
      dim=c(length(pid),length(pid),length(vDyadCov[[i]])-1))
    dimnames(vDyadCov[[i]]) <- list(pid,pid,NULL)
  }

  ## If RSiena == FALSE, return cx, vx, and net as is
  # The value of this is that they are subsetted and ordered
  if(!RSiena)
    return( list(net=net, constant=cx, varying=vx) )

  ## if RSiena == TRUE, then form result as RSiena requires
  require(RSiena)
  r <- list()
  for(i in concov)
    r[[i]] <- coCovar(as.numeric(cx[,i]), nodeSet=nodeSet)
  for(i in varcov)
    r[[i]] <- varCovar(sapply(vx, function(x) x[,i]), nodeSet=nodeSet)
  for(i in names(vDyadCov))
    r[[i]] <- varDyadCovar(vDyadCov[[i]], nodeSets=nodeSet, sparse=F)
  for(i in depvar)
    r[[i]] <- sienaDependent(sapply(vx, function(x) x[,i]), nodeSet=nodeSet, allowOnly=allowOnlyBeh)
  for(i in names(net))
    r[[i]] <- sienaNet(net[[i]], nodeSet=nodeSet, allowOnly=allowOnlyNet)
  if(!is.null(compChange))
    r[['compChange']] <- compChange

  # return
  list(RSienaObj=do.call(sienaDataCreate,r), pid=pid)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> multiGroup <<
# ___________________
#' Given a list of ID vectors and other arguments accepted by datToSiena(),
#' this function returns a sienaGroup object with groups defined by the
#' list of ID vectors supplied as the first argument.
#'
#' @author Nathan J. Doogan \email{doogan.1@@osu.edu}
#' @export
multiGroup <- function(idlist=NA, ...) {
  if(length(idlist) == 1 && is.na(idlist))
    stop("idlist must be supplied")
  res <- lapply(idlist, function(i) {
    datToSiena(..., pid=i)$RSienaObj
  })
  sienaGroupCreate(res)
}


#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> addStrZeros.2 <<
#______________________________________________________________________________
#' Insert structural zeros to prevent ties between houses.
#'
#' @param pNetList A list of social network matrices (typically for a set of
#' waves).
#'   * Class may be
#'     +  'matrix' (base R 2-dim array)
#'     +  'Matrix' (any of the s4 classes supported by pkg 'Matrix')
#'   * Rows and columns must be named by node identifier (usually SID)
#' @param pSIDHIDList A list of dataframes (or objects that can act like them,
#'   such as data.tables), corresponding to the networks in pNetList. Each is
#'   a lookup table that codes which nodes (SIDs) belong in which houses
#'   (HIDs) so each has two columns (SID and HID), which index this
#'   correspondence.
#' @return Returns the list of network matrices in the same form they were
#'   input, but with structural zeros (10's) encoded for linkages between
#'   individuals in different houses.
#' @export
addStrZeros.2 <- function(pNetList, pSIDHIDList) {
  pSIDHIDList <- lastKnownHouse(pSIDHIDList)
  # Creates w named vectors; the elements are HID, the names are SID
  hLookup <- lapply(2:(length(pNetList)+1),
                    function(x) setNames(pSIDHIDList[,x], pSIDHIDList[,1]))
  # subset and order the house lookup vectors according to network
  # node identifiers (the row names of the networks)
  for(i in seq(hLookup)) {
    hLookup[[i]] <- hLookup[[i]][rownames(pNetList[[i]])]
  }
  # Generate struc zero locator matrices (logical matrices of
  # same dimension as the network matrices)
  str0 <- lapply(hLookup, function(x) outer(x,x,'!='))
  # Apply them to the data
  for(i in seq(pNetList)) {
    pNetList[[i]][str0[[i]]] <- 10
  }
  return(pNetList)
}



#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> addStrZeros.1 <<
#______________________________________________________________________________
#' Insert structural zeros to prevent ties between houses when two nodes
#' have never lived in the same house at the same time.
#'
#' @param pNetList A list of social network matrices (typically for a set of
#' waves).
#'   * Class may be
#'     +  'matrix' (base R 2-dim array)
#'     +  'Matrix' (any of the s4 classes supported by pkg 'Matrix')
#'   * Rows and columns must be named by node identifier (usually SID)
#' @param pSIDHIDList A data.frame with ncol() == the number of waves + 1.
#' The first col is the participant ID, and the remaining cols are house
#' identifiers representing where the participant lived at each observation.
#' @return Returns the list of network matrices in the same form they were
#'   input, but with structural zeros (10's) encoded for linkages between
#'   individuals in different houses.
#' @export
addStrZeros.1 <- function(pNetList, pSIDHIDList) {
  pSIDHIDList <- lastKnownHouse(pSIDHIDList)
  pSIDHIDList <- as.data.frame(pSIDHIDList)
  rownames(pSIDHIDList) <- pSIDHIDList[,1]
  pSIDHIDList <- pSIDHIDList[rownames(pNetList[[1]]),]
  # if two participants were ever in the same house, do NOT place
  # a structural zero between them. Ideally, another part of the
  # setup process will place an NA between them to indicate
  # they are no longer together in the house, but once were.
  str0 <- apply(pSIDHIDList[,2:(length(pNetList)+1)], 1, function(x) {
    apply(pSIDHIDList[,2:(length(pNetList)+1)], 1, function(y) !any(x==y, na.rm=T))
  })
  for(i in seq(pNetList)) pNetList[[i]][str0] <- 10
  pNetList
}



#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeCCVec <<
# ____________________________________________________________________________
#' Returns a list of vectors representing RSiena composition change.
#'
#' The output of this function may be input directly to the RSiena function
#'     'sienaCompositionChange'.
#'
#' @param pElig  An n x 1+w tbl_df, with 1 row for every SID to be included
#'     in a particular analysis (an "Analysis Set"), and 1 col for
#'     each wave w, containing a number >0 if the SID is in the analysis for
#'     that wave, and NA if not if not (col 1 is SID). In this project, this
#'     dataframe (or data.table) is called 'SIDInAnyWave', or a similar
#'     construction, with SID as the first column, and the house id the SID
#'     belonged to in each of the w waves in the analysis, named 'HID1',
#'     'HID2', ....'HIDw'.
#' @return A list of n elements, one for each row of the input DF. Each
#'     element is a vector of pairs of arrival and departure times from
#'     the network, as explained in the RSiena manual under 'method of
#'     changing composition'.
#' @examples
#' # Returns a list of composition change vectors for the SIDs
#' # in waves 1, 3, and 4, and schools 3, 4, 5, 6, and 30.
#' ccDF <- makeCCVec(SIDInAnyWave)
#'
#' @export
makeCCVec <- function(pElig){
  #' @import dplyr

  if(!"tbl_df" %in% class(pElig)){
    stop(paste("\nFunction: ", callFunction,
               "\n The input network object must be class tbl_df",
               sep=""))
  }

  wv<-dim(pElig)[2] # cols (waves-1)
  nn<-dim(pElig)[1] # rows (nodes)

  # Recode input rows into 0's (NA) and 1's (not NA)
  pElig[,2:wv] <- lapply(pElig[,2:wv], function(x)
    ifelse(is.na(x), 0, 1))

  pElig <- as.matrix(pElig)

  CCVec <- vector("list", nn) # empty list with nn elements
  #
  for (i in 1:nn){
    for (j in 2:wv) {
      # -- Wave is 1st wave
      if (j==2 & pElig[i,j]==1) {CCVec[[i]]<-c(1)}
      # -- any other wave than 1st or last
      if (j>2 & j<wv) {
        # switch from 1 to 0
        if(pElig[i,j]==0 & pElig[i,(j-1)]==1){
          CCVec[[i]]<-c(CCVec[[i]],c(j-1.5))}
        if(pElig[i,j]==1 & pElig[i,(j-1)]==0){
          CCVec[[i]]<-c(CCVec[[i]],c(j-1.5))}
      } # if j>2,<wv
      # -- if last wave
      if (j==wv) {
        if(pElig[i,wv]==0 & pElig[i,(wv-1)]==1){
          CCVec[[i]]<-c(CCVec[[i]],wv-1.5)}
        if(pElig[i,wv]==1 & pElig[i,(wv-1)]==0){
          CCVec[[i]]<-c(CCVec[[i]],c(wv-1.5,wv-1))}
        if(pElig[i,wv]==1 & pElig[i,(wv-1)]==1){
          CCVec[[i]]<-c(CCVec[[i]],c(wv-1))}
      } # if j==wv
    } # j loop
  } # i loop

  setNames(CCVec,pElig[,1])
}


#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> TriadCensus <<
# ____________________________________________________________________________
#' RSiena goodness-of-fit statistic function for the 16-dimensional triad
#'   census. Few details are supplied because this function is not intended
#'   to be used directly.
#'
#' @export
TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
  unloadNamespace("igraph") # to avoid package clashes
  require(network)
  require(sna)
  x <- networkExtraction(i, data, sims, wave, groupName, varName)
  if (network.edgecount(x) <= 0){x <- symmetrize(x)}
  # because else triad.census(x) will lead to an error
  tc <- sna::triad.census(x)[1,levls]
  # names are transferred automatically
  tc
}


#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> QOLMixing <<
# ____________________________________________________________________________
#' RSiena goodness-of-fit statistic function for a 3x3 mixing matrix of QOL
#'   Few details are supplied because this function is not intended
#'   to be used directly. Moreover, this is a specialized function for a
#'   specific project and a specific covariate.
#'
#' @export
QOLMixing <- function(i, data, sims, wave, groupName, varName, levls=1:9){
  require(network)
  require(sna)
  g <- networkExtraction(i, data, sims, wave, groupName, varName)
  x <- as.numeric(as.factor(data$Data1$vCovars$QOL[,wave]))
  u <- unique(x)
  u <- u[!is.na(u)]
  crs <- expand.grid(u, u)
  apply(crs, 1, function(s) sum(g[x==s[1],x==s[2]], na.rm=T))
}


#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> CompMod <<
# ____________________________________________________________________________
#' Returns a list of {M,m}atrices with the same dimensions as the input 'n'
#'     except with entire rows set to NA for those rows with names matching
#'     the names of rows in the input 'x', which identifies time points when
#'     an actor was missing (NA) from the network data.
#'
#'
#' @param n A list of matrices (representing networks across time). Matrices
#'     should be identical in dimensionality and should include row and column
#'     names that match with the identifiers in the input x.
#'
#' @param x A data frame with k + 1 columns, where k == length(n), and the
#'     first column is an identifier column. Columns 2:ncol(x) must
#'     correspond with matrices 1:length(n). This is usually "SIDInAnyWave"
#'
#' @param value The value to plug in (e.g., NA, or 10)
#'
#' @return A list of n matrices with the exact structure of the input 'n',
#'     in which rows corresponding with missing participants, as captured
#'     in the input 'x', set to all NA values.
#'
#' @examples
#'
#' @export
CompMod <- function(n, x, test=NA, value=NA) {
  x <- as.data.frame(x)
  rownames(x) <- x[,1]
  for(j in 2:(length(n)+1)) {
    for(i in rownames(n[[j-1]])) {
      if(is.na(test)) {
        if(is.na(x[i,j])) {
          n[[j-1]][i,] <- value
          n[[j-1]][,i] <- value
        }
      }else{
        if(!is.na(x[i,j]) && x[i,j] == test) {
          n[[j-1]][i,] <- value
          n[[j-1]][,i] <- value
        }
      }
    }
  }
  n
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> CompLast <<
# ____________________________________________________________________________
#' Returns a list of {M,m}atrices with the same dimensions as the input 'n'.
#'     For all row/col indices that are identified in 'x' as missing for a given
#'     time point, the entire row/col matching the index is set to the last
#'     known value.
#'
#'
#' @param n A list of matrices (representing networks across time). Matrices
#'     should be identical in dimensionality and should include row and column
#'     names that match with the identifiers in the input x.
#'
#' @param x A data frame with k + 1 columns, where k == length(n), and the
#'     first column is an identifier column. Columns 2:ncol(x) must
#'     correspond with matrices 1:length(n).
#'
#' @return A list of n matrices with the exact structure of the input 'n',
#'     in which rows corresponding with missing participants, as captured
#'     in the input 'x', set to all NA values.
#'
#' @examples
#'
#' @export
CompLast <- function(x, n) {
  rownames(x) <- x[,1]
  for(j in 2:(ncol(x))) {
    for(i in rownames(n[[j-1]])) {
      if(is.na(x[i,j])) {
        if(j==2) {
          n[[j-1]][i,] <- 0
        }else{
          n[[j-1]][i,] <- n[[j-2]][i,]
          n[[j-1]][,i] <- n[[j-2]][,i]
        }
      }
    }
  }
  n
}

# function to carry forward last seen house number for -1 values in SIAW
lastKnownHouse <- function(d) {
  for(i in 1:nrow(d)) {
    for(j in 2:ncol(d)) {
      if(!is.na(d[i,j]) && d[i,j] == -1) {
        d[i,j] <- d[i,j-1]
      }
    }
  }
  d
}
