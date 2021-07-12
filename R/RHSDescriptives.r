#  ___________________________________________________________________________
#  >> makeCountByHouseByWave <<
#  _____________________________
#' Make table showing number of residents x house x wave
#'
#'
#' @details Should be a method for the longTB.raw class, when there is one.
#'
#' @param longTB (tbl_df) A multiwave long-format (from 'makeLongNet')
#'   tibble.
#' @param includeAltID (logical) If TRUE, include individuals who appear as
#'   only altIDs in the counts (i.e. individuals who were residents at the
#'   time of survey, but who did not participate in the survey)
#' @param print2Cons (logical) If TRUE, write the output to the console.
#' @return (tbl_df) A tibble with cols HID, WID, Count, in order by HID, WID.
#' @export
makeCountByHouseByWave <- function(longTB,
                                includeAltID=FALSE,
                                print2Cons=FALSE){
#' @import dplyr

  sid.hid.wid <- longTB %>%
    select(SID, HID, WID) %>%
    arrange(SID, HID, WID)
  if(includeAltID==TRUE){
    aidonly.hid.wid <- longTB %>% select(AltID, HID, WID) %>%
      mutate(SID=AltID) %>%
      select(SID, HID, WID) %>%
      filter(!(SID %in% sid.hid.wid$SID))
    sid.hid.wid<- bind_rows(sid.hid.wid, aidonly.hid.wid)
  }

  xG <- sid.hid.wid %>%
    group_by(HID, WID) %>%
    summarize(xNum = n_distinct(SID)) %>%
    arrange(HID, WID)
  names(xG) <- c("HID","WID","Count")
  if(print2Cons==TRUE){
    knitr::kable(xG.1of3, col.names = c("HID", "WID", "Count"), row.names=F)
  }
  return(xG)
}

# _____________________________________________________________________________
# >> makeSurveyedRateTblByHouseByWave <<
# _____________________________________________________________________________
#' Make table showing rates of participation (participants / eligible) x house
#' & wave
#'
#' @param longTBElig (tbl_df) A tibble in the form created by 'makeLongNet';
#'   for correct results, this table should include all individuals considered
#'   survey-eligible for each wave, whether they took the survey or not.
#'   IMPORTANT: The function assumes that this includes individuals showing up
#'   only as alters!
#' @param longTBSurveyed (tbl_df) a tibble in the form created by 'makeLongNet'.
#'   This input should normally include only individuals who show up as egos
#'   in the data. IMPORTANT: the function assumes that alters-only in this
#'   table should NOT be counted as 'participants'.
#' @param print2Cons (logical) If TRUE, the output is printed to the console.
#'   (Note that this parameter prints only the whole rate table, not the
#'   individual elig and surveyed tables)
#' @return (tbl_df) A tibble with 5 columns: HID, WID, Elig, Surveyed, and
#'   'Rate' (ordered by WID within HID). 'Elig' shows the number of individuals
#'   per house per wave as counted by the union of SID and AltID in
#'   'longTBElig', and normally interpreted to mean, all survey-eligible
#'   individuals per house per wave. 'Surveyed' shows the number of individuals
#'   per wave as counted by SIDs only, in 'longTBSurveyed'.
#' @note It is possible to use this function to calculate rates of anything by
#'   house by wave, if you want to, simply by how you filter the two input
#'   tables, bearing in mind how this function treats SIDs and AltIDs for each
#'   input.
#' @export
makeSurveyedRateTblByHouseByWave <- function(longTBElig,
                                             longTBSurveyed,
                                             print2Cons=FALSE){
#' @import dplyr
#' @import knitr

  # Make denominator (usually "eligible" count)
  denomTB <- makeCountByHouseByWave(longTBElig,
                                    includeAltID=TRUE,
                                    print2Cons=FALSE)
  names(denomTB) <- c("HID", "WID", "Elig")

  # Make numerator (usually "surveyed" count)
  numTB <- makeCountByHouseByWave(longTBSurveyed,
                                  includeAltID=FALSE,
                                  print2Cons=FALSE)
  names(numTB) <- c("HID", "WID", "Surveyed")

  xGRates <- left_join(denomTB, numTB, by = c("HID", "WID")) %>%
    mutate(Missing = 1-(Surveyed/Elig))
  return(xGRates)
}

#  ============================================================================
#  >> nIsolates <<
#  =======================
#' output count of # of isolates in a network
#'
#'
#' @details Wraps the sna function 'isolates', which gives a list of them,
#' not a count.
#'
#' @param pNet ('network') A 'network' class object (from pkg 'network').
#' Could be binary or non-binary.
#'
#' @return (numeric) An integer count of the number of isolates (individuals
#' with no indegrees or outdegrees).
#' @export
nIsolates <- function(pNet){
  #' @import sna
  isoCount <-  length(sna::isolates(pNet))
  return(isoCount)
}

#  ============================================================================
#  >> netAPL <<
#  =======================
#' Calculate average path (geodesic) length for a network
#'
#'
#' @details Wraps the sna function 'geodist', which gives a matrix of
#' the pairwise distances, not the average. This function only includes
#' connected pairs.
#'
#' @param pNet ('network') A 'network' class object (from pkg 'network').
#' Could be binary or non-binary.
#'
#' @return (numeric) A numeric variable that averages.
#' @export
netAPL <- function(pNet){
  #' @import sna
  if (class(pNet)!="network"){
    stop("function netAPL requires a *network*-class object as input")
  }
  baseGDi <- sna::geodist(pNet,inf.replace = NA, na.omit = TRUE)
  # Mean calc here ignores unconnected or missing pairs:
  outval <- baseGDi$gdist[(baseGDi$gdist != 0)
                          & (!is.na(baseGDi$gdist))] %>% mean()
  return (outval)
}

#  ============================================================================
#  >> btwCent <<
#  =======================
#' Calculate avg betweenness centrality (Freeman) for a network
#'
#'
#' @details Wraps the sna function 'betwenness', which gives a vector of
#' the betweenness scores by node.
#'
#' @param pNet ('network') A 'network' class object (from pkg 'network').
#' Could be binary or non-binary.
#'
#' @return (numeric) A numeric variable that averages over the vector,
#' excluding any NAs.
#' @export

btwCent <- function(pNet){
  vecOfBCents <- sna::betweenness(pNet)
  avCent <- mean(vecOfBCents, na.rm = TRUE)
  return(avCent)
}

#  ============================================================================
#  >> netDiam <<
#  =======================
#' Find the longest shortest path between any 2 nodes in a network
#'
#'
#' @details Wraps the sna function 'geodist', which gives a matrix of
#' the shortest (directed) path between each pair of nodes.
#'
#' @param pNet ('network') A 'network' class object (from pkg 'network').
#' Could be binary or non-binary.
#'
#' @return (numeric) A numeric variable of the max value of this network,
#' ignoring all nodes that are not connected at all.
#' @export
netDiam <- function(pNet){
  mtxOfGeodesics <- sna::geodist(pNet, inf.replace = NA)[[2]]
  diam <- max(mtxOfGeodesics, na.rm = TRUE)
  return(diam)
}

#  ============================================================================
#  >> netDeg <<
#  =======================
#' Calculate average in + out ("Freeman") degrees across a whole network
#'
#'
#' @details Wraps the sna function 'degree', which gives a vector of
#' the total in + out-degree values by node.
#'
#' @param pNet ('network') A 'network' class object (from pkg 'network').
#' Could be binary or non-binary.
#'
#' @return (numeric) A numeric variable that averages.
#' @export
netDeg <- function(pNet){
  vecOfInOutDegs <- sna::degree(pNet)
  avDeg <- mean(vecOfInOutDegs, na.rm = TRUE)
  return(avDeg)
}




