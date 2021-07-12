###################### Data Consistency Checks/Fixes ##########################

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> runConChecks <<
#______________________________________________________________________________
#' Runs consistency checks on network data
#'
#' @param rlpNet (tbl_df) A tbl in 'longNet' or 'rlpNet' format. Can include
#'   multiple waves but only one will be processed (see 'pWave' doc).
#' @param pWave (scaler, default=0) The wave to select. If default, function
#'   assumes rlpNet is for 1 wave only, & that is the wave to use. Otherwise
#'   the function will throw an error.
#'
#' @return A list of tbl_df's containing specifics needed to find
#'    and fix any inconsistencies.\cr
#'    Element 1: The SID and house ID of individuals found to be in multiple
#'       houses, along with two 0-1 variables 'ego' and 'alt', which are 1
#'       if ego has chosen alters in the house, and/or received choices in
#'       the house, respectively.\cr
#'    Element 2: The House ID, SID, and AltID of any rows in tbl_df that
#'       are duplicated\cr
#'    Element 3: Rows of the input tbl_df that are self-selections
#'       (loops)\cr
#'    In addition, console output is generated, summarizing the results
#' @export
runConChecks <- function(rlpNet, pWave=0){
#' @import dplyr

  errChecks(rlpNet, pWave)
  if(pWave>0){ #'errChecks' already checked that rlpNet is not multiwave
    # and also pWave=0.
    rlpNet <- rlpNet %>%
      filter(WID == pWave)
  } else{
    pWave = unique(rlpNet$WID)
  }

  numberOfTests <- 4 #...and they are:
  multHouses     <- checkInMultipleHouses(rlpNet, pWave) #Test1
  duplicateEdges <- checkForDuplicateEdges(rlpNet, pWave) #Test2
  selfLoops      <- checkForSelfSelection(rlpNet, pWave) #Test3
  noAlters       <- checkForNoAlters(rlpNet, pWave) #Test4

  #
  if(dim(multHouses)[1]==0 & dim(duplicateEdges)[1]==0
     & dim(selfLoops)[1]==0 & dim(noAlters)[1]==0){
    cat("w", pWave, ": No inconsistencies detected; ",
        "return empty list\n", sep="")
    listOut <- list()
  } else {
    if(dim(multHouses)[1] > 0) {
      cat("w",pWave, ": Multiple house membership detected;",
          "see output list[[1]]\n", sep="")
      print(multHouses)
    }
    if(dim(duplicateEdges)[1] > 0){
      cat("w", pWave, ": Duplicate edges detected; ",
          "see output list[[2]]\n", sep="")
      print(duplicateEdges)
    }
    if(dim(selfLoops)[1] > 0){
      cat("w", pWave, ": Self-loops detected; ",
          "see output list[[3]]\n", sep="")
      print(selfLoops)
    }
    if(dim(noAlters)[1] > 0){
      cat("w", pWave, ": Egos chosen by no alters detected; ",
          "see output list[[4]]\n", sep="")
    }
    listOut <- vector('list', numberOfTests)
    listOut[[1]] <- multHouses
    listOut[[2]] <- duplicateEdges
    listOut[[3]] <- selfLoops
    listOut[[4]] <- noAlters
    names(listOut) <- c("MultipleHouses","DuplicateEdges","SelfLoops",
                        "NoAlters")
  }
  return(listOut)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> checkForDuplicateEdges <<
#______________________________________________________________________________
#' Checks long-format data (output from 'makeLongNet') for duplicate edges
#'
#' @param rlpNet (tbl_df) Long-format edge list of the form 'longNet' or
#'   'rlpNet.
#' @param pWave (scaler, default=0) The wave to select. If default, function
#'   assumes rlpNet is for 1 wave only, & that is the wave to use. Otherwise
#'   the function will throw an error.
#'
#' @return Outputs a tbl_df showing duplicated rows, in the same format
#'    as 'rlpNet'
#' @export
checkForDuplicateEdges <- function(rlpNet, pWave=0){
#' @import dplyr
  errChecks(rlpNet, pWave=pWave)
  if(pWave>0){ #'errChecks' already checked that rlpNet is not multiwave
    # and also pWave=0.
    rlpNet <- rlpNet %>%
      filter(WID == pWave)
  } else{
    pWave = unique(rlpNet$WID)
  }
  pDupes <- rlpNet %>%
    select (SID, HID, AltID) %>%
    group_by(SID, HID, AltID) %>%
    summarise(n=n()) %>%
    filter (n>1)
  return(pDupes)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> checkForSelfSelection <<
#______________________________________________________________________________
#' Checks long-format data (from 'makeLongNet') for self-choices (loops)
#'
#' Self-choices are not allowed. RSiena ignores them, but they make graphs
#'    weird, and make the addition of structural zeros more difficult.
#'
#' @param rlpNet (tbl_df)Input long-format, i.e. 'longNet' or 'rlpNet'
#'   format.
#'
#' @param pWave (scaler, default=0) The wave to select. If default, function
#'   assumes rlpNet is for 1 wave only, & that is the wave to use. Otherwise
#'   the function will throw an error.
#'
#' @return Outputs a tbl showing self-choices. Normally these should
#'    be deleted.
#' @export
checkForSelfSelection <- function(rlpNet, pWave=0){
  #' @import dplyr
  errChecks(rlpNet, pWave=pWave)
  dupes <- rlpNet %>%
    filter(SID == AltID)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> checkInMultipleHouses <<
#___________________
#' Check possible multiple house membership within 1 wave.
#'
#' @param rlpNet (tbl_df) A tbl in 'longNet' or 'rlpNet' format. Can include
#'   multiple waves but only one will be processed (see 'pWave' doc).
#' @param pWave (scaler, default=0) The wave to select. If default, function
#'   assumes rlpNet is for 1 wave only, & that is the wave to use. Otherwise
#'   the function will throw an error.
#'
#' @details SIDs can be assigned to multiple houses in a given wave either
#'     because of an incorrect manual input to the database, or (more
#'     typically) because the SID actually was a member of multiple
#'     houses during the wave survey period, and ended up being surveyed
#'     in several houses (theoretically this shoudld not happen--RAs should
#'     check first if they've been surveyed since the wave began), or
#'     because they were on two or more house rosters (this should not
#'     happen either, actually; RAs should first that the current
#'     roster is accurate before each survey).
#'
#' @return A tbl_df of all unique SID-HID combinations where the SID
#'     appeared in more than one house as chooser, chosen, or both. The
#'     columns 'ego' and 'alt' are 1 or 0, respectively, depending on
#'     whether SID was a chooser (ego) and/or chosen (alt) in that
#'     particular house.
#' @export
checkInMultipleHouses <- function(rlpNet, pWave=0){
  #' @import dplyr
  # Input parameter errors.
  errChecks(rlpNet, pWave=pWave) #Common errors
  if(pWave>0){ #'errChecks' already checked that rlpNet is not multiwave
               # and also pWave=0.
    rlpNet <- rlpNet %>%
      filter(WID == pWave)
  } else{
    pWave = unique(rlpNet$WID)
  }

  # Create DT's of HIDs in which each SID was surveyed (made choices),
  #    or was chosen (and may or may not have also been surveyed).
  # Choosers (egos)
  df1 <- rlpNet %>%
    select(SID,HID) %>%
    distinct() %>%
    arrange(SID, HID) %>%
    mutate(ego=1)
  # Chosen (alters)
  df2 <- rlpNet %>%
    select(SID=AltID, HID) %>%
    distinct() %>%
    arrange(SID, HID) %>%
    mutate(alt=1)
  # Outer Join on SID-HID, using egos from df1, alters from df2
  dfb <- full_join(df1, df2, by=c("SID", "HID")) %>%
    arrange(SID, HID)
  dfb[is.na(dfb)] <- 0

  # Output a table reporting those in more than 1 house
  dupes <- aggregate(data.frame(count = dfb$SID),
                     list(value = dfb$SID), length)
  names(dupes) <- c("SID", "NHouses")
  outDT <- dfb[dfb$SID %in% dupes[dupes$NHouses > 1,]$SID,]
  return(outDT)
}

# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> checkForNoAlters <<
#______________________________________________________________________________
#' Check individuals who were only egos in a house (never alters).
#'
#' @details Because house residents are asked to rate all other residents on
#'    study relationship criteria, there should never be a case where someone
#'    did a survey, but was not named as an alter by anyone. This check also
#'    covers the situation where only 1 individual was surveyed in a house
#'    (in that instance, there will be just 1 ego, though possibly
#'    multiple alters)
#'
#' @param rlpNet (tbl_df) A tbl in 'longNet' or 'rlpNet' format. Can include
#'   multiple waves but only one will be processed (see 'pWave' doc).
#' @param pWave (scaler, default=0) The wave to select. If default, function
#'   assumes rlpNet is for 1 wave only, & that is the wave to use. Otherwise
#'   the function will throw an error.
#' @return A tbl_df containing SID, HID, and WID for any individuals &
#'    waves where the individual did a survey, but never appears as an
#'    alter (AltID) in the input table.
#' @note Only within a wave does it make sense to assume that all egos should
#'    be chosen by at least some (same-house) alters.
#' @export
checkForNoAlters <- function(rlpNet, pWave=0){
#' @import dplyr
#'
  errChecks(rlpNet, pWave)
  if(pWave>0){ #'errChecks' already checked that rlpNet is not multiwave
    # and also pWave=0.
    rlpNet <- rlpNet %>%
      filter(WID == pWave)
  } else{
    pWave = unique(rlpNet$WID)
  }

  allSIDs <- as.vector(unique(rlpNet$SID))
  allAltIDs <- as.vector(unique(rlpNet$AltID))
  missingAllAlters <- allSIDs[!(allSIDs %in% allAltIDs)]

  if (length(missingAllAlters) == 0){
    outTB <- tibble(SID = integer(), HID = integer(), WID = integer())
  } else {
    outTB <- rlpNet %>%
      select(SID, HID, WID) %>%
      filter(SID %in% missingAllAlters) %>%
      distinct()
  }
  return(outTB)
}

# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> checkForHousesOf1 <<
# _____________________________________________________________________________
#' Check for houses with just 1 network member
#'
#' @details In such cases there is no 'network', so they should be excluded
#'    from any network analysis. Usually this should not arise but it can
#'    if individuals were excluded from a supposed-final analysis set because,
#'    e.g., they did not have enough observations (say, 2 or more across all
#'    waves) to be included.
#' @param rlpNet a tbl_df in 'longNet' or 'rlpNet format.
#'    This table can contain any number of waves, but if there is more than
#'    one wave, the check will be done within wave, i.e. so the user
#'    can be sure that no single wave has a house with just one member surveyed.
#' @return A tbl_df containing WID, and HID for any individuals &
#'    waves where the individual was the only one in his/her house.
#' @note This check is important to run before taking any network set into
#'    production analyses, because other data corrections can create houses
#'    of size 1 as a 'side effect' that may not otherwise be obvious.
#' @export
checkForHousesOf1 <- function(rlpNet){
#' @import dplyr
#'
  if (length(class(rlpNet)) == 3){
    if (class(rlpNet)[1] != "tbl_df"){
      stop(paste("\nFunction ", callFunction,
                 "\n: the input network object must be a tibble",
                 sep=""))
    }
  } else {
    if (class(rlpNet) == "data.frame"){
      cat("Adding class tbl_df to data.frame")
      rlpNet <- as.tbl(rlpNet)
    }
  }
  if(!all(names(rlpNet)[1:4]==c("SID", "HID", "WID", "AltID"))){
    stop(paste("\nFunction ", callFunction,
               "\n requires SID HID WID AltID as 1st 4 input cols.",
               sep=""))
  }
  if(!names(rlpNet)[5] %in% c("friend","loan","help","conv","advc","rate" )){
    stop(paste("\nFunction ", callFunction,
               "\n requires friend, loan, help, conv, advc, or rate",
               "\n as 5th col name.",
               sep=""))
  }

  grpByHouse <- rlpNet %>%
    group_by(WID, HID) %>%
    summarize(xNum = n_distinct(SID)) %>%
    arrange(WID, HID) %>%
    filter(xNum==1) %>%
    select(WID, HID)
  return(grpByHouse)
}


# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> fixHouse <<    >>>>>>>> S T A R T   H E R E <<<<<<<<<<
# _____________________________________________________________________________
#' Remove selected network connections in particular houses
#'
#' There are instances where a study participant may have made relationship
#' choices (or received them) in multiple houses during one wave period, or
#' may have completed a survey in a house after all or most others in the
#' house did so, when the index individual was not listed on the house roster
#' and could not thus be rated by others. In the case of multiple houses, we
#' must decide which house to keep them in (usually the first
#' one they were surveyed in that wave), and eliminate any in or out-choices
#' in any other houses. In the case of incomplete responses from other house
#' members (e.g. because ego was not yet in the house when they all surveyed),
#' normally we eliminate this individual's data for analysis purposes.
#'
#' @param rlpNet (tbl_df) A tbl in 'longNet' or 'rlpNet' format. Can include
#'   multiple waves but only one will be processed (see 'pWave' doc).
#' @param pWave (scaler, default=0) The wave to select. If default, function
#'   assumes rlpNet is for 1 wave only, & that is the wave to use. Otherwise
#'   the function will throw an error.
#' @param pSID.HID (tbl_df) Each row represents an individual
#'     (SID) and house (HID) for which any relationships involving that
#'     individual and house should be deleted.Each row consists of two
#'     columns, 'SID' and 'HID'. These names are assigned, regardless of
#'     what they're called in this data table/frame, so it makes sense to
#'     just call them that from the get-go.
#'
#' @return A tbl_df in the same format as the input data table. This table\
#'   will have have been purged of any network data observations with
#'   SIDs and HIDs that match a row in 'pSID.HID'.
#' @export
fixHouse <- function(rlpNet, pWave, pSID.HID){  #>>> START HERE <<<
  #' @import dplyr
  # _________________
  # Check input
  errChecks(rlpNet, pWave)
  if(pWave < 1 | pWave > 8) stop("fixHouse: wave number must be 1-8")
  if(!c("SID") %in% names(pSID.HID) | !c("HID") %in% names(pSID.HID)){
    stop("fixHouse: table of deletes must have cols SID and HID")
  }

  # Add a flag 'flg' to the input DT
  rlpNet <- rlpNet %>%
    mutate(flg=0)

  # Apply down the rows of pSID.HID
  for (i in 1:dim(pSID.HID)[1]){
    iSID <- pSID.HID[i,]$SID # Short names for readability below...
    iHID <- pSID.HID[i,]$HID
    rlpNet <- rlpNet %>%
      mutate(flg = ifelse((SID == iSID | AltID == iSID) & HID==iHID, 1, flg))
  }
  dtRemoved <- rlpNet %>%
    filter (flg == 1)
  cat("Removing", dim(dtRemoved)[1], "relationships from network...", "\n")
  print(dtRemoved %>% select(SID, HID, AltID)) # Display them
  # Remove all rows where flg = 1
  dtOut <- rlpNet %>%
    filter(flg == 0) %>%
    select(-flg)

  return(dtOut)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >>errChecks <<
#FFFFFFFFFFFFFFFF
#' Internal function to check common errors for these functions
#'
#' @param rlpNet (tbl_df) A network edge list in the form of rlpNet,
#'   or longNet. May be multiwave, but only 1 wave is ever used.
#' @param pWave (scaler, default=0) The wave to select. If default, function
#'   assumes rlpNet is for 1 wave only, & that is the wave to use. Otherwise
#'   the function will throw an error.
#  >> not exported <<
errChecks <- function(rlpNet, pWave){
#'@import dplyr
#'
  callFunction <- deparse(sys.call(-1)) #gets calling function

  if(!"tbl_df" %in% class(rlpNet)){
    stop(paste("\nFunction: ", callFunction,
               "\n The input network object must be class tbl_df",
                 sep=""))
    }
  if(!all(names(rlpNet)[1:4]==c("SID", "HID", "WID", "AltID"))){
    stop(paste("\nFunction: ", callFunction,
               "\n INput requires SID HID WID AltID as 1st 4 input cols.",
               sep=""))
  }
  if(!names(rlpNet)[5] %in% c("friend","loan","help","conv","advc","rate" )){
    stop(paste("\nFunction: ", callFunction,
               "\n Input requires friend, loan, help, conv, advc, or rate",
               "\n as 5th col name.",
               sep=""))
  }
  if (length(pWave)>1){
    stop(paste("\nFunction: ", callFunction,
               "\n Only 1 value of pWave allowed.",
               sep=""))
  }
  if(pWave==0 & length(unique(rlpNet$WID))>1){
    stop(paste("\nFunction: ", callFunction,
               "\npWave must be >0 if input table includes >1 wave.",
               sep=""))
  }
}

