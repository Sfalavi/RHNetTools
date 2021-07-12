################ Network Creation Functions ######################

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeListOfNetsByHouse
#___________________
#' Makes a list of house-specific networks, for a single wave only.
#'
#' The main purpose of this arrangement of the network data is to facilitate
#' house by house descriptive statistics.
#'
#' @param pInTB A tibble of the network data in 'long' format, i.e., output
#'     from the function 'makeLongNet'. Could be multiwave, but only one
#'     wave will be used, based on 'pWav'.
#' @param pWav A (single) wave ID to select for.
#' @param pTypNet (character; default = "FR") A code for the relationship type
#'   the network should represent.
#'   Possibilities are:
#'     FR: friend (1=close friend,2=friend, 3=acquaintence, 4=stranger,
#'       5=adversary)
#'     LO: loan $ (1=$0, 2=$10, 3=$50, 4=$100, 5=$500)
#'     HE: help   (1=very likely, 2=likely, 3=maybe, 4=prob not, 5=no)
#'     CO: conversation (1=daily, 2=almost daily, 3=every few days, 4=weekly,
#'                 5=almost never)
#'     AD: advice from (1=very often, 2=quite often, 3=regularly, 4=rarely,
#'                  5=never)
#'     ST: strength (1=very strong, 2=strong, 3=weak, 4=none, 5=negative)
#' @param pTypOut (character; Default "MX") A code indicating format for
#'   networks in the output list.
#'     "NT" => class 'network' (pkg network)
#'     "MX" => class 'matrix' (pkg 'Matrix').
#' @param pTHold A number from 1 to 5, or -1 to -5.
#'    If positive, the number means 'this value or higher = 1; else 0'.
#'    If negative, the number means 'this value or lower = 0; else 0'.
#'    Default is 0, which will not do any recoding.
#' @param includeAltID (logical; default FALSE). If only survey participants
#'    (choosers as well as chosen) are to be included, this parameter should
#'    be FALSE. If relationship ratings given to non participants (Alters
#'    only) are also to be included, set to TRUE. Note that if any house
#'    network includes non-participant alters, this parameter MUST be
#'    TRUE, or an error is thrown.
#' @return If pTypOut = A list of tibbles, one for each house in the same format as
#'     pInTB.
#'
#' @export
makeListOfNetsByHouse <- function(pInTB, pWav, pTypNet = "FR", pTypOut = "MX",
                                  pTHold = 0, includeAltID = FALSE){
  #' @import dplyr
  #' @import network
  #' @import Matrix
  # ___________________
  if (!"tbl_df" %in% class(pInTB)){
      stop("Network object must be a tbl_df")
  }
  if (!all(names(pInTB) %in% c("SID","HID","WID","AltID","friend","loan",
                       "help","conv","advc","rate"))){
    stop(paste("Input network object has 1 or more incorrect variable.",
               "\nMust be a subset of vars from makeLongNet.", sep=""))
  }
  if(length(pWav)>1){
    stop("\nmakeListOfNets only makes house specific networks for 1 wave.")
  }
  if (!pTypNet %in% c("FR", "LO", "HE", "CO", "AD", "ST")){
    stop("Output type (pTypNet) must be NT, SP, or MT")
  }
  if (!pTypOut %in% c("NT", "MX")){
    stop("Output type (pTypNet) must be NT or MX")
  }

  # Select relationship type
  rlpName <- assignRlpName(pTypNet)
  rlpNet  <- getNet4OneRlp (pInTB,
                            pWavVec = pWav,
                            rlpName)

  # Recode the relationship based on pTHold, if it's not zero.
  if(pTHold !=0){
    rlpNet[[5]] <- recodeRlpVar(rlpNet[[5]], pTHold)
  }

  SIDHID <- makeSIDHIDLookups(list(rlpNet), includeAltID = FALSE)
  # make a unique vector of HIDs
  HIDVec <- sort(unique(rlpNet$HID))
  # Pre-allocate output
  outLS <- vector(mode = "list", length = length(HIDVec))

  # Iterate through house vector, fill list networks, 1 for each house
  for (i in 1:length(HIDVec)){
    outLS[[i]] <- rlpNet %>%
      filter(HID == HIDVec[i]) %>%
      arrange(SID)
    names(outLS)[i] <- toString(HIDVec[i])
    if(includeAltID==FALSE){
      # Find anyone who is only an AltID, and delete that row
      altID <- outLS[[i]]$AltID
      altIDOnly <- altID[which(!altID %in% outLS[[i]]$SID)]
      outLS[[i]] <- outLS[[i]] %>%
        filter(!AltID %in% altIDOnly)
    }
    outLS[[i]] <- makeAdjMatrix(outLS[[i]], pWave = pWav,
                                includeAltID = includeAltID)
    if(pTypOut=="NT"){
    outLS[[i]] <- as.network(outLS[[i]])
    }
  }
  return(outLS)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeNetworkSet <<
# ___________________
#' Make an 'Analysis Set' list of 'network' or matrix -class networks.
#'
#' Takes 'long' format network data (from 'makeLongNet') in a
#'     class 'tibble' (tbl_df) form, and creates a #wave-length list of
#'     class = matrix (or class =network) networks for the requested
#'     relationship type and set of requested waves.
#'
#' @param pInTB (tbl_df) A long-format tibble, i.e. as created by
#'     'makeLongNet'. This table will usually include several waves, but it
#'     does not have to. \cr
#'     IMPORTANT NOTE: It is assumed that all of the SIDs (and, if
#'     includeAltID=TRUE, AltIDs) in this tbl are exactly the ones you
#'     want in your final data analysis set; thus, any pruning of these
#'     IDs must have occurred prior to running this function.
#' @param pWavVec (numerical vector, default c(1)) A vector of waves to pull
#'     from pInTB.
#' @param pTypNet (character string, default = "FR") Specifies type of network
#'     relationship to use to construct the network. Possibilities are: \cr
#'     FR: friend (1=close friend,2=friend, 3=acquaintence,
#'     4=stranger,5=adversary) \cr
#'     LO: loan $ (1=$0, 2=$10, 3=$50, 4=$100, 5=$500) \cr
#'     HE: help   (1=very likely, 2=likely, 3=maybe, 4=prob not, 5=no) \cr
#'     CO: conversation (1=daily, 2=almost daily, 3=every few days, 4=weekly,
#'                 5=almost never) \cr
#'     AD: advice from (1=very often, 2=quite often, 3=regularly, 4=rarely,
#'                  5=never) \cr
#'     ST: strength (1=very strong, 2=strong, 3=weak, 4=none, 5=negative)
#' @param pTypOut (character string, default "MX") MX for matrix output, or
#'     "NT" for 'network' class output.
#' @param pTHold (number, default 0) Threshold for recoding a relationship
#'     value (selected with pTypNet) to 0 or 1. If 1-5, it means 'this value or
#'     higher = 1; else 0'. If <0, the meaning is
#'     'this value or lower = 1'. Default is 0, meaning 'do not recode;
#'     just return the originally-coded rating'.
#' @param includeAltID (logical; default=FALSE). If TRUE, any individuals
#'     who appear in pInTB as AltIDs only (chosen by some SID) are
#'     to be included in the analysis even if they never appear as
#'     choosers (SIDs). According to project conventions, this implies
#'     that such individuals would not have participated in the survey for
#'     the wave in question, and thus would have only incoming relationship
#'     ratings. (Normally such data is not included, but it might be, for
#'     example if one wanted to impute outgoing ties for such individuals,
#'     etc.)
#' @param not_active_code Default=10. This is the code put into the adjacency
#'   matrix if the individual represented by that row & column did not
#'   contribute network data during wave 'pWave'
#'
#' @return A length(pWavVec+1)-length list of wave-specific matrices, in
#'     standard base R 'matrix' format (adjacency matrix). The
#'     w+1st element is a tibble with a row for every SID in the analysis,
#'     plus a column for each wave with the house (HID) the person was
#'     assigned to that wave. This value is NA if and only if the person
#'     did not participate in that wave.
#'
#' @export
makeNetworkSet <- function(pInTB,
                       pWavVec = c(1),
                       pTypNet = "FR",
                       pTypOut = "MX",
                       pTHold = 0,
                       includeAltID = FALSE,
                       not_active_code = 10){
  #' @import Matrix
  #' @import dplyr
  #_____________________

  # Vector of allowable 'pTypNet' codes
  rlpTypVec <- c("FR", "LO", "HE", "CO", "AD", "ST")

  # Input parameter errors.
  if (!(pTypNet %in% rlpTypVec)){
    stop("Network Type must be FR, LO, HE, CO, AD or ST")
  }
  if (abs(pTHold) > 5){
    stop("Network threshold must be between -5 and +5, 0 for no recode.")
  }
  if (!"tbl_df" %in% class(pInTB)){
    stop("the input network object must be a tbl_df")
  }

  # Connect input relationship type 'short name' to col names in input DT
  rlpName <- assignRlpName(pTypNet)

  # Select the network (relationship) specifically requested
  # Creates a 'rlpNet'-type 5-column edge list of class 'tbl_df':
  rlpNet <- getNet4OneRlp (pInTB, pWavVec, rlpName)
  # Recode the relationship var if user gives a nonzero threshold:
  if(pTHold != 0){
    rlpNet[[5]] <- recodeRlpVar(rlpNet[[5]], pTHold)
  }

  # Creates a list (x wave) in adjacency matrix or class'network' format:
  outList <- makeNetworkList(rlpNet, rlpname, pWavVec, pTypOut,
                             includeAltID,
                             not_active_code = not_active_code)
  # Add 'SIDInAnyWave table to output  (now req.)
    # Format input required by makeSIDHIDLookups
    inList <-   vector(mode = "list", length = length(pWavVec))
    for (i in 1:length(pWavVec)){
      inList[[i]] <- rlpNet %>%
        filter(WID==pWavVec[i])
    }

    outList$SIDInAnyWave <- inList %>%
      makeSIDHIDLookups(includeAltID = includeAltID) %>%
      makeSIDInAnyWave()
    #Add class and attributes
    class(outList) <- c("networkset", "list") #networkset is shown first
    attr(outList, 'rlptype') <- pTypNet #Friend, loan, etc...
    attr(outList, 'rlpTHold') <- pTHold #Threshold for a 1 value
    attr(outList, 'size') <- dim(dplyr::last(outList))[1] # Num SIDs
    attr(outList, 'uniqueHID') <- "NA" # each SID in only 1 house? (T/F) (later?)
    attr(outList, 'waves') <- pWavVec # which waves represented
 return (outList)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> assignRlpName <<
#_____________________
#' Converts a relationship "short name" string (e.g. FR, LO, etc. ) to a
#' full (and more descriptive) string (e.g. friend, loan, etc. )
#'
#' @param pRlpShortName The two-character 'short name' code for each
#'    relationship type
#'
#' @return The name  as a string
#' @export
assignRlpName <- function(pRlpShortName){
  vNames <- c("AltID","friend", "loan", "help", "conv", "advc", "rate")
  # lookup table of variable relationship name and input relationship parameter
  vNamesParms <- data.frame (vNames[-1],c("FR", "LO", "HE", "CO", "AD", "ST"),
                             stringsAsFactors = F)
  names(vNamesParms) <- c("vname","parm")
  rlpName <- vNamesParms$vname[match(pRlpShortName,vNamesParms$parm)]
  return(rlpName)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeListOfSIDsByHouse <<
#____________________________
#' Returns a list of labeled house elements, each containing a vector of
#' the SIDs in that house for the wave specified.
#'
#' @param rlpNet (tbl_df) Requires the format produced by
#'    'getNet4OneRlp', i.e. with columns: SID, HID, WID, AltID, <rlpname>,
#'    where <rlpname> is one of: friend, loan, help, conv, advc, or rate.
#'    It would also work with any long-format edgelist that has the
#'    columns WID, SID, HID, and AltID.
#' @param pWID (scaler, default 1) The wave number being processed
#' @param includeAltID (logical, default FALSE)
#'
#' @return A list of vectors, each element containing the SIDs of
#'    the residents of the corresponding house. Elements are named by
#'    house number, and are ordered by house number.
#'
#' @export
makeListOfSIDsByHouse <- function(rlpNet, pWID=1, includeAltID=FALSE){
  #' @import dplyr
  if(!all(c("SID", "HID", "WID", "AltID") %in% names(rlpNet))){
    stop("\nmakeListOfSIDsByHouse: input lacks SID, WID, HID, or AltID.")
  }
  if(includeAltID==TRUE){
    SIDVec <- sort(unique(c(rlpNet$SID, rlpNet$AltID)))
  } else {
    SIDVec <- sort(unique(rlpNet$SID))
  }
  SIDHID <- makeSIDHIDLookups(list(rlpNet), includeAltID)[[1]]
  sidHIDList <- split(SIDHID$SID, as.factor(SIDHID$HID))

  return(sidHIDList)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getNet4OneRlp <<
#_____________________
#' Returns an edgelist data.table for just 1 type of relationship
#'
#' @param tbLong A long-format tibble, output by 'makeLongNet'
#' @param pWavVec A vector of waves; default is just the 1st wave
#' @param pRlpName A character string with the name of the relationship;
#'     this must be a name in dtLong corresponing to a type of relationship.
#'     These are: friend, loan, help, conv, advc, rate.
#'
#' @return Returns an edgelist-format tibble, with the columns SID, HID,
#'     WID, and AltID for the selected relationship type. Will have the same
#'     number of rows as tbLong, less any for which the relationship selected
#'     for has NAs.
#'
#' @export
getNet4OneRlp <- function(tbLong,
                          pWavVec = c(1),
                          pRlpName){
  #' @import dplyr
  # ___________________
  # Select relationship column
  rlps <-  c("friend", "loan", "help", "conv", "advc", "rate")
  if (!(pRlpName %in% rlps)){
    stop(paste("Error: Relationship name must be friend, loan, help, ",
               "conv, advc, or rate."))
  }

  outNet <- as.data.frame(tbLong)[, c("SID", "HID", "WID", "AltID", pRlpName)]
  # NOTE: the switching back & forth between data.table and dataframe is
  #  necessitated because there is no general way I know of to pass a
  #  variable name as a string.
  # Select waves, weed out NAs
  #outNet <- outNet[outNet$WID %in% pWavVec & !is.na(outNet[[pRlpName]]),]
  outNet <- outNet[outNet$WID %in% pWavVec,]
  return (as_tibble(outNet))
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeLongNet <<
# _____________________________________________________________________________
#' Create long-format tibble from DT converted from SPSS file
#'
#' This tibble is an edge-list format of choosers x chosen (for each of the
#'    relationship criteria friend, loan, help, conv, advc, and rate.
#' @param pInTB A 'wide' tibble created by 'inputSPSS', or equivalent format.
#'
#' @return A tibble with multiple rows per 'chooser' SID, one for each
#'     chosen alter. The row format is:
#'     SID -- chooser's (ego's) ID
#'     HID -- chooser & chosen's House ID
#'     WID -- wave ID
#'     AltID -- chosen (alter's) ID
#'     friend -- 'friend' relationship (1=close,...5=advesary)
#'     loan -- 'would loan $' relationahip (1=$0,...,5=$500)
#'     help -- 'would help' relationship (1=very likely,...5=no)
#'     conv -- 'freq conversation' relationship (1=daily,...5=almost never)
#'     advc -- 'receive advice' relationship (1=very often,...5=never)
#'     rate -- 'strength' of relationship (1=very strong,...5=negative)
#'
#' @export
makeLongNet <- function(pInTB){
  #' @import dplyr

  # Vector of allowable 'pTypNet' codes
  rlpTypVec <- c("FR", "LO", "HE", "CO", "AD", "ST")

  # Input parameter errors.
  if (length(class(pInTB)) == 3){
    if (class(pInTB)[1] != "tbl_df"){
      stop("Error: the input network object must be a tibble")
    }
  } else {
    if (class(pInTB) == "data.frame"){
      cat("Adding class tbl_df to data.frame")
      pInTB <- as_tibble(pInTB)
    }
  }

  # Create vectors of names that are 1 variable in long format
  ridx    <- c("res1id", "res2id", "res3id", "res4id", "res5id",
               "res6id", "res7id", "res8id", "res9id")
  friendx <- c("res1friend", "res2friend", "res3friend", "res4friend",
               "res5friend", "res6friend", "res7friend", "res8friend",
               "res9friend")
  loanx   <- c("res1loan", "res2loan", "res3loan", "res4loan", "res5loan",
               "res6loan", "res7loan", "res8loan", "res9loan")
  helpx   <- c("res1help", "res2help", "res3help", "res4help", "res5help",
               "res6help", "res7help", "res8help", "res9help")
  convx   <- c("res1conv", "res2conv", "res3conv", "res4conv", "res5conv",
               "res6conv", "res7conv", "res8conv", "res9conv")
  advcx   <- c("res1advc", "res2advc", "res3advc", "res4advc", "res5advc",
               "res6advc", "res7advc", "res8advc", "res9advc")
  ratex   <- c("res1rate", "res2rate", "res3rate", "res4rate", "res5rate",
               "res6rate", "res7rate", "res8rate", "res9rate")

  # Names of long-form variable sets
  vNames <- c("AltID", "friend", "loan", "help", "conv", "advc", "rate")
  # lookup table of variable relationship name and input relationship parameter
  vNamesParms <- data.frame (vNames[-1],c("FR", "LO", "HE", "CO", "AD", "ST"),
                             stringsAsFactors = F)
  names(vNamesParms) <- c("vname","parm")
  # ...the reshaped dataframe:
  tb.Long <- reshape(as.data.frame(pInTB), direction = "long", idvar =  "SID",
                     varying = list(ridx, friendx, loanx, helpx, convx, advcx, ratex),
                     v.names = vNames)
  # ...which we then sort by AltID within SID, and remove rows
  # that had no data (corresponding to houses with only n<9 residents)
  tbLong <- tb.Long %>%
            as_tibble() %>%
            filter(!is.na(AltID)) %>%
            select(SID, HID, WID, AltID, friend, loan, help,
                   conv, advc, rate)
  return(tbLong)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeAdjMatrix <<
#FFFFFFFFFFFFFFFFFF
#' Makes an adjacency matrix from an edge list, with SID-labeled rows/cols
#'
#' @param rlpNetAllWaves An edge list network of the form created by
#'   'getNet4OneRlp', i.e. a tbl_df, with cols: SID, HID, WID, AltID, and
#'   one of: friend, loan, help, conv, advc, rate. If for one wave of a
#'   multiwave set, this tbl MUST include all edges for all waves
#'   (see 'details' for explanation).
#' @param pWave A numeric value indicating which WID to use from rlpNet, if the
#'   latter includes multiple waves (which it should, generally, when creating
#'   network matrices for a multiwave analysis; see 'details' for more on this)
#' @param includeAltID Default=FALSE. If TRUE, both AltIDs and SIDs are
#'   included in the output matrix, so that individuals who do not appear
#'   as a chooser (SID) might still be included in the output. If FALSE,
#'   only choosers (with an SID in rlpNet) will be included; any links
#'   to AltIDs who are not also SIDs are ignored.
#' @param not_active_code Default=10. This is the code put into the adjacency
#'   matrix if the individual represented by that row & column did not
#'   contribute network data during wave 'pWave'
#' @details The function does not break up rlpNet in any way; that is, it
#'   returns a single matrix, with 1's where SID chose AltID, and 0's
#'   otherwise.\cr
#'   *Importantly*, if this function is being used to create a network
#'   matrix for a particular wave of a multiwave analysis, and if also
#'   included SIDs do not necessarily appear in all waves (which is typical),
#'   the input tbl (rlpNetAllWaves) must include the data for the entire
#'   analysis, i.e. all edges for all waves.
#' @note The function *does not* add structural zeros between individuals in
#'   different houses. For that, create a network set and input it to
#'   the function 's0Fill'; you get output in the same format, but with the
#'   structural zeros added.
#' @return A matrix of dim unique(SID) (or if includeAltID=TRUE, of
#'   unique(SID U AltID)), containing the values appearing in col 5 of
#'   rlpNet, and with rownames = colnames = the original ID values (SID
#'   &/or AltID), to identify which rows and columns refer to which individuals
#'   in rlpNet.
#' @export
makeAdjMatrix <- function(rlpNetAllWaves, pWave=0, includeAltID=FALSE,
                          not_active_code = 10){
#' @import dplyr

  if(length(pWave)>1){
    stop("\nmakeAdjMatrix pWave must be a scalar (numeric & length 1)")
  }
  if(dim(rlpNetAllWaves)[2]!=5){
    stop("\nmakeAdjMatrix input must be a 5-col tbl_df.")
  }
  if(!all(names(rlpNetAllWaves)[1:4]==c("SID", "HID", "WID", "AltID"))){
    stop("\nmakeAdjMatrix requires SID HID WID AltID as 1st 4 input columns")
  }
  if(!names(rlpNetAllWaves)[5] %in% c("friend","loan","help",
                                      "conv","advc","rate" )){
    stop(paste("\nName of makeAdjMatrix col 5 input must be friend, loan,",
               "\nhelp, conv, advc, or rate.",sep=""))
  }
  if(pWave<1 | pWave>7){
    stop("\nmakeAdjMatrix pWave value must be between 1 and 7")
  }
  if(length(unique(c(rlpNetAllWaves$SID, rlpNetAllWaves$AltID))) !=
     length(unique(c(rlpNetAllWaves$SID))) & includeAltID==FALSE){
    stop(paste("\nmakeAdjMatrix: input has AltIDs that are not SIDs, ",
               "\nbut includeAltID=FALSE. Either remove those AltIDs ",
               "\nor set includeAltID=TRUE.", sep="")
         )
  }

  # Select the SIDs (and possibly AltIDs-only) to be included in the
  # analysis and the current wave. Also select the network data
  # compatible with the selected set of IDs.
  if(includeAltID == FALSE){
    allIDs <-sort(unique(c(rlpNetAllWaves$SID)))
    allIDs.now <- rlpNetAllWaves %>%
      filter(WID == pWave) %>%
      arrange(SID) %>%
      pull(SID) %>%
      unique()
    rlpNet.allIDs <- rlpNetAllWaves %>%
      filter(SID %in% allIDs) #Any AltID must also be an SID
  }

  if(includeAltID == TRUE){
    allIDs <-sort(unique(c(rlpNetAllWaves$SID,
                           rlpNetAllWaves$AltID)))
    allIDs.now <- rlpNetAllWaves %>%
      filter(WID == pWave) %>%
      arrange(SID)
    allIDs.now <- sort(unique(c(allIDs.now$SID, allIDs.now$AltID)))
    SIDs.now <- rlpNetAllWaves %>%
      filter(WID == pWave) %>%
      arrange(SID) %>%
      pull(SID) %>%
      unique()
    altOnly.now <- setdiff(allIDs.now, SIDs.now)
    rlpNet.allIDs <- rlpNetAllWaves %>%
      filter(SID %in% allIDs | AltID %in% allIDs) #Some AltIDs may have no SID
  }
  not.now <- setdiff(allIDs, allIDs.now) #...differs by includeAltID T or F

  #Create an empty adjacency matrix & label rows, cols. Same dim every wave.
  adjMatrix <- matrix(0, length(allIDs), length(allIDs))
    rownames(adjMatrix) <- colnames(adjMatrix) <- allIDs

  # Replace SIDs with RowIDs
  #   1. Create SID-Row ID crosswalk
  SIDRID <- tibble(SID = allIDs, RID = 1:length(allIDs))
  #   2. Replace SIDs with Row IDs in the rlpNet table (other SID
  #      vectors are thus replaced just before they are used to
  #      select relevant matrix entries for coding)

  rlpNet.allIDs$SID   <- sid2rid(rlpNet.allIDs$SID, SIDRID)
  rlpNet.allIDs$AltID <- sid2rid(rlpNet.allIDs$AltID, SIDRID)
  names(rlpNet.allIDs) <- c("ROut", "HID", "WID", "RIn",
                            names(rlpNet.allIDs[5]))
  #   3. Make matrix of ego, alt, value for current wave
  rlpNetMX <-rlpNet.allIDs %>%
    mutate(rlp = rlpNet.allIDs[[5]]) %>%
    filter(WID == pWave) %>%
    select(ROut, RIn, rlp) %>%
    as.matrix(rownames.force=T)
  #Coding note: the rownames.force parameter eliminates an irrelevant
  #  warning message related to a converted matrix object's
  #  rownames property. See:
  # https://stackoverflow.com/questions/43209857/transposing-identical-objects

  # If there is data, use this assignment:
  adjMatrix[rlpNetMX[,1:2]] <- rlpNetMX[,3]

  # Other cases requiring special coding
  not.now     <- sid2rid(not.now, SIDRID)
  #     Code for 'not in this wave':
  adjMatrix[not.now,] <- adjMatrix[,not.now] <- not_active_code
  if(includeAltID==TRUE){
    # An alt-Only (with only incoming ties)
    altOnly.now <- sid2rid(altOnly.now, SIDRID)
    adjMatrix[altOnly.now,] <- not_active_code
  }
  return(adjMatrix)
}

# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> sid2rid <<
# FFFFFFFFFFFFFFFF
#' Transforms vector of SIDs into vector of Row IDs
#'
#' This function is just a light wrapper on the 'match' function, and a little
#' easier to read.
#'
#' @param SIDVec (numerical vector) The SIDs to be transformed into Row IDs.
#'   Normally this should be sorted in ascending numerical order, but
#'   'row id's' will be assigned based on the vector elements' order. If any
#'   of these values have no SID value in the 'SIDHID' table, the function
#'   fails and returns an error message.
#' @param SIDRID (tbl_df) The SID-Row ID crosswalk to be used to assign
#'   RIDs to the SIDs in SIDVec. The two columns must be named SID and RID,
#'   respectively.
#'
#' @return (numerical vector) A vector of of the same length as SIDVec, with
#'   the SIDs replaced by Row IDs. The Row IDs are simply the order number
#'   of the SID element, sorted ascending.
#' @export
sid2rid <- function(SIDVec, SIDRID){
  #SID-Row ID Crosswalk
  RIDVec <- SIDRID$RID[match(SIDVec,SIDRID$SID)]
  if(any(is.na(RIDVec))){
    stop("Function sid2rid: input vector has SID with no matching RID")
  }
  return(RIDVec)
}

# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeRegionVar <<
# FFFFFFFFFFFFFFFFFFF
#' Recodes a vector of House IDs into region IDs.
#'
#' Region 1: NC \cr
#' Region 2: TX \cr
#' Region 3: OR
#'
#' @param pHIDs A vector of House ID #'s (integers) to be assigned to regions
#'
#' @return A vector of region numbers the same length as the input vector
#' @export
makeRegionVar <- function (pHIDs){
  outVec <- as.numeric(cut(pHIDs,c(0,40,70,90),right=F,labels=c(1:3)))
  return (outVec)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeNetworkList <<
#FFFFFFFFFFFFFFFFFF
#' Creates a w length list of 'Matrix' class networks,
#'     plus (if requested) the sidHID dataframe as the w+1st element.
#'
#'  w is the number of waves, with one network per wave. Elements of the list
#'      are named "wv1", "wv2" (the specific wave numbers that appear in
#'      pinTB$WID).
#'
#' @param rlpNet A 5-column tbl_df Columns are SID, HID, WID, AltID, and
#'     a final column with the name of one of the 6 types of relationships
#'     measured in this study. Legal values are friend, loan, help, conv,
#'     advc, rate. This dataframe should have edges for all of the waves
#'     included in the parameter 'pWavVec', but if it does not have any
#'     for a particular wave, that element of the output list will simply
#'     be empty.
#' @param pRlpName The name of the relationship measured by this set of
#'     networks; see 'makeSparse' for the allowed values. This column
#'     normally will contain scored (0/1) values, though there is no
#'     requirement in this function for this to be true.
#' @param pWavVec (default c(1)) A w-length vector with the numbers of
#'     the waves to create list elemenets for.
#' @param pTypOut (default c("MX")) Character sting: MX for matrix, NT for
#'     class 'network'.
#' @param includeAltID (default FALSE)
#' @param not_active_code Default=10. This is the code put into the adjacency
#'   matrix if the individual represented by that row & column did not
#'   contribute network data during wave 'pWave'
#'
#' @return Returns a list of networks in matrix form, one element per
#'     unique wave appearing in the WID column of pinTB
#'
#' @note For internal use only (called by 'makeNetworkSet'
makeNetworkList <- function (rlpNet, pRlpName,
                             pWavVec = c(1),
                             pTypOut = c("MX"),
                             includeAltID=FALSE,
                             not_active_code = 10){
  #' @import dplyr
  #' @import Matrix
  #' @import network

  nWaves <- length(pWavVec) # waves requested
  nnodes <- nWaves
  # Pre-create output list for 1 element per wave
  outList <- vector(mode = "list", length = nWaves)
  # data frame.

  for (i in 1:nWaves){
    if(pTypOut=="NT"){
      #Make it a 'network' object
      outList[[i]] <- network(makeAdjMatrix(rlpNet, pWave=pWavVec[i],
                                            includeAltID,
                                            not_active_code = not_active_code))
    } else {
      if (pTypOut == "MX"){
        #Make it a 'matrix' object (default)
        outList[[i]] <- makeAdjMatrix(rlpNet, pWave=pWavVec[i],
                                      includeAltID=includeAltID,
                                      not_active_code = not_active_code)
      } else{
        stop("Output type must be MX or NT")
      }
    }
    itemName <- paste("wv", toString(pWavVec[i]), sep = "")
    names(outList)[i] <- itemName
  }
  return (outList)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#  >> selectHouses <<
#______________________________________________________________________________
#' Select or remove a specified set of houses from a network
#'
#' @param rlpNet (tbl_df) in in the form created by 'makeLongNet', and
#'   meant to represent a network edge list. Will actually accept any such
#'   input containing at least the variables SID, HID, WID, and AltID.
#'   May be single- or multi-wave
#' @param pHIDVec (numerical vecto) Vector of House ID numbers. They
#'   must be either all positive numbers, or all negative, or an error is
#'   thrown. If all positive, these house IDs are to be included. If all
#'   negative, these house IDs are to be excluded
#' @return A tbl_df in the same format as the input, including only the
#'    user-specified set of houses from pHIDVec
#' @export
selectHouses <- function(rlpNet, pHIDVec){
#' @import dplyr
  if(!all(c("SID", "HID", "WID", "AltID") %in% names(rlpNet))){
    stop("\nmakeListOfSIDsByHouse: input lacks SID, WID, HID, or AltID.")
  }
  if(!all(abs(pHIDVec) %in% abs(rlpNet$HID))){
    cat("\nWarning: some HIDs passed to selectHouses are not in the input.")
  }
  if (isTRUE(all(pHIDVec >0))){
    # Include HIDs in pHIDVec
    rlpNet <- rlpNet %>%
      filter(HID %in% pHIDVec)
    return(rlpNet)
  } else{
    if (isTRUE(all(pHIDVec < 0))){
      # Exclude HIDs in pDHIVec
      rlpNet <- rlpNet %>%
        filter(!(HID %in% -pHIDVec))
      return(rlpNet)
    } else{
      cat("\nWarning: HIDs must be all >0 or all <0;")
      cat("\nReturning input DT unchanged\n")
    }
  }

  return(rlpNet)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeSIDHIDLookups
#______________________________________________________________________________
#' Make a w-length list of SID-HID lookup tables from a w-length list of
#'    edge-format tibbles (each of which, e.g., created by 'makeLongNet')
#'
#' @param tbList A list of tibbles, one for each wave, in the format created
#'   by 'makeLongNet'. The data must have columns named SID and HID,
#'   representing ego and house, AltID for alter (if 'includeAltID' = TRUE)
#'   and WID for wave.
#'
#' @param includeAltID Logical, default=FALSE. If set to TRUE, the
#'    output lookup tables will include individuals who were chosen
#'    as alters (in 'AltID' col), but may not have been egos (in
#'    'SID' col), implying they did not survey that wave. Normally
#'    this is not desired.
#'
#' @details Length of the input list must be >= 1. If includeAltID=FALSE,
#'   the resulting output lists will only include individuals who actually
#'   surveyed on the wave in question. Otherwise it may include some
#'   house residents who were nonparticipants.
#'
#' @return If w is the number of distinct waves (WID), i.e. the length of
#'   the input list, a w-length list of SID-HID lookup data.tables is returned,
#'   showing SID membership in each house for each wave. Within a wave, any SID
#'   belong to one and only one HID.
#'
#' @note Because an analysis cannot meaningfully include an individual who is
#'   in more than one house, the function will not work if such a condition is
#'   found. Thus, the input 'tbList' elements must already have been filtered
#'   to remove cases where an individual appears in more than one house.
#'
#' @export
makeSIDHIDLookups <- function(tbList,
                             includeAltID = FALSE){
#' @import dplyr
  #Integrity checks
  for (i in 1:length(tbList)){
    if(!("tbl_df" %in% class(tbList[[i]]))){
      stop(paste("\n", "Input list elements must be of type tbl_df",
                 "; element ", i, ".", sep=""))
    }
    if(!("SID" %in% names(tbList[[i]]) & "HID" %in% names(tbList[[i]]))){
      stop(paste("\n", "tbl missing SID or HID",
                 "; element ", i, ".", sep=""))
    }
    if(includeAltID==TRUE & !("AltID" %in% names(tbList[[i]]))){
      stop(paste("\n", "tbl missing AltID",
                 "; element ", i, ".", sep=""))
    }
  }
  #Which waves are represented in the input table?
  # (gets actual WIDs, not just integers 1,...,w)
  waves <- numeric(length = length(tbList))
  for (i in 1:length(tbList)){
    waves[i] <- tbList[[i]][1,] %>%
      select(WID)
  }
  # If 2+ input list elements contain the same WID:
  if(length(waves) != length(unique(waves))){
    stop("In makeSIDHIDLookups, 2 or more waves are duplicated")
  }
  nwaves <- length(tbList) #...and how many

  # Prepare (empty) output list
  outList <- vector(mode = "list", length = nwaves)

  if(includeAltID==FALSE){
  for (i in seq(waves)){
    outList[[i]] <- tbList[[i]] %>%
      distinct(SID, HID) %>%
      select(SID, HID) %>%
      arrange(SID, HID)
    # Integrity check: no one can be in 2+ houses for any given wave:
    if(length(outList[[i]]$SID) != length(unique(outList[[i]]$SID))){
      stop(paste("\nIn makeSIDHIDLookups, nonunique SID for wave ",i))
    }

  }
  } else{#Add AltIDs in addition to SIDs
    cat(paste("\nWarning: AltIDs included in SIDHID lookup.",
              " Some may be nonparticipants.", sep="")
    )
    for (i in seq(waves)){
      tbAltID <- tbList[[i]] %>%
        distinct(AltID, HID) %>%
        select(AltID, HID) %>%
        mutate(SID = AltID) %>%
        select(SID, HID)

      outList[[i]] <- bind_rows(tbList[[i]],tbAltID) %>%
        distinct(SID, HID) %>%
        arrange(SID, HID)
      # Integrity check: no one can be in 2+ houses for any given wave:
      if(length(outList[[i]]$SID) != length(unique(outList[[i]]$SID))){
        stop(paste("\nIn makeSIDHIDLookups, nonunique SID for wave ",i))
        }
    }
  }
  return(outList)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> makeSIDInAnyWave <<
#FFFFFFFFFFFFFFFFFFFFFFFF
#' Creates a tibble of SIDs x houses in each wave
#'
#' @param pInLst (list) A list of tbls, one for each wave of interest,
#'   with columns SID and HID, e.g. as created by 'makeSIDHIDLookups'.
#' @return Returns a single tbl (tbl_df) with one row for any of the SIDs
#'   found in pInLst, and 1 column for each wave, containing the HID the
#'   the individual was considered to have been living in for that wave.
#'
#' @details If the individual was not a resident anywhere (i.e. not in the
#'   study), the value for that SID and wave is NA. If the individual switched
#'   houses while surveys were in progress, it is possible he/she could have
#'   had a longNet row from two houses; however, he/she could only have
#'   surveyed in one, and this is the one that should be assigned to that SID.
#'   If such a switch occurred but the individual did not survey in either
#'   house (we've never seen this but it's possible), then if that SID is to
#'   be included for some reason (normally one would not),
#'   then an ad hoc decision must be made in the data prep script.
#' @export
makeSIDInAnyWave <- function(pInLst){
#' @import dplyr
  for (i in 1:length(pInLst)){
    if(!("SID" %in% names(pInLst[[i]]))){
      stop("\nCol named SID missing from makeSIDInAnyWave input.")
    }
    if(!("HID" %in% names(pInLst[[i]]))){
      stop("\nCol named HID missing from makeSIDInAnyWave input.")
    }
  }

  SIDVec <- vector(mode="numeric") #Create SID Vector & build it up
  for (i in 1:length(pInLst)){
    SIDVec <- c(SIDVec, pInLst[[i]]$SID)
  }
  SIDVec <- unique(SIDVec)

  outTbl <- tibble(SID=SIDVec)
  # Tack on HID columns for each wave.
  for (i in 1:length(pInLst)){
    outTbl <- outTbl %>%
      left_join(pInLst[[i]], by = "SID")
    names(outTbl)[i+1] <- paste("HID",i,sep="")
  }
  #NOTE: One deficienty with this is that waves will always be labeled
  # sequentially, rather than by some actual wave identifier. If this is
  # a problem, you could either pass in a wave vector, or require that
  # the input list elements have names n corresponding to the wave number
  # they represent.
  return(outTbl %>%
           arrange(SID))
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> getHouseSwitchers <<
#FFFFFFFFFFFFFFFFFFFFFFFF
#' Finds SIDs of anyone in 2 or more different houses
#'
#' @param pInTB A tibble representing an edge-list multiwave set of networks,
#'   i.e., with at least variables SID, AltID, HID, WID. Any other vars
#'   are ignored. The output of 'makeLongNet' works for this, as does the
#'   object generated by 'getNet4OneRlp'
#'

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> s0Fill <<
#FFFFFFFFFFFFFFFFFFFFFFFF
#' Fills a 'networkSet' of networks with structural zeros between houses
#'
#' @param netSet A 'networkSet' object, equivalent to the output of
#'   the 'makeNetworkSet' function, i.e. a list structured as:
#'   1. w 'matrix' objects of equal dimension n, with SIDs as
#'      row and column names;
#'   2. One n x w+1 'tbl', sorted on the first column (SID) which gives
#'      + The SID of each individual in the entire network set as col 1;
#'      + Additional columns HID1, HID2, ...HID<w> giving the house
#'        membership ID for each one.
#'
#' @details At each wave, the wth adjacency matrix has structural zeros (10)
#'   added for all pairs of individuals who are not in the same house for that
#'   wave.
#'
#' @return A 'networkSet' object identical to the input network Set, but with
#'   structural zeros added (as explained in 'details')
#'
#' @export
s0Fill <- function(netSet){
#' @import dplyr
  if(c("networkset") %in% class(netSet)) {} #OK to continue
     else{
       stop("s0Fill: input not of class *networkset*")
     }
  nWaves <- length(netSet)-1
  outList <- vector(mode = "list", length = nWaves+1)

  if (!(c("tbl") %in% class(netSet[[nWaves+1]]))){
    stop("Network Set Object must include SIDInAnyWave Table as last element")
  }
  for(i in 1:nWaves){
    sidhid <- netSet[[nWaves+1]][,c(1,i+1)] #tbl of SID, HIDi
    hids <- sidhid %>%                      #vector of unique HIDs
      filter(!is.na(sidhid[,2])) %>%
      pull(names(sidhid[2])) %>%
      unique()
    combos <- combn(hids, 2, simplify = F)   #List of all pairs of houses
    mm <- netSet[[i]] #Network for ith wave
    #Closure, returns s0-filling function specific to this wave
    s0FillNet <- s0Fill.plusData(hids, mm, sidhid)
    outList[[i]] <- s0FillNet(combos)
  }
  #Copy class attributes (poor man's inheritance!)
  outList[[nWaves+1]] <- netSet[[nWaves+1]]
  class(outList) <- c("networkset", "list") #networkset is shown first
  attr(outList, 'rlptype') <- attr(netSet, 'rlptype') #Friend, loan, etc...
  attr(outList, 'rlpTHold') <- attr(netSet, 'rlpTHold') #Threshold for a 1 value
  attr(outList, 'size') <- attr(netSet, 'size') # Num SIDs
  attr(outList, 'uniqueHID') <- attr(netSet, 'uniqueHID') #Unique HID/SID? (NA)
  attr(outList, 'waves') <-  attr(netSet, 'waves') # which waves represented

  return(outList)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> s0Fill.plusData <<
#FFFFFFFFFFFFFFFFFFFFFFFFF
#' Return a function with data used to zero-fill a single-wave network
#'
#' @param hids (vector) The unique house IDs applicable to the zero-fill
#'   operation in progress. Normally this will be obtained from one of the
#'   columns (2,...w+1) of an object as created by 'makeSIDInAnyWave'.
#' @param mm (matrix) The adjacency matrix to be zero-filled.
#' @param sidhid (tbl) A two-column tbl (tbl_df) of SIDs and their HIDs for
#'   the current wave. There should be no missing values; anyone missing a
#'   HID would need to have been removed prior to calling this function.
#' @return (function) A closure, which has the values of the input parameters
#'   as part of its execution environment. It is designed to be called
#'   repeatedly by 's0Fill', once for each wave to be zero-filled, each time
#'   returning a zero-filled matrix otherwise equivalent to the input 'mm'.
#' NOT EXPORTED <<
s0Fill.plusData <- function(hids, mm, sidhid){
#' @import dplyr
  function(comboList){
    # 'listElement' is a length-2 vector of the 2 houses
    #  in the list of all combos of houses in this wave
    #  (see 'combn' function used in 's0Fill')
    #Get the index values from sidhid (1 to size of network)
    #  for respective members of each of these houses
    #NOTE: this 'sidhid' only has 2 cols, SID and the HID for
    #  this particular wave (i.e. HID1, HID2, etc...)

    ncombos <- length(comboList)
    for (i in 1:ncombos){
      HIDColName <- as.name(names(sidhid[2])) #Get name of HID col
      h1memb <- which(sidhid$SID %in% (sidhid %>%
                                         filter(!!HIDColName == comboList[[i]][1]) %>%
                                         pull(SID)))
      h2memb <- which(sidhid$SID %in% (sidhid %>%
                                         filter(!!HIDColName == comboList[[i]][2]) %>%
                                         pull(SID)))
      #_____________________________________________________________________
      #Fill differing-house-pair submatrices both (symmetrically)
      #  above & below diag with structural zeros.
      sidPairsAboveDiag <- expand.grid(h1memb, h2memb) %>% as.matrix()
      sidPairsBelowDiag <- expand.grid(h2memb, h1memb) %>% as.matrix()
      mm[sidPairsAboveDiag] <- 10
      mm[sidPairsBelowDiag] <- 10
      #_____________________________________________________________________
      #Insert NAs for anyone with a missing HID for this wave (=>missing
      #  for this wave). This is not really necessary if one is using
      # CompChange to identify arrivals/departures, but if one cannot
      # use that approach (e.g. if using MLE or Bayes) then this coding
      # signals to RSiena not to include these individuals in affected
      # between-wave simulations.

      #This code is superseded by code from makeAdjMatrix which does the
      # same thing.We keep in as a reminder of how this used to work,
      # and in case a similar type of arrangement might be needed in
      # the future.

      #missingRows <- which(sidhid$SID %in% (sidhid %>%
      #               filter(is.na(!!HIDColName)) %>%
      #               pull(SID)))
      #mm[missingRows,] <- NA
      #mm[,missingRows] <- NA
    }
    return(mm)
  }
}




#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> recodeRlpVar <<
#FFFFFFFFFFFFFFFFFFFF
#' Recodes a vector representing a relationship variable to 0-1
#'
#' @param pInVec A numeric vector representing some relationship variable
#' @param pTHold A number from 1 to 5, or -1 to -5. \cr
#'    If positive, the number means 'this value or higher = 1; else 0'.\cr
#'    If negative, the number means 'this value or lower = 0; else 0'.\cr
#'    Default is 0, which will not do any recoding.
#' @return Returns a numeric vector of 0's and 1's representing the recoded
#'    input vector.
#' @export
recodeRlpVar <- function(pInVec, pTHold){
#' @import dplyr

  if(!class(pInVec)=="numeric"){
    stop("Relationship var to be recoded is not a numeric vector")
  }

  if (pTHold > 0) { #This value or higher
    pInVec <- ifelse(pInVec >= pTHold, 1, 0)
  }
  if (pTHold < 0) { #This value or lower
    pInVec <- ifelse(pInVec <= abs(pTHold), 1, 0)
  }
  return(pInVec)
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> netvtxAttr <<
#FFFFFFFFFFFFFFFFF
#' Creates a 'network' node attribute
#'
#' @param pAttrMasterDF A two-column data frame or tbl_df with SID and the
#'   values of some variable 'x' for (at least) all the SIDs in the desired
#'   set, for which there are data (if no data, attribute will be NA). 'x'
#'   can be any name you like.
#' @param pSIDVec A vector of SIDs exactly matching the nodes in the desired
#'   set for the network of interest. This will be the length of the vertex
#'   set (#rows in the returned DF or tbl_df), i.e. '<networkObj>$oel'.
#'
#' @return A n x 2 data frame or tbl_df (whichever was passed in), where n
#'   is the number of nodes (vertices) to be assigned the value of variable x,
#'   col 1 is SID, and col 2 is x.
#' @note The calling script should check that the number of rows n is equal to
#'   '<networkObj>$oel, and that the same ordering is used in both. \cr
#'   Note also that this is a pretty simple function; it really only selects a
#'   subset of some variable and makes sure they're ordered by SID.
#' @export
netVtxAttr <- function(pAttrMasterDF,pSIDVec) {
  #' @import network
  #' @import dplyr
  # ________________
  # Check a few things
  if(length(pAttrMasterDF)!=2){
    stop("netVtxAttr input table containing attributes does not have 2 cols.")
  }
  if(names(pAttrMasterDF)[1] != "SID"){
    stop("netVtxAttr input table 1st col is not named 'SID'")
  }
  # You'd want this next check in your script!
  #if (length(pNetwork$oel)!=length(pSIDVec)){
  #  stop("Network vertex count not equal to # SIDs (pSIDVec)")
  #}
  # make sure everything is in order, because vertex attributes are
  # assigned based on some original order of the nodes, which in our
  # case will correspond to SID order
  pAttrMasterDF <- pAttrMasterDF[order(pAttrMasterDF$SID),]
  pSIDVec <- sort(pSIDVec)
  # Select just the required SIDs from pAttrMasterDF
  targDF <- merge(as.data.frame(pSIDVec),pAttrMasterDF,
                  by.x = getNameAsString(pSIDVec), by.y = "SID", all.x = T)
  names(targDF)[1] <- "SID"
  if("tbl_df" %in% class(pAttrMasterDF)){
    targDF <- as_tibble(targDF)
  }
  return(targDF)
}


