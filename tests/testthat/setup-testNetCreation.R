  # 1 wave of wide network data
  wideNetTB.w1 <- readRDS(paste(system.file("testdata", "wideNetTB.w1.RDS",
                                            package = "RHNetTools")))
  # 3 waves of long network data (from makeLongNet)
  ln3 <- readRDS(paste(system.file("testdata", "longNetTB.2of3.RDS",
                                   package = "RHNetTools")))
  #-----------------------------------------------------------------------
  # >> makeNetworkList<<
  # >> makeAdjMatrix  <<

  # 3 tbls in the 'rlpNet' format. Note too that SIDs <> rowIDs.
  # Wave 1: SIDs and AltIDs are identical sets
  testTbl1 <- tibble(SID =   c(2,2,4,4,6,6,8,8,10,10,12,12),
                     HID =   c(10,10,10,10,10,10,20,20,20,20,20,20),
                     WID =   c(rep(1,12)),
                     AltID = c(4,6,2,6,2,4,10,12,8,12,8,10),
                     friend =c(5,3,2,5,3,1,4,1,1,3,1,5))
  # Wave 2: There is 1 AltID (7) in house 2 that is not an SID (i.e. it
  #         imitates a nonpartipant in a network edge list)
  testTbl2 <- tibble(SID =   c(2,2,4,4,6,6,8,8,8,10,10,10,12,12,12),
                     HID =   c(10,10,10,10,10,10,20,20,20,20,20,20,20,20,20),
                     WID =   c(rep(2,15)),
                     AltID = c(4,6,2,6,2,4,10,12,14,8,12,14,8,10,14),
                     friend =c(5,1,3,5,3,2,1,3,1,3,1,5,1,1,3))

  # Wave2 Alternate: SID 14 is removed, sothere are 'altOnly' individuals.
  #       Because makeAdjMatrix assumes no such individuals are present
  #       unless 'includeAltID=TRUE', this is a good way to test cases where
  #       you call 'makeNetworkSet' or its subfunctions wihout that flag set.
  testTbl2a <-tibble(SID =   c(2,2,4,4,6,6,8,8,10,10,12,12),
                     HID =   c(10,10,10,10,10,10,20,20,20,20,20,20),
                     WID =   c(rep(2,12)),
                     AltID = c(4,6,2,6,2,4,10,12,8,12,8,10),
                     friend =c(1,4,2,5,1,1,4,3,4,3,1,4))

  testLst <- list(testTbl1, testTbl2) #Passed into the function
  ttbl <- bind_rows(testTbl1, testTbl2) # w2 has an 'altOnly'
  ttbla<- bind_rows(testTbl1, testTbl2a)# w2 has NO 'altOnly'

  # >> makeAdjMatrix << output
  #    wave 1 (based on ttbla): >OK<
  testAdj_includeAltIDFALSE <- matrix(c(0,5,3,0,0,0,
                                        2,0,5,0,0,0,
                                        3,1,0,0,0,0,
                                        0,0,0,0,4,1,
                                        0,0,0,1,0,3,
                                        0,0,0,1,5,0),
                                      nrow=6, ncol=6, byrow=TRUE)
  rownames(testAdj_includeAltIDFALSE) <- c("2","4","6","8","10","12")
  colnames(testAdj_includeAltIDFALSE) <- c("2","4","6","8","10","12")
  #    wave 2 (based on ttbl): >OK<
  testAdj_includeAltIDTRUE <- matrix(c(0,5,1,0,0,0,0,
                                       3,0,5,0,0,0,0,
                                       3,2,0,0,0,0,0,
                                       0,0,0,0,1,3,1,
                                       0,0,0,3,0,1,5,
                                       0,0,0,1,1,0,3,
                                       rep(NA, 7)),
                                     nrow=7, ncol=7, byrow=TRUE)
  rownames(testAdj_includeAltIDTRUE) <- c("2","4","6","8","10","12","14")
  colnames(testAdj_includeAltIDTRUE) <- c("2","4","6","8","10","12","14")

  # >> s0Fill/Input << (includeAltID = FALSE)
  s0Adjw1.F <- matrix(c(0,1,1,0,0,0,
                      1,0,1,0,0,0,
                      1,0,0,0,0,0,
                      0,0,0,0,1,0,
                      0,0,0,0,0,1,
                      0,0,0,0,1,0),
                    nrow=6, ncol=6, byrow=TRUE)
  rownames(s0Adjw1.F) <- c("2","4","6","8","10","12")
  colnames(s0Adjw1.F) <- c("2","4","6","8","10","12")

  s0Adjw2.F <- matrix(c(0,0,1,0,0,0,
                      1,0,1,0,0,0,
                      0,0,0,0,0,0,
                      0,0,0,0,1,1,
                      0,0,0,1,0,1,
                      0,0,0,0,1,0),
                    nrow=6, ncol=6, byrow=TRUE)
  rownames(s0Adjw2.F) <- c("2","4","6","8","10","12")
  colnames(s0Adjw2.F) <- c("2","4","6","8","10","12")

  s0SIDHID.F <- tibble(SID = c(2,4,6,8,10,12),
                     HID1 = c(10,10,10,20,20,20),
                     HID2 = c(10,10,10,20,20,20))

  s0NSList_altIDFALSE <- list(s0Adjw1.F, s0Adjw2.F, s0SIDHID.F)
  class(s0NSList_altIDFALSE) <- c("networkset", "list")

  # >> s0Fill/Input << (includeAltID = TRUE)
  s0Adjw1.T <- matrix(c(0,1,1,0,0,0,0,
                        1,0,1,0,0,0,0,
                        1,0,0,0,0,0,0,
                        0,0,0,0,1,0,0,
                        0,0,0,0,0,1,0,
                        0,0,0,0,1,0,0,
                        rep(NA, 7)),
                        nrow=7, ncol=7, byrow=TRUE)
  rownames(s0Adjw1.T) <- c("2","4","6","8","10","12","14")
  colnames(s0Adjw1.T) <- c("2","4","6","8","10","12","14")

s0Adjw2.T <- matrix(c(0,1,0,0,0,0,0,
                      1,0,1,0,0,0,0,
                      1,1,0,0,0,0,0,
                      0,0,0,0,0,1,0,
                      0,0,0,0,0,0,1,
                      0,0,0,0,0,0,1,
                      rep(NA,7)),
                      nrow=7, ncol=7, byrow=TRUE)
  rownames(s0Adjw2.T) <- c("2","4","6","8","10","12","14")
  colnames(s0Adjw2.T) <- c("2","4","6","8","10","12","14")

  s0SIDHID.T <- tibble(SID = c(2,4,6,8,10,12,14),
                       HID1 = c(10,10,10,20,20,20,NA),
                       HID2 = c(10,10,10,20,20,20,20))

  s0NSList_altIDTRUE <- list(s0Adjw1.T, s0Adjw2.T, s0SIDHID.T)
  class(s0NSList_altIDTRUE) <- c("networkset", "list")

  # >> S0Fill/Output << (includeAltID = FALSE)
  test_s0Fillw1.F <- matrix(c(0,1,1,10,10,10,
                        1,0,1,10,10,10,
                        1,0,0,10,10,10,
                        10,10,10,0,1,0,
                        10,10,10,0,0,1,
                        10,10,10,0,1,0),
                      nrow=6, ncol=6, byrow=TRUE)
  rownames(test_s0Fillw1.F) <- c("2","4","6","8","10","12")
  colnames(test_s0Fillw1.F) <- c("2","4","6","8","10","12")

  test_s0Fillw2.F <- matrix(c(0,0,1,10,10,10,
                        1,0,1,10,10,10,
                        0,0,0,10,10,10,
                        10,10,10,0,1,1,
                        10,10,10,1,0,1,
                        10,10,10,0,1,0),
                      nrow=6, ncol=6, byrow=TRUE)
  rownames(test_s0Fillw2.F) <- c("2","4","6","8","10","12")
  colnames(test_s0Fillw2.F) <- c("2","4","6","8","10","12")


  # >> s0Fill/Output <<  (includAltID = TRUE (for SID 14))
  test_s0Fillw1.T <- matrix(c(0,1,1,10,10,10,0,
                            1,0,1,10,10,10,0,
                            1,0,0,10,10,10,0,
                            10,10,10,0,1,0,0,
                            10,10,10,0,0,1,0,
                            10,10,10,0,1,0,0,
                            rep(NA, 7)),
                            nrow=7, ncol=7, byrow=TRUE)
  rownames(test_s0Fillw1.T) <- c("2","4","6","8","10","12","14")
  colnames(test_s0Fillw1.T) <- c("2","4","6","8","10","12","14")

  test_s0Fillw2.T <- matrix(c(0,1,0,10,10,10,10,
                            1,0,1,10,10,10,10,
                            1,1,0,10,10,10,10,
                            10,10,10,0,0,1,0,
                            10,10,10,0,0,0,1,
                            10,10,10,0,0,0,1,
                            10,10,10,rep(NA,4)),
                            nrow=7, ncol=7, byrow=TRUE)
  rownames(test_s0Fillw2.T) <- c("2","4","6","8","10","12","14")
  colnames(test_s0Fillw2.T) <- c("2","4","6","8","10","12","14")

  #-----------------------------------------------------------------------
  attribDF<- data.frame(SID=c(2,4,6,8,10,12),
                        X  =c(3,5,3,1,2,2))
  attribTB<- tibble(SID=c(2,4,6,8,10,12),
                    X  =c(3,5,3,1,2,2))
  #-----------------------------------------------------------------------
  # 6 & 8 are both ego/alt in h10 but only alt in h20
  # (this is the typical multihouse scenario because an individual
  # should only ever survey in one house, even though they could
  # have been included in several house rosters, e.g. if they changed
  # project houses)
  ttblaa <- tibble(SID =   c(2,2,4,4,6,8,8,
                             10,10,10,12,12,14,14),
                   HID =   c(10,10,10,10,10,10,10,
                             20,20,20,20,20,20,20),
                   WID =   c(rep(1,14)),
                   AltID = c(4,6,2,8,4,2,4,
                             14,6,8,10,6,10,8),
                   friend =c(5,3,2,5,3,1,2,
                             4,1,2,2,4,1,1))
  # 6 is ego in both, alt in 10 (anomalous);
  # 8 is ego/alt in 10, only alt in h20 (OK)
  ttblab <- tibble(SID =   c(2,2,4,4,6,8,8,
                             10,10,10,12,12,14,14,6,6),
                   HID =   c(10,10,10,10,10,10,10,
                             20,20,20,20,20,20,20,20,20),
                   WID =   c(rep(1,16)),
                   AltID = c(4,6,2,8,4,2,4,
                             14,6,8,10,6,10,8,10,12),
                   friend =c(5,3,2,5,3,1,2,
                             4,1,2,2,4,1,1,2,2))
  # 6 is ego in h10 but not alter; error, but not a dupe error!
  # 8 is ego in h10, alter in h10 and h20 (OK)
  ttblba <- tibble(SID =   c(2,4,4,6,8,8,
                             10,10,12,14,14),
                   HID =   c(10,10,10,10,10,10,
                             20,20,20,20,20),
                   WID =   c(rep(1,11)),
                   AltID = c(4,2,8,4,2,4,
                             14,8,10,10,8),
                   friend =c(5,3,2,5,3,1,
                             4,1,2,2,4))

  outputaa <- tibble(SID=c(6,6,8,8), HID=c(10,20,10,20),
                     ego=c(1,0,1,0), alt=c(1,1,1,1))
  outputab <- tibble(SID=c(6,6,8,8), HID=c(10,20,10,20),
                     ego=c(1,1,1,0), alt=c(1,1,1,1))
  outputba <- tibble(SID=c(8,8), HID=c(10,20),
                     ego=c(1,0), alt=c(1,1))
  #-----------------------------------------------------------------------
  tblWithSS <- tibble(SID=c(2,2,4,6,8), HID=c(10,10,10,10,10),
                      WID=c(1,1,1,1,1), AltID=c(4,2,2,8,4),
                      friend=c(2,1,4,5,4))
  outputSS <- tibble(SID=2, HID=10, WID=1, AltID=2, friend=1)
  #-----------------------------------------------------------------------
  tblDupe <- tibble(SID=c(2,2,4,6,8), HID=c(10,10,10,10,10),
                    WID=c(1,1,1,1,1), AltID=c(4,4,2,8,4),
                    friend=c(2,1,4,5,4))
  x<-2
  nInt <-as.integer(2) #Ugly kludge to make n an integer...
  outputDupe <- tibble(SID=2, HID=10, AltID=4, n=nInt)
  #-----------------------------------------------------------------------
  tblNoA <- tibble(SID   =c(2,4,6,8),
                   HID   =c(10,10,10,10),
                   WID   =c(1,1,1,1),
                   AltID =c(6,2,8,6),
                   friend=c(2,1,4,5))
  outputNoA <- tibble(SID=4, HID=10, WID=1)
  #-----------------------------------------------------------------------
  tblHof1 <- tibble(SID   =c(2,2,4,6,8,
                             10),
                    HID   =c(10,10,10,10,10,
                             20),
                    WID   =c(1,1,1,1,1,
                             1),
                    AltID =c(6,4,2,8,6,
                             12),
                    friend=c(2,1,4,5,2,
                             4))
  outputHof1 <- tibble(WID=1, HID=20)
  #-----------------------------------------------------------------------
  # Remove SID 2 from house 10, SID 8 from house 20
  rmvSIDHID <- tibble(SID=c(2,8),
                      HID=c(10,20))
  # function will return these remaining rows:
  outputFix <- tibble(SID=c(4,6,10,12),
                      HID=c(10,10,20,20),
                      WID=c(1,1,1,1),
                      AltID=c(6,4,12,10),
                      friend=c(5,1,3,5))
  #-----------------------------------------------------------------------
  # Error: table is not tbl_df
  errTbl1 <- data.table(SID =   c(2,2,4,4,6,6,8,8,10,10,12,12),
                        HID =   c(10,10,10,10,10,10,20,20,20,20,20,20),
                        WID =   c(rep(1,12)),
                        AltID = c(4,6,2,6,2,4,10,12,8,12,8,10),
                        friend =c(5,3,2,5,3,1,4,1,1,3,1,5))
  # Error: table is not tbl_df
  errTbl2 <- data.frame(SID =   c(2,2,4,4,6,6,8,8,10,10,12,12),
                        HID =   c(10,10,10,10,10,10,20,20,20,20,20,20),
                        WID =   c(rep(1,12)),
                        AltID = c(4,6,2,6,2,4,10,12,8,12,8,10),
                        friend =c(5,3,2,5,3,1,4,1,1,3,1,5))
  # Error: WID is missing
  errTbl3 <- tibble(SID =   c(2,2,4,4,6,6,8,8,10,10,12,12),
                    HID =   c(10,10,10,10,10,10,20,20,20,20,20,20),
                    AltID = c(4,6,2,6,2,4,10,12,8,12,8,10),
                    friend =c(5,3,2,5,3,1,4,1,1,3,1,5))
  # Error: none of the relationship cols (friend, loan, etc.) are present
  errTbl4 <- tibble(SID =   c(2,2,4,4,6,6,8,8,10,10,12,12),
                    HID =   c(10,10,10,10,10,10,20,20,20,20,20,20),
                    WID =   c(rep(1,12)),
                    AltID = c(4,6,2,6,2,4,10,12,8,12,8,10),
                    freund =c(5,3,2,5,3,1,4,1,1,3,1,5))
  # OK input; error will be something else
  okTbl <- tibble(SID =   c(2,2,4,4,6,6,8,8,10,10,12,12),
                  HID =   c(10,10,10,10,10,10,20,20,20,20,20,20),
                  WID =   c(rep(1,12)),
                  AltID = c(4,6,2,6,2,4,10,12,8,12,8,10),
                  friend =c(5,3,2,5,3,1,4,1,1,3,1,5))
  # Calling function (tests that sys.call operates as expected)
  myF <- function(rlpNet, pWave){
    errChecks(rlpNet, pWave)
  }
  #-----------------------------------------------------------------------




