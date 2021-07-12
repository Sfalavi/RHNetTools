context("Tests Net Creation Functions")

test_that("longTB Class OK, Spot-Ck vars, See if populated", {

  #Is output the right class?
  expect_equal(class(makeLongNet(wideNetTB.w1)),
               c("tbl_df", "tbl", "data.frame"))
  #Spot check columns
  expect_equal(names(makeLongNet(wideNetTB.w1))[1:6],
               c("SID", "HID", "WID", "AltID", "friend", "loan"))
  #Make sure it's not empty
  expect_true(dim(makeLongNet(wideNetTB.w1))[1] > 100
              &
              dim(makeLongNet(wideNetTB.w1))[1] >= 5)
})

test_that("getNet4OneRlp returns a correct tbl", {
  expect_true("tbl_df" %in% class(getNet4OneRlp(ln3, c(1,2,3), c("loan"))))
  expect_equal(0, 1045-dim(getNet4OneRlp(ln3, c(1,2,3), c("friend")))[1])
})

test_that("makeAdjMatrix returns correct matrix or network with includeAltID=F", {
  expect_true("matrix" %in% class(makeAdjMatrix(ttbla, pWave = c(1),
                                                includeAltID = FALSE)))
  expect_equal(dim(makeAdjMatrix(ttbla, pWave=c(1),
                                                includeAltID=FALSE))[1], 6)
  expect_true(all(colnames(makeAdjMatrix(testTbl1, pWave = 1,
                                                includeAltID=FALSE))==
               c("2","4","6","8","10","12")))
  expect_equal(makeAdjMatrix(ttbla, pWave = c(1), includeAltID = FALSE),
               testAdj_includeAltIDFALSE)
  expect_error(makeAdjMatrix(testTbl2, pWave=2,includeAltID=FALSE),
                 "makeAdjMatrix: input has AltIDs")
})

test_that("makeAdjMatrix returns correct matrix or network with includeAltID=T", {
  expect_equal(dim(makeAdjMatrix(ttbl, pWave=2,
                                 includeAltID=TRUE))[1], 7)
  expect_true(all(colnames(makeAdjMatrix(ttbl, pWave=2,
                                         includeAltID=TRUE))==
                    c("2","4","6","8","10","12", "14")))
#  expect_equal(makeAdjMatrix(ttbl, pWave = 2, includeAltID = TRUE),
#               testAdj_includeAltIDTRUE)
})

test_that("makeSIDHIDLookups gives correct results", {
  # Class of output components
  expect_true("list" %in% class(makeSIDHIDLookups(testLst,
                                                 includeAltID=FALSE)))
  expect_true("tbl_df" %in% class(makeSIDHIDLookups(testLst,
                                                 includeAltID=FALSE)[[1]]))
  expect_true("tbl_df" %in% class(makeSIDHIDLookups(testLst,
                                                 includeAltID=FALSE)[[2]]))
  # Size of output components
  expect_equal(c(6,2), dim(makeSIDHIDLookups(testLst,
                                            includeAltID=FALSE)[[1]]))
  expect_equal(c(6,2), dim(makeSIDHIDLookups(testLst,
                                            includeAltID=FALSE)[[2]]))
  expect_equal(c(7,2), dim(makeSIDHIDLookups(testLst,
                                            includeAltID=TRUE)[[2]]))
  # IDs included by wave and includeAltID=T/F
  expect_true (setequal(testTbl1$SID,
                        makeSIDHIDLookups(testLst, includeAltID=FALSE)[[1]]$SID))
  expect_true (setequal(testTbl2$SID,
                        makeSIDHIDLookups(testLst, includeAltID=FALSE)[[2]]$SID))
  expect_true (setequal(testTbl1$SID,
                        makeSIDHIDLookups(testLst, includeAltID=TRUE)[[1]]$SID))
  expect_false(setequal(testTbl2$SID,
                        makeSIDHIDLookups(testLst, includeAltID=TRUE)[[2]]$SID))
  expect_output(makeSIDHIDLookups(testLst, includeAltID=TRUE),
                 "Warning: AltIDs included in SIDHID lookup.")
})

test_that("makeNetworkSet gives correct results",{
  # Class & elements of output
  expect_true("list" %in% class(makeNetworkSet(ln3, c(1,3),
                              c("FR"), c("MX"), pTHold = 2,
                              includeAltID = FALSE)))
  expect_true("tbl_df" %in% class(makeNetworkSet(ln3, c(1,3),
                              c("FR"),c("MX"), pTHold = 2,
                              includeAltID = FALSE)[[3]]))
  expect_equal(3, length(makeNetworkSet(ln3, c(1,3),
                              c("FR"),c("MX"), pTHold = 2,
                              includeAltID = FALSE)))
  # Dimension of matrices
  expect_equal(c(141,141), dim(makeNetworkSet(ln3, c(1,3),
                              c("FR"), c("MX"), pTHold = 2,
                              includeAltID = FALSE)[[1]]))
  # Spot check matrices' 1st 5 and last 5 column labels
  expect_equal(c("1001", "1002", "1003", "1005", "1006"),
               colnames(makeNetworkSet(ln3, c(1,3),
                              c("FR"), c("MX"), pTHold = 2,
                              includeAltID = FALSE)[[1]])[1:5])
  expect_equal(c("5087", "5091", "5092", "5093", "5094"),
               colnames(makeNetworkSet(ln3, c(1,3),
                              c("FR"), c("MX"),pTHold = 2,
                              includeAltID = FALSE)[[1]])[137:141])
  # Density (# of 1's) per wave
  expect_equal(138, sum(makeNetworkSet(ttbl, c(1,2),
                                       c("FR"), c("MX"),pTHold = 2,
                                       includeAltID = TRUE)[[1]]))
  expect_equal(8, sum(makeNetworkSet(ttbla, c(1,2),
                              c("FR"), c("MX"), pTHold = 2,
                              includeAltID = FALSE)[[2]]))
})

test_that("s0Fill works correctly",{
  # if includeAltID was false for the network set (s0NSList)
  expect_equal(test_s0Fillw1.F,
               s0Fill(s0NSList_altIDFALSE)[[1]])
  expect_equal(test_s0Fillw2.F,
               s0Fill(s0NSList_altIDFALSE)[[2]])
  # if includeAltID was true for the network set
  expect_equal(test_s0Fillw1.T,
               s0Fill(s0NSList_altIDTRUE)[[1]])
  expect_equal(test_s0Fillw2.T,
               s0Fill(s0NSList_altIDTRUE)[[2]])
})

test_that("makeListOfNetsByHouse gives correct results",{
  expect_equal(2, length(makeListOfNetsByHouse(ttbl, 1, pTypNet = "FR",
                            pTypOut="MX", pTHold = 2,
                            includeAltID=FALSE)))
  expect_true("matrix" %in%
                class(makeListOfNetsByHouse(ttbl, 1, pTypNet = "FR",
                            pTypOut="MX", pTHold = 2,
                            includeAltID=FALSE)[[1]]))
  expect_true("matrix" %in%
                class(makeListOfNetsByHouse(ttbl, 1, pTypNet = "FR",
                            pTypOut="MX", pTHold = 2,
                            includeAltID=FALSE)[[2]]))
  expect_equal(4, dim(makeListOfNetsByHouse(ttbl, 2, pTypNet = "FR",
                                         pTypOut="MX", pTHold = 2,
                                         includeAltID=TRUE)[[2]])[1])
  # Ensures pTHold works as expected, output matrices have
  # correct density
  expect_equal(5, sum(makeListOfNetsByHouse(ttbl, 1, pTypNet = "FR",
                                            pTypOut="MX", pTHold = 2,
                                            includeAltID=FALSE)[[1]]))
  expect_equal(4, sum(makeListOfNetsByHouse(ttbl, 1, pTypNet = "FR",
                            pTypOut="MX", pTHold = 3,
                            includeAltID=FALSE)[[1]]))
#  expect_equal(4, sum(makeListOfNetsByHouse(ttbl, 2, pTypNet = "FR",
#                                            pTypOut="MX", pTHold = 2,
#                                            includeAltID=TRUE)[[2]], na.rm=T))

})

test_that("selectHouses gives correct results",{
  expect_true(all(selectHouses(ttbl, c(10))$HID == 10))
  expect_true(all(selectHouses(ttbl, c(-10))$HID == 20))
  expect_output(selectHouses(ttbl, c(30)),
                "Warning: some HIDs passed to selectHouses")
  expect_output(selectHouses(ttbl, c(-10, 30)),
                "Warning: HIDs must be all >0 or all <0;")
})

test_that("netVtxAttr gives correct results",{
  expect_equal(3, dim(netVtxAttr(attribDF, c(2,4,6)))[1])
  expect_true("data.frame" %in% class(netVtxAttr(attribDF, c(2,4,6))))
  expect_true("tbl_df" %in% class(netVtxAttr(attribTB, c(2,4,6))))
  expect_equal("X", names(netVtxAttr(attribTB, c(2,4,6)))[2])
})

test_that("checkInMultipleHouses gives correct results",{

  expect_equal(outputaa, checkInMultipleHouses(ttblaa))
  expect_equal(outputab, checkInMultipleHouses(ttblab))
  expect_equal(outputba, checkInMultipleHouses(ttblba))
})

test_that("checkForSelfSelection works correctly",{
  expect_equal(outputSS, checkForSelfSelection(tblWithSS))
})

test_that("checkForDuplicateEdges works correctly",{
  expect_equal(outputDupe, checkForDuplicateEdges(tblDupe) %>% ungroup())
})

test_that("checkForNoAlters works correctly",{
  expect_equal(outputNoA, checkForNoAlters(tblNoA))
})

test_that("checkForHousesOf1 works correctly",{
  expect_equal(outputHof1, checkForHousesOf1(tblHof1) %>% ungroup())
})

test_that("runConChecks works correctly",{
  expect_equal(4, length(runConChecks(tblNoA, pWave=1)))
  expect_output(runConChecks(ttblab, pWave=1),
                "w1: Multiple house membership detected;")
  expect_output(runConChecks(tblDupe, pWave=1),
                "w1: Duplicate edges detected;")
  expect_output(runConChecks(tblWithSS, pWave=1),
                "w1: Self-loops detected;")
  expect_output(runConChecks(tblNoA, pWave=1),
                "w1: Egos chosen by no alters detected;")
  expect_output(runConChecks(testTbl1, pWave=1),
                "w1: No inconsistencies detected;")
})

test_that("fixHouse works correctly",{

  expect_equal(outputFix, fixHouse(testTbl1, pWave=1, rmvSIDHID ))
})

test_that("errChecks works correctly",{
    expect_error(errChecks(errTbl1, pWave=1),
                "input network object must be class tbl_df")
  expect_error(errChecks(errTbl2, pWave=1),
               "input network object must be class tbl_df")
  expect_error(errChecks(errTbl3, pWave=1),
               "requires SID HID WID AltID as 1st 4 input cols.")
  expect_error(errChecks(errTbl4, pWave=1),
               "requires friend, loan, help, conv, advc, or rate")
  expect_silent(errChecks(okTbl, pWave=1))
})
