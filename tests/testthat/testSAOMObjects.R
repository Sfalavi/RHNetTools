context("Tests RHSSAOMObjects code")

test_that("makeCCVec gives correct results",{
# Uses testTbl1, testTbl2, testLst from 'setup-testNetCreation.R'
  expect_equal(ccVecOutput, makeCCVec(testSIDInAnyWave))
})
