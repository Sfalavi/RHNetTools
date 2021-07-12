context("Tests Descriptive functions")

test_that("makeSurveyRateTblByHouseByWave works", {
  expect_equivalent(makeSurveyedRateTblByHouseByWave(ttbl, ttbl),
               missingXHID)
})
