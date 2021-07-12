context("Tests Utility Functions")

test_that("replaceNanWithNA works", {
  expect_identical(replaceNaNWithNA(tibble(a=c(1,2,NaN), b=c(NaN, 5,6))),
                   tibble(a=c(1,2,NA), b=c(NA,5,6)))
})
