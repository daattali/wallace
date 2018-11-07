context("Occ Data")
library(wallace)
source("test_helper_functions.R")

out.gbif <- c1_queryDb(spName = "Meles meles", occDb = "gbif", occNum = 1000)

test_that("error checks", {
  expect_error(c1_queryDb(spName = "", occDb = "gbif", occNum = 1000), 'Please input both genus and species names.')
  expect_error(c1_queryDb(spName = "Meles", occDb = "gbif", occNum = 1000), 'Please input both genus and species names.')
})

test_that("output type checks", {
  expect_is(out.gbif, "list")
  expect_equal(length(out.gbif), 2)
})

test_that("output data checks", {
  expect_equal(1000, nrow(out.gbif$orig))
})