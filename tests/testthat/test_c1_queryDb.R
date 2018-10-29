context("Occ Data")
library(stringr)
source("test_helper_functions.R")

occNum <- 1000
out.gbif <- c1_queryDb(spName = "Meles meles", occDb = "gbif", occNum = 1000)
out.gbif.latlon <- out.gbif$orig[!is.na(out.gbif$orig$latitude) & !is.na(out.gbif$orig$longitude),]
out.gbif.dups <- out.gbif.latlon[!duplicated(out.gbif.latlon[,c('longitude','latitude')]),]

test_that("general checks on occurrence data", {
  expect_equal(occNum, nrow(out.gbif$orig))
  expect_equal(nrow(out.gbif.dups), nrow(out.gbif$cleaned))
})