# Load necessary libraries
library(testthat)
library(lab5Package)  # Make sure to load your package

# Test cases for lat_long function

test_that("lat_long rejects errounous input", {
  expect_error(lat_long("1","error"))
})

test_that(" 'Lund' is character", {
  expect_equal(typeof('Lund'), "character")
})

test_that("lat_long() is not working", {
  expect_false(is.list("lund"))
})

test_that("lat_long rejects special characters as input", {
  expect_error(lat_long("%%%%%","My error!"))
})