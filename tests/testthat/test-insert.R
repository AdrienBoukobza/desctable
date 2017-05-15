context("insert")


test_that("inserting a vector",
          {
            expect_equal(insert(1:5, 3, 3), c(1, 2, 3, 3, 4, 5))
            expect_equal(insert(1:10, 2:3, 3), c(1, 2, 3, 2, 3, 4, 5, 6, 7, 8, 9, 10))
          })

test_that("inserting multiple vectors",
          {
            expect_equal(insert(1:5, list(2, 3:4), c(2,3)), c(1, 2, 2, 3, 3, 4, 4, 5))
          })

test_that("inserting at the end",
          {
            expect_equal(insert(1:5, 6, 5), 1:6)
            expect_equal(insert(1:5, 6, 15), 1:6)
          })

test_that("inserting empty vector or inside empty vector",
          {
            expect_equal(insert(1:5, numeric(0), 4), 1:5)
            expect_equal(insert(numeric(0), 1, 1), 1)
            expect_equal(insert(numeric(0), 1:2, 5), 1:2)
          })

test_that("mismatched number of vectors and positions returns an error",
          {
            expect_error(insert(1:5, list(6,7), 2))
            expect_error(insert(1:5, 6, c(2,3)))
          })
