context("parse_formula")

test_that("simple condition works",
          {
            expect_equal(parse_formula(1:10, is.numeric ~ T | F), T)
            expect_equal(parse_formula(LETTERS[1:3], is.numeric ~ T | F), F)
          })

test_that("condition with no F returns NA",
          {
            expect_equal(parse_formula(1:10, is.factor ~ T), NULL)
          })

test_that("nested conditions work",
          {
            expect_equal(parse_formula(1:10, is.numeric ~ (is.normal ~ mean | median) | F), median)
          })
