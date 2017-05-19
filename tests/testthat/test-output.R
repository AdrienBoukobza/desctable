context("output")

test_that("flatten desctable returns a dataframe",
          {
            expect_is(print(desctable(iris)), "data.frame")
            expect_is(as.data.frame(desctable(iris)), "data.frame")
            expect_is(flatten_desctable(desctable(iris)), "data.frame")
          })
