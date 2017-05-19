context("stats")

test_that("basic stat functions", {
            expect_equal(stats_default(iris),
                         list("N" = length,
                              "Mean/%" = is.factor ~ percent | (is.normal ~ mean),
                              "sd" = is.normal ~ sd,
                              "Med" = is.normal ~ NA | median,
                              "IQR" = is.factor ~ NA | (is.normal ~ NA | IQR))
                         )
            expect_equal(stats_normal(iris),
  list("N" = length,
       "Mean/%" = is.factor ~ percent | mean,
       "sd" = stats::sd)
)
            expect_equal(stats_nonnormal(iris),
  list("N" = length,
       "Median/%" = is.factor ~ percent | median,
       "IQR" = is.factor ~ NA | IQR)
)
})

test_that("stats_auto works",
          {
            expect_equal(stats_auto(iris),
                         list(N = length,
                              `Mean/%` = is.factor ~ percent | (is.normal ~ mean),
                              sd = is.normal ~ sd,
                              Med = is.normal ~ NA | median,
                              IQR = is.factor ~ NA | (is.normal ~ NA | IQR))
                         )
            expect_equal(stats_auto(mtcars),
                         list(N = length,
                              Mean = is.normal ~ mean,
                              sd = is.normal ~ sd,
                              Med = is.normal ~ NA | median,
                              IQR = is.normal ~ NA | IQR)
                         )
          })
