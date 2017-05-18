context("convenience_functions")

test_that("percent works on a factor",
          {
            expect_equal(percent(iris$Species), c(NA, setosa = 100/3, versicolor = 100/3, virginica = 100/3))
          })

test_that("percent returns NA on non-factors",
          {
            expect_equal(percent(1:4), NA)
          })

test_that("IQR works with numerical values",
          {
            expect_equal(IQR(1:10), stats::IQR(1:10, na.rm = T))
            expect_equal(IQR(c(NA, 1:10)), stats::IQR(c(NA, 1:10), na.rm = T))
          })

test_that("IQR throws an error for non-numeric input",
          {
            expect_error(IQR(letters))
          })

test_that("Formula versions of fisher.test and chisq.test works",
          {
            expect_equal(fisher.test(iris$Species ~ iris$Petal.Length > 5)$p.value, stats::fisher.test(iris$Species, iris$Sepal.Length > 5)$p.value)
            expect_equal(chisq.test(iris$Species ~ iris$Petal.Length > 5)$p.value, stats::chisq.test(iris$Species, iris$Sepal.Length > 5)$p.value)
          })

test_that("no.test returns NA",
          {
            expect_equal(no.test(a ~ b),
                         data.frame(p.value = NA))
          })
