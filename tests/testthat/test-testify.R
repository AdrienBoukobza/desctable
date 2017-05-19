context("testify")

test_that("testify works",
          {
            expect_equal(testify(iris$Petal.Length > 5, ~chisq.test, iris$Species),
                         data.frame(p = 2.70788859770093e-24,
                                    test = "chisq.test",
                                    stringsAsFactors = F)
                         )
            expect_equal(testify(iris$Species , ~chisq.test, iris$Petal.Length > 5),
                         data.frame(p = c(2.70788859770093e-24, NA, NA, NA),
                                    test = c("chisq.test", NA, NA, NA),
                                    stringsAsFactors = F)
                         )
          })

test_that("tests_auto works",
          {
            expect_equal(tests_auto(iris$Petal.Length, iris$Species),
                         ~kruskal.test)
            expect_equal(tests_auto(iris$Sepal.Length, iris$Species),
                         ~. %>% oneway.test(var.equal = F))
          })
