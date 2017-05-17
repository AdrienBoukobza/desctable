context("build")

test_that("statColumn returns correct lengths",
          {
            expect_equal(length(statColumn(length, iris)), 8)
          })

test_that("varColumn returns correct names",
          {
            expect_equal(varColumn(iris), data.frame(Variables = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "**Species**", "**Species**: *setosa*", "**Species**: *versicolor*", "**Species**: *virginica*)))
          })
