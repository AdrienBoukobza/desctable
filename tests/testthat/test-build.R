context("build")

test_that("statColumn returns correct lengths",
          {
            expect_equal(statColumn(length, iris),
                         c(Sepal.Length = 150,
                           Sepal.Width = 150,
                           Petal.Length = 150,
                           Petal.Width = 150,
                           Species1 = 150,
                           Species2 = 50,
                           Species3 = 50,
                           Species4 = 50))
          })

test_that("varColumn returns correct names",
          {
            expect_equal(varColumn(iris),
                         data.frame(Variables = c("Sepal.Length",
                                                  "Sepal.Width",
                                                  "Petal.Length",
                                                  "Petal.Width",
                                                  "**Species**",
                                                  "**Species**: *setosa*",
                                                  "**Species**: *versicolor*",
                                                  "**Species**: *virginica*"),
                                    stringsAsFactors = F))
            expect_equal(varColumn(mtcars),
                         data.frame(Variables = c("mpg",
                                                  "cyl",
                                                  "disp",
                                                  "hp",
                                                  "drat",
                                                  "wt",
                                                  "qsec",
                                                  "vs",
                                                  "am",
                                                  "gear",
                                                  "carb"),
                                    stringsAsFactors = F))
            expect_equal(varColumn(mtcars, labels = c(mpg = "Miles per gallon", nothing = "Nothing")),
                         data.frame(Variables = c("Miles per gallon",
                                                  "cyl",
                                                  "disp",
                                                  "hp",
                                                  "drat",
                                                  "wt",
                                                  "qsec",
                                                  "vs",
                                                  "am",
                                                  "gear",
                                                  "carb"),
                                    stringsAsFactors = F))
          })
