context("build")

test_that("statColumn returns correct values",
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

test_that("Simple call to desctable works",
          {
            expect_equal(desctable(iris, stats = list("N" = length, "Mean" = mean, "SD" = sd)),
                         structure(list(Variables = data.frame(Variables = c("Sepal.Length",
                                                                             "Sepal.Width",
                                                                             "Petal.Length",
                                                                             "Petal.Width",
                                                                             "**Species**",
                                                                             "**Species**: *setosa*",
                                                                             "**Species**: *versicolor*",
                                                                             "**Species**: *virginica*"),
                                                               stringsAsFactors = F,
                                                               check.names = F),
                                        stats = data.frame(N = c(150, 150, 150, 150, 150, 50, 50, 50),
                                                           Mean = c(5.84333333333333, 3.05733333333333, 3.758, 1.19933333333333, NA, NA, NA, NA),
                                                           SD = c(0.828066127977863, 0.435866284936698, 1.76529823325947, 0.762237668960347, 0.81923192051904, 0, 0, 0),
                                                           stringsAsFactors = F,
                                                           check.names = F)),
                                        class = "desctable"))
          })

test_that("stats argument works with lambdas",
          {
            expect_equal(desctable(iris, stats = list("N" = length, "Sum" = sum, "Mean" = function(x){sum(x) / length(x)})),
                         structure(list(Variables = data.frame(Variables = c("Sepal.Length",
                                                                             "Sepal.Width",
                                                                             "Petal.Length",
                                                                             "Petal.Width",
                                                                             "**Species**",
                                                                             "**Species**: *setosa*",
                                                                             "**Species**: *versicolor*",
                                                                             "**Species**: *virginica*"),
                                                               stringsAsFactors = F,
                                                               check.names = F),
                                        stats = data.frame(N = c(150, 150, 150, 150, 150, 50, 50, 50),
                                                           Sum = c(876.5, 458.6, 563.7, 179.9, NA, NA, NA, NA),
                                                           Mean = c(5.843333333333333, 3.05733333333333, 3.758, 1.19933333333333, NA, NA, NA, NA),
                                                           stringsAsFactors = F,
                                                           check.names = F)), class = "desctable"))
          })

test_that("table with one var works",
          {
            expect_equal(desctable(data.frame(a = 1:100), stats = list("N" = length, "Med" = median, "IQR" = IQR)),
                         structure(list(Variables = data.frame(Variables = "a",
                                                               stringsAsFactors = F),
                                        stats = data.frame(N = 100,
                                                           Med = 50.5,
                                                           IQR = 49.5)),
                                   class = "desctable")
                         )
          })

test_that("subNames returns the correct names",
          {
            expect_equal(subNames(as.symbol("Species"), iris),
                         c("Species: setosa (n=50)", "Species: versicolor (n=50)", "Species: virginica (n=50)")
                         )
          })

test_that("Grouped call to desctable works",
          {
            expect_equal(desctable(group_by(iris, Species), stats= list("N" = length, "Med" = median, "IQR" = IQR), tests = list(.default = ~kruskal.test, Sepal.Length = ~oneway.test)),
                         structure(list(Variables = data.frame(Variables = c("Sepal.Length",
                                                                             "Sepal.Width",
                                                                             "Petal.Length",
                                                                             "Petal.Width"),
                                                               stringsAsFactors = F),
                                        `Species: setosa (n=50)` = data.frame(N = c(50, 50, 50, 50),
                                                                              Med = c(5, 3.4, 1.5, 0.2),
                                                                              IQR = c(0.4, 0.475, 0.175, 0.1)),
                                        `Species: versicolor (n=50)` = data.frame(N = c(50, 50, 50, 50),
                                                                                  Med = c(5.9, 2.8, 4.35, 1.3),
                                                                                  IQR = c(0.7, 0.475, 0.6, 0.3)),
                                        `Species: virginica (n=50)` = data.frame(N = c(50, 50, 50, 50),
                                                                                 Med = c(6.5, 3, 5.55, 2),
                                                                                 IQR = c(0.675, 0.375, 0.775000000000001, 0.5)),
                                        tests = data.frame(p = c(1.5050589627451e-28, 1.56928209403159e-14, 4.80397359115759e-29, 3.26179555242197e-29),
                                                           test = c("oneway.test", "kruskal.test", "kruskal.test", "kruskal.test"),
                                                           stringsAsFactors = F)),
                                   class = "desctable"))
          })
