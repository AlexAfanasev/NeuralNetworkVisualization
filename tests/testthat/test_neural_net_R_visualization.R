test_that("Plotting with numerical dependent variable works", {
    library(MASS)
    data <- Boston
    index <- sample(1:nrow(data), round(0.75*nrow(data)))
    train <- data[index,]

    set.seed(1)
    model <- NeuralNetwork(medv ~ ., data = train, layers = c(5, 3),
                           scale = TRUE, linear.output = TRUE)

    expect_error(plot_partial_dependencies(model), NA)
    expect_error(plot_partial_dependencies(model, predictors = "crim"), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("crim", "age")), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("crim", "all")))
    expect_error(plot_partial_dependencies(
        model, predictors = c("crim", "not_there")))
    expect_error(plot_partial_dependencies(model, predictors = "not_there"))
})

test_that("Plotting with categorical dependent variable works", {
    library(datasets)
    data("iris")
    iris$setosa <- iris$Species=="setosa"
    iris$setosa <- iris$setosa + 0
    iris$versicolor <- iris$Species == "versicolor"
    iris$versicolor <- iris$versicolor + 0
    iris$virginica <- iris$Species == "virginica"
    iris$virginica <- iris$virginica + 0
    index <- sample(x = nrow(iris), size = nrow(iris)*0.5)
    train_test <- iris[index,]
    train_model <- train_test[, !(names(train_test) %in%
                                      c("setosa", "versicolor", "virginica"))]
    set.seed(1)
    model <- NeuralNetwork(
        Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        data = train_model, layers = c(10, 10), rep = 5, err.fct = "ce",
        linear.output = FALSE, lifesign = "minimal", stepmax = 1000000,
        threshold = 0.001)

    expect_error(plot_partial_dependencies(model), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = "Sepal.Length"), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("Sepal.Length", "Petal.Length")), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("Sepal.Length", "all")))
    expect_error(plot_partial_dependencies(
        model, predictors = c("Sepal.Length", "not_there")))
    expect_error(plot_partial_dependencies(model, predictors = "not_there"))
})

test_that("Plotting with binary dependent variable works", {
    library(faraway)
    library(DMwR)
    pima$glucose[pima$glucose == 0] <- NA
    pima$diastolic[pima$diastolic == 0] <- NA
    pima$triceps[pima$triceps == 0] <- NA
    pima$insulin[pima$insulin == 0] <- NA
    pima$bmi[pima$bmi == 0] <- NA
    pima <- pima[-manyNAs(pima),]
    pima <- knnImputation(pima, k = 10)
    pima$test <- as.factor(pima$test)
    levels(pima$test) <- c("Negative", "Positive")
    pima[, 9] <- unclass(pima[, 9])
    pima_size <- floor(0.75 * nrow(pima))
    index <- sample(seq_len(nrow(pima)), size = pima_size)
    train <- pima[index, ]

    set.seed(1)
    model <- NeuralNetwork(test ~ pregnant + glucose + diastolic + triceps +
                               insulin + bmi + diabetes + age,
                           data = train, layers = 4,
                           scale = TRUE, linear.output = TRUE)

    expect_error(plot_partial_dependencies(model), NA)
    expect_error(plot_partial_dependencies(model, predictors = "glucose"), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("pregnant", "diastolic")), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("pregnant", "all")))
    expect_error(plot_partial_dependencies(
        model, predictors = c("pregnant", "not_there")))
    expect_error(plot_partial_dependencies(model, predictors = "not_there"))
})