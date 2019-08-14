test_that("Data is correctly prepared for plotting with numerical
          dependent variable", {
    library(MASS)
    data <- Boston
    index <- sample(1:nrow(data), round(0.75*nrow(data)))
    train <- data[index,]

    set.seed(1)
    model <- NeuralNetwork(medv ~ ., data = train, layers = c(5, 3),
                           scale = TRUE, linear.output = TRUE)

    for (predictor in syms(model$neural_network$model.list$variables)) {
        result_data <- prepare_data(model, predictor, nrepetitions = 5)
        expect_equal(any(is.na(result_data$yhat)), FALSE)
        expect_equal(any(is.na(result_data$lwr)), FALSE)
        expect_equal(any(is.na(result_data$upr)), FALSE)
        expect_equal(all(result_data$lwr <= result_data$yhat), TRUE)
        expect_equal(all(result_data$upr >= result_data$yhat), TRUE)
    }
})

test_that("Data is correctly prepared for plotting with categorical
          dependent variable", {
    library(datasets)
    data("iris")
    index <- sample(x = nrow(iris), size = nrow(iris)*0.5)
    train <- iris[index,]

    set.seed(1)
    model <- NeuralNetwork(
      Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
      data = train, layers = c(10, 10), rep = 5, err.fct = "ce",
      linear.output = FALSE, lifesign = "minimal", stepmax = 1000000,
      threshold = 0.001)

    for (predictor in syms(model$neural_network$model.list$variables)) {
        result_data <- prepare_data(model, predictor, nrepetitions = 5)
        expect_equal(any(is.na(result_data$yhat)), FALSE)
        expect_equal(any(is.na(result_data$lwr)), FALSE)
        expect_equal(any(is.na(result_data$upr)), FALSE)
        expect_equal(all(result_data$lwr <= result_data$yhat), TRUE)
        expect_equal(all(result_data$upr >= result_data$yhat), TRUE)
    }
})

test_that("Data is correctly prepared for plotting with binary
          dependent variable", {
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

    for (predictor in syms(model$neural_network$model.list$variables)) {
        result_data <- prepare_data(model, predictor, nrepetitions = 5)
        expect_equal(any(is.na(result_data$yhat)), FALSE)
        expect_equal(any(is.na(result_data$lwr)), FALSE)
        expect_equal(any(is.na(result_data$upr)), FALSE)
        expect_equal(all(result_data$lwr <= result_data$yhat), TRUE)
        expect_equal(all(result_data$upr >= result_data$yhat), TRUE)
    }
})
