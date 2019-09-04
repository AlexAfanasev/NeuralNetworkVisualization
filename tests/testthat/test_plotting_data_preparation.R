test_that("Data is correctly prepared for plotting with numerical
          dependent variable", {
    library(MASS)
    data <- Boston; data$chas <- as.factor(data$chas)

    set.seed(1)
    model <- NeuralNetwork(medv ~ ., data = data, layers = c(5, 3),
                           scale = TRUE, linear.output = TRUE, threshold = 0.5)

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
    train <- iris

    set.seed(1)
    model <- NeuralNetwork(
      Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
      data = train, layers = c(5, 5), rep = 5, err.fct = "ce",
      linear.output = FALSE, stepmax = 1000000, threshold = 0.001)

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
    pima$glucose[pima$glucose == 0] <- NA
    pima$diastolic[pima$diastolic == 0] <- NA
    pima$triceps[pima$triceps == 0] <- NA
    pima$insulin[pima$insulin == 0] <- NA
    pima$bmi[pima$bmi == 0] <- NA
    pima <- pima[complete.cases(pima), ]
    pima$test <- as.factor(pima$test)
    levels(pima$test) <- c("Negative", "Positive")
    train <- pima

    set.seed(1)
    model <- NeuralNetwork(test ~ pregnant + glucose + diastolic + triceps +
                             insulin + bmi + diabetes + age, data = train,
                           layers = 2, linear.output = FALSE, threshold = 0.5,
                           stepmax = 1e6)

    for (predictor in syms(model$neural_network$model.list$variables)) {
        result_data <- prepare_data(model, predictor, nrepetitions = 5)
        expect_equal(any(is.na(result_data$yhat)), FALSE)
        expect_equal(any(is.na(result_data$lwr)), FALSE)
        expect_equal(any(is.na(result_data$upr)), FALSE)
        expect_equal(all(result_data$lwr <= result_data$yhat), TRUE)
        expect_equal(all(result_data$upr >= result_data$yhat), TRUE)
    }
})
