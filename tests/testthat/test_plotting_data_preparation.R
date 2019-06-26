test_that("Data is correctly prepared for plotting with numerical
          dependent variable", {
    library(MASS)
    library(tidyverse)
    data <- Boston
    index <- sample(1:nrow(data), round(0.75*nrow(data)))
    train <- data[index,]
    maxs <- apply(train, 2, max)
    mins <- apply(train, 2, min)
    scaled <- as.data.frame(scale(train, center = mins, scale = maxs - mins))
    n <- names(train)
    f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

    set.seed(1)
    nn <- neuralnet(f, data = scaled, hidden = c(5,3), linear.output = T)

    set.seed(1)
    model <- NeuralNetwork(medv ~ ., data = train, layers = c(5, 3),
                         scale = TRUE, linear.output = TRUE)

    prepare_numeric_data <- function (predictor, train, neural_network) {
      grid_predictor <- dplyr::select(train, !!predictor)
      grid_input <- dplyr::select(train, -!!predictor)
      grid <- crossing(grid_predictor, grid_input)
      grid <- grid %>%
          mutate(pred = neuralnet::compute(neural_network,grid)$net.result)
      partial_dependence <- grid %>%
          group_by(!!predictor) %>%
          summarize(yhat = mean(pred))
      return(partial_dependence)
    }

    for (predictor in syms(model$neural_network$model.list$variables)) {
        result_data <- prepare_data(model, predictor)
        expected_data <- prepare_numeric_data(predictor, scaled, nn)
        expect_equal(result_data$yhat, expected_data$yhat)
        expect_equal(any(is.na(result_data$lwr)), FALSE)
        expect_equal(any(is.na(result_data$upr)), FALSE)
        expect_equal(all(result_data$lwr <= result_data$yhat), TRUE)
        expect_equal(all(result_data$upr >= result_data$yhat), TRUE)
    }
})

test_that("Data is correctly prepared for plotting with categorical
          dependent variable", {
    library(datasets)
    library(tidyverse)
    data("iris")
    iris$setosa <- iris$Species=="setosa"
    iris$setosa <- iris$setosa + 0
    iris$versicolor <- iris$Species == "versicolor"
    iris$versicolor <- iris$versicolor + 0
    iris$virginica <- iris$Species == "virginica"
    iris$virginica <- iris$virginica + 0
    index <- sample(x = nrow(iris), size = nrow(iris)*0.5)
    train_test <- iris[index,]

    set.seed(1)
    nn <- neuralnet(
      setosa + versicolor + virginica ~
          Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
      data = train_test, hidden = c(10, 10), rep = 5, err.fct = "ce",
      linear.output = FALSE, lifesign = "minimal", stepmax = 1000000,
      threshold = 0.001)

    train_model <- train_test[, !(colnames(train_test) %in%
                                    c("setosa", "versicolor", "virginica"))]

    set.seed(1)
    model <- NeuralNetwork(
      Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
      data = train_model, layers = c(10, 10), rep = 5, err.fct = "ce",
      linear.output = FALSE, lifesign = "minimal", stepmax = 1000000,
      threshold = 0.001)

    prepare_categorical_data <- function (predictor, train, nn, class) {
      grid_predictor <- dplyr::select(train, !!predictor)
      grid_input <- dplyr::select(train, -!!predictor)
      grid <- crossing(grid_predictor, grid_input)
      pred <- as.data.frame(neuralnet::compute(nn, grid)$net.result)
      names(pred) <- paste(nn$model.list$response, "_prediction",
                           sep = "")
      grid <- grid %>% bind_cols(pred)
      pd <- grid %>%
          gather(class, pred, ends_with("prediction")) %>%
          mutate(class = str_replace(class, "_prediction","")) %>%
          group_by(class, !!predictor) %>%
          summarize(yhat = mean(pred))
      pd$class <- as.factor(pd$class)
      return(pd)
    }

    for (predictor in syms(model$neural_network$model.list$variables)) {
        result_data <- prepare_data(model, predictor)
        expected_data <- prepare_categorical_data(predictor, train_test,
                                                  nn, "Species")
        expect_equal(result_data$yhat, expected_data$yhat)
        expect_equal(any(is.na(result_data$lwr)), FALSE)
        expect_equal(any(is.na(result_data$upr)), FALSE)
        expect_equal(all(result_data$lwr <= result_data$yhat), TRUE)
        expect_equal(all(result_data$upr >= result_data$yhat), TRUE)
    }
})

test_that("Data is correctly prepared for plotting with binary
          dependent variable", {
    library(tidyverse)
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

    maxs <- apply(train, 2, max)
    mins <- apply(train, 2, min)
    scaled <- as.data.frame(scale(train, center = mins,
                                scale = maxs - mins))

    set.seed(1)
    nn <- neuralnet(test ~ pregnant + glucose + diastolic + triceps + insulin +
                      bmi + diabetes + age,
                  hidden = 4, data = scaled, linear.output = TRUE)

    set.seed(1)
    model <- NeuralNetwork(test ~ pregnant + glucose + diastolic + triceps +
                             insulin + bmi + diabetes + age,
                         data = train, layers = 4,
                         scale = TRUE, linear.output = TRUE)

    prepare_numeric_data <- function (predictor, train, neural_network) {
      grid_predictor <- dplyr::select(train, !!predictor)
      grid_input <- dplyr::select(train, -!!predictor)
      grid <- crossing(grid_predictor, grid_input)
      grid <- grid %>%
          mutate(pred = neuralnet::compute(neural_network,grid)$net.result)
      partial_dependence <- grid %>%
          group_by(!!predictor) %>%
          summarize(yhat = mean(pred))
      return(partial_dependence)
    }

    for (predictor in syms(model$neural_network$model.list$variables)) {
        result_data <- prepare_data(model, predictor)
        expected_data <- prepare_numeric_data(predictor, scaled, nn)
        expect_equal(result_data$yhat, expected_data$yhat)
        expect_equal(any(is.na(result_data$lwr)), FALSE)
        expect_equal(any(is.na(result_data$upr)), FALSE)
        expect_equal(all(result_data$lwr <= result_data$yhat), TRUE)
        expect_equal(all(result_data$upr >= result_data$yhat), TRUE)
    }
})
