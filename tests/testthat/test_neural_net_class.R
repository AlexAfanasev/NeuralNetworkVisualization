test_that("Creating NeuralNetwork for numerical dependent variable works", {
    # load library for getting Boston data set
    library(MASS)
    data <- Boston; data$chas <- as.factor(data$chas)
    factor_matrix <- class.ind(data$chas); colnames(factor_matrix) <- c(
        "chas0", "chas1")
    train <- cbind(data[, -which(colnames(data) == "chas")],
                   factor_matrix)

    # scale data set
    scale_column <- function(column){
        if (!(is.numeric(column))) {
            return(column)
        } else {
            min_col <- min(column)
            max_col <- max(column)
            return(scale(column, center = min_col, scale = max_col - min_col))
        }
    }

    numeric_columns <- sapply(train, is.numeric)
    scaled <- as.data.frame(lapply(train, scale_column))
    maxs <- apply(train[, numeric_columns], 2, max)
    mins <- apply(train[, numeric_columns], 2, min)

    # create formula for fitting expected model
    n <- names(train)
    f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

    # fit expected model
    set.seed(1)
    nn <- neuralnet(f, data = scaled, hidden = c(5, 3), linear.output = TRUE,
                    threshold = 0.5)

    # fit test model
    set.seed(1)
    model_options <- list(store = TRUE, parallel = TRUE, probs = c(0.05, 0.95),
                          nrepetitions = 50)
    model <- NeuralNetwork(medv ~ ., data = train, layers = c(5, 3),
                           scale = TRUE, linear.output = TRUE, threshold = 0.5,
                           options = model_options)

    # test for correct model results
    expect_s3_class(model, "NeuralNetwork")
    expect_equal(model$neural_network$result.matrix, nn$result.matrix)
    expect_true(all.equal(model$neural_network$data, nn$data,
                          check.attributes = FALSE))
    expect_equal(model$neural_network$model.list$response,
                 nn$model.list$response)
    expect_equal(model$neural_network$model.list$variables,
                 nn$model.list$variables)
    expect_equal(model$scale, TRUE)
    expect_equal(model$min_and_max_numeric_columns,
                 data.frame(min = mins, max = maxs))
    expect_equal(model$type, "numerical")
    expect_equal(model$dependent, "medv")
    expect_equal(model$options, model_options)
    expect_true(!is.null(model$stored_data))
})

test_that("Creating NeuralNetwork for categorical dependent variable works", {
    # load library for getting iris data set
    library(datasets)
    data("iris")

    # prepare data for analysis
    iris$setosa <- iris$Species == "setosa"
    iris$setosa <- iris$setosa + 0
    iris$versicolor <- iris$Species == "versicolor"
    iris$versicolor <- iris$versicolor + 0
    iris$virginica <- iris$Species == "virginica"
    iris$virginica <- iris$virginica + 0

    # create test data set for the expected model
    train_test <- iris

    # fit expected model
    set.seed(1)
    nn <- neuralnet(
        setosa + versicolor + virginica ~
        Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        data = train_test, hidden = c(5, 5), rep = 5, err.fct = "ce",
        linear.output = FALSE, stepmax = 1000000, threshold = 0.5)

    # create data set for the test model
    train_model <- train_test[, !(names(train_test) %in%
                                      c("setosa", "versicolor", "virginica"))]

    # fit test model
    set.seed(1)
    model_options <- list(store = TRUE, parallel = TRUE, probs = c(0.05, 0.95),
                          nrepetitions = 50)
    model <- NeuralNetwork(
        Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        data = train_model, layers = c(5, 5), rep = 5, err.fct = "ce",
        linear.output = FALSE, stepmax = 1000000, threshold = 0.5,
        options = model_options)

    # test for correct model results
    expect_s3_class(model, "NeuralNetwork")
    expect_equal(model$neural_network$result.matrix, nn$result.matrix)
    expect_equal(sort(colnames(model$neural_network$data)),
                 sort(colnames(nn$data)))
    expect_equal(model$neural_network$model.list$response,
               nn$model.list$response)
    expect_equal(model$neural_network$model.list$variables,
               nn$model.list$variables)
    expect_equal(model$scale, FALSE)
    expect_equal(model$type, "categorical")
    expect_equal(model$dependent, "Species")
    expect_equal(model$options, model_options)
    expect_true(!is.null(model$stored_data))
})

test_that("Creating neural network for binary dependent variable works", {
    # load library for data
    library(faraway)
    library(nnet)

    # prepares data for analysis
    pima$glucose[pima$glucose == 0] <- NA
    pima$diastolic[pima$diastolic == 0] <- NA
    pima$triceps[pima$triceps == 0] <- NA
    pima$insulin[pima$insulin == 0] <- NA
    pima$bmi[pima$bmi == 0] <- NA
    pima <- pima[complete.cases(pima), ]
    pima$test <- as.factor(pima$test)
    levels(pima$test) <- c("Negative", "Positive")
    train <- pima

    # scale data
    scale_column <- function(column){
        if (!(is.numeric(column))) {
            return(column)
        } else {
            min_col <- min(column)
            max_col <- max(column)
            return(scale(column, center = min_col, scale = max_col - min_col))
        }
    }

    numeric_columns <- sapply(train, is.numeric)
    scaled <- as.data.frame(lapply(train, scale_column))
    maxs <- apply(train[, numeric_columns], 2, max)
    mins <- apply(train[, numeric_columns], 2, min)

    identifier <- class.ind(scaled[["test"]])
    rownames(identifier) <- rownames(scaled)
    scaled <- cbind(scaled, identifier)

    # fit expected and test model
    set.seed(1)
    nn <- neuralnet(Negative + Positive ~ pregnant + glucose + diastolic +
                        triceps + insulin + bmi + diabetes + age, hidden = 2,
                    data = scaled, linear.output = FALSE, threshold = 1.0,
                    stepmax = 1e6)

    set.seed(1)
    model_options <- list(store = TRUE, parallel = TRUE, probs = c(0.05, 0.95),
                          nrepetitions = 50)
    model <- NeuralNetwork(test ~ pregnant + glucose + diastolic + triceps +
                               insulin + bmi + diabetes + age, data = train,
                           layers = 2, scale = TRUE, linear.output = FALSE,
                           stepmax = 1e6, threshold = 1.0,
                           options = model_options)

    # test for correct model results
    expect_s3_class(model, "NeuralNetwork")
    expect_equal(model$neural_network$result.matrix, nn$result.matrix)
    expect_equal(model$neural_network$data, nn$data)
    expect_equal(model$neural_network$model.list$response,
                 nn$model.list$response)
    expect_equal(model$neural_network$model.list$variables,
                 nn$model.list$variables)
    expect_equal(model$scale, TRUE)
    expect_equal(model$min_and_max_numeric_columns,
                 data.frame(min = mins, max = maxs))
    expect_equal(model$type, "categorical")
    expect_equal(model$dependent, "test")
    expect_equal(model$options, model_options)
    expect_true(!is.null(model$stored_data))
})
