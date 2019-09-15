test_that("Plotting with numerical dependent variable works", {
    library(MASS)
    data <- Boston; data$chas <- as.factor(data$chas)
    train <- data

    set.seed(1)
    model_options <- list(store = TRUE, parallel = TRUE, probs = c(0.05, 0.95),
                          nrepetitions = 50)
    model <- NeuralNetwork(medv ~ ., data = train, layers = c(5, 3),
                           scale = TRUE, linear.output = TRUE, threshold = 0.5,
                           options = model_options)

    expect_error(plot_partial_dependencies(model), NA)
    expect_error(plot_partial_dependencies(model, predictors = "crim"), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = "crim", type = "ggplotly", probs = c(0.2, 0.8),
        nrepetitions = 50, parallel = TRUE), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("crim", "age"), probs = c(0.1, 0.9),
        nrepetitions = 50, parallel = TRUE), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("crim", "age"), type = "ggplotly"), NA)
    expect_error(plot_partial_dependencies(model, use_stored_data = TRUE), NA)
    expect_error(plot_partial_dependencies(
        model, use_stored_data = TRUE,  predictors = "crim",
        type = "ggplotly"), NA)
    expect_error(plot_partial_dependencies(
        model, use_stored_data = TRUE,  predictors = c("crim", "age")), NA)

    expect_error(plot_partial_dependencies(
        model, predictors = c("crim", "all")))
    expect_error(plot_partial_dependencies(
        model, predictors = c("crim", "not_there")))
    expect_error(plot_partial_dependencies(model, predictors = "not_there"))
    expect_error(plot_partial_dependencies(
        model, predictors = "crim", probs = c("a", 1)))
    expect_error(plot_partial_dependencies(
        model, predictors = "crim", probs = c(1, 2)))
    expect_error(plot_partial_dependencies(
        model, predictors = "crim", type = "test"))
    expect_error(plot_partial_dependencies(
        model, predictors = "crim", probs = c(0.05, 0.95), nrepetitions = 1))
})

test_that("Plotting with categorical dependent variable works", {
    library(datasets)
    data("iris")
    train_model <- iris

    set.seed(1)
    model_options <- list(store = TRUE, parallel = TRUE, probs = c(0.05, 0.95),
                          nrepetitions = 50)
    model <- NeuralNetwork(
        Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        data = train_model, layers = c(5, 5), rep = 5, err.fct = "ce",
        linear.output = FALSE, stepmax = 1000000, threshold = 0.5,
        options = model_options)

    expect_error(plot_partial_dependencies(model), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = "Sepal.Length"), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = "Sepal.Length", type = "ggplotly",
        probs = c(0.2, 0.8), nrepetitions = 50, parallel = TRUE), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("Sepal.Length", "Petal.Length")), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("Sepal.Length", "Petal.Length"),
        probs = c(0.1, 0.9), nrepetitions = 50, parallel = TRUE), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("Sepal.Length", "Petal.Length"),
        type = "ggplotly"), NA)
    expect_error(plot_partial_dependencies(model, use_stored_data = TRUE), NA)
    expect_error(plot_partial_dependencies(
        model, use_stored_data = TRUE,  predictors = "Sepal.Length",
        type = "ggplotly"), NA)
    expect_error(plot_partial_dependencies(
        model, use_stored_data = TRUE,
        predictors = c("Sepal.Length", "Petal.Length")), NA)

    expect_error(plot_partial_dependencies(
        model, predictors = c("Sepal.Length", "all")))
    expect_error(plot_partial_dependencies(
        model, predictors = c("Sepal.Length", "not_there")))
    expect_error(plot_partial_dependencies(model, predictors = "not_there"))
    expect_error(plot_partial_dependencies(
        model, predictors = "Sepal.Length", probs = c(1, 2)))
    expect_error(plot_partial_dependencies(
        model, predictors = "Sepal.Length", type = "hi"))
    expect_error(plot_partial_dependencies(
        model, predictors = "Sepal.Length", probs = c(0.05, 0.95),
        nrepetitions = 1))
})


test_that("Plotting with binary dependent variable works", {
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
    model_options <- list(store = TRUE, parallel = TRUE, probs = c(0.05, 0.95),
                          nrepetitions = 50)
    model <- NeuralNetwork(test ~ pregnant + glucose + diastolic + triceps +
                               insulin + bmi + diabetes + age,
                           data = train, layers = 2, linear.output = FALSE,
                           threshold = 1.0, stepmax = 1e6,
                           options = model_options)

    expect_error(plot_partial_dependencies(model), NA)
    expect_error(plot_partial_dependencies(model, predictors = "glucose"), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("pregnant", "diastolic")), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = "glucose", type = "ggplotly",
        probs = c(0.2, 0.8), nrepetitions = 50, parallel = TRUE), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("pregnant", "diastolic"),
        probs = c(0.1, 0.9), nrepetitions = 50, parallel = TRUE), NA)
    expect_error(plot_partial_dependencies(
        model, predictors = c("pregnant", "diastolic"), type = "ggplotly"), NA)
    expect_error(plot_partial_dependencies(model, use_stored_data = TRUE), NA)
    expect_error(plot_partial_dependencies(
        model, use_stored_data = TRUE,  predictors = "glucose",
        type = "ggplotly"), NA)
    expect_error(plot_partial_dependencies(
        model, use_stored_data = TRUE,
        predictors = c("pregnant", "diastolic")), NA)

    expect_error(plot_partial_dependencies(
        model, predictors = c("pregnant", "all")))
    expect_error(plot_partial_dependencies(
        model, predictors = c("pregnant", "not_there")))
    expect_error(plot_partial_dependencies(model, predictors = "not_there"))
    expect_error(plot_partial_dependencies(
        model, predictors = c("pregnant", "diastolic"), probs = c(100, 0)))
    expect_error(plot_partial_dependencies(
        model, predictors = c("pregnant", "diastolic"), type = "?"))
    expect_error(plot_partial_dependencies(
        model, predictors = "pregnant", probs = c(0.05, 0.95),
        nrepetitions = 1))
})
