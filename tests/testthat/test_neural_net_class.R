test_that("Creating NeuralNetwork for numerical dependent variable works", {
    library(MASS)
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

    expect_equal(model$neural_network$result.matrix, nn$result.matrix)
    expect_equal(model$neural_network$data, nn$data)
    expect_equal(model$neural_network$model.list$response,
                 nn$model.list$response)
    expect_equal(model$neural_network$model.list$variables,
                 nn$model.list$variables)
    expect_equal(model$type, "numerical")
    expect_equal(model$dependent, "medv")
})

test_that("Creating NeuralNetwork for categorical dependent variable works", {
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

    set.seed(1)
    nn <- neuralnet(
        setosa + versicolor + virginica ~
        Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        data = train_test, hidden = c(10, 10), rep = 5, err.fct = "ce",
        linear.output = FALSE, lifesign = "minimal", stepmax = 1000000,
        threshold = 0.001)

    train_model <- train_test[, !(names(train_test) %in%
                                      c("setosa", "versicolor", "virginica"))]

    set.seed(1)
    model <- NeuralNetwork(
        Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        data = train_model, layers = c(10, 10), rep = 5, err.fct = "ce",
        linear.output = FALSE, lifesign = "minimal", stepmax = 1000000,
        threshold = 0.001)

    expect_equal(model$neural_network$result.matrix, nn$result.matrix)
    expect_equal(sort(colnames(model$neural_network$data)),
                 sort(colnames(nn$data)))
    expect_equal(model$neural_network$model.list$response,
               nn$model.list$response)
    expect_equal(model$neural_network$model.list$variables,
               nn$model.list$variables)
    expect_equal(model$type, "categorical")
    expect_equal(model$dependent, "Species")
})

test_that("Creating neural network for binary dependent variable works", {
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

    expect_equal(model$neural_network$result.matrix, nn$result.matrix)
    expect_equal(model$neural_network$data, nn$data)
    expect_equal(model$neural_network$model.list$response,
                 nn$model.list$response)
    expect_equal(model$neural_network$model.list$variables,
                 nn$model.list$variables)
    expect_equal(model$type, "numerical")
    expect_equal(model$dependent, "test")
})
