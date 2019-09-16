library(NeuralNetworkVisualization)

# Example for Plotting with numerical dependent variable
library(MASS)
data <- Boston; data$chas <- as.factor(data$chas)
train <- data

set.seed(1)
system.time(
    model <- NeuralNetwork(medv ~ ., data = train, layers = c(5, 3),
                           scale = TRUE, linear.output = TRUE, threshold = 0.5,
                           options = list(store = TRUE, parallel = TRUE,
                                          probs = c(0.05, 0.95),
                                          nrepetitions = 1000)))
saveRDS(model, "numerical.rds")

plot_partial_dependencies(model, use_stored_data = TRUE, type = "ggplotly")
plot_partial_dependencies(model, predictors = "crim", use_stored_data = TRUE)
plot_partial_dependencies(model, predictors = "crim")
plot_partial_dependencies(model, predictors = c("crim", "age"),
                          type = "ggplotly", probs = c(0.05, 0.95),
                          nrepetitions = 100, parallel = TRUE)

# Example for Plotting with categorical dependent variable
library(datasets)
data("iris")
train_model <- iris

set.seed(1)
system.time(
    model <- NeuralNetwork(
        Species ~ ., data = train_model, layers = c(5, 5), rep = 5,
        linear.output = FALSE, scale = TRUE, err.fct = "ce", stepmax = 1000000,
        threshold = 0.5, options = list(
            store = TRUE, parallel = TRUE, nrepetitions = 1000,
            probs = c(0.05, 0.95))))
saveRDS(model, "categorical.rds")

plot_partial_dependencies(model, type = "ggplotly", use_stored_data = TRUE)
plot_partial_dependencies(model, predictors = "Sepal.Length",
                          use_stored_data = TRUE)
plot_partial_dependencies(model, predictors = "Sepal.Length")
plot_partial_dependencies(model, predictors = c("Sepal.Length", "Petal.Length"),
                          type = "ggplotly", probs = c(0.1, 0.9),
                          nrepetitions = 100, parallel = TRUE)

# Example for Plotting with binary dependent variable
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
system.time(
    model <- NeuralNetwork(
        test ~ pregnant + glucose + diastolic + triceps + insulin + bmi +
            diabetes + age, data = train, layers = 2, err.fct = "ce",
        linear.output = FALSE, threshold = 1.0, stepmax = 1e6, scale = TRUE,
        options = list(store = TRUE, parallel = TRUE, probs = c(0.05, 0.95),
                       nrepetitions = 1000)))
saveRDS(model, "binary.rds")

plot_partial_dependencies(model, use_stored_data = TRUE, type = "ggplotly")
plot_partial_dependencies(model, predictors = "glucose", use_stored_data = TRUE)
plot_partial_dependencies(model, predictors = "glucose")
plot_partial_dependencies(model, predictors = c("pregnant", "diastolic"),
                          type = "ggplotly", probs = c(0.05, 0.95),
                          nrepetitions = 100, parallel = TRUE)
