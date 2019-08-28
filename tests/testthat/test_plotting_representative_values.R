test_that("Plotting with numerical dependent variable works", {
    library(MASS)
    data <- Boston
    index <- sample(1:nrow(data), round(0.75*nrow(data)))
    train <- data[index,]
    
    set.seed(1)
    model <- NeuralNetwork(medv ~ ., data = train, layers = c(5, 3),
                           scale = TRUE, linear.output = TRUE, threshold = 0.5)
            expect_error(plot_marginal_repres(model, predictor = "crim", 
                                              predictor_value = 2, 
                                              kind = "quantile"))
            expect_error(plot_marginal_repres(model, 
                                              predictor = c("crim", "tax"),
                                              predictor_value = 2))
            expect_error(plot_marginal_repres(model, predictor = "crim",
                                              predictor_value = 2,
                                              change_variables = "tax", 
                                              change_values = c(180,190)))
            expect_error(plot_marginal_repres(model, predictor = "tax", 
                                              predictor_value = 190,
                                              change_variables = "tax",
                                              change_value = 180))
            expect_error(plot_marginal_repres(model, predictor = c("black", 
                                                                   "tax"), 
                                              predictor_value = 190,
                                              change_variables = "crim",
                                              change_value = 2))
            expect_error(plot_marginal_repres(model, predictor = c("black", 
                                                                   "tax"), 
                                              predictor_value = c(190, 200),
                                              change_variables = "crim",
                                              change_value = 2), NA)
            expect_error(plot_marginal_repres(model, predictor = "black", 
                                              predictor_value = 190,
                                              change_variables = "crim",
                                              change_value = 2 , rep = 200,
                                              units = 2), NA)
            expect_error(plot_marginal_repres(model, predictor = "black", 
                                              predictor_value = 190,
                                              change_variables = NULL,
                                              change_value = NULL , rep = 200,
                                              units = 2, probs = c(0.2,0.8)),
                         NA)
            expect_error(plot_marginal_repres(model, predictor = c("black",
                                                                   "crim"), 
                                              predictor_value = NULL,
                                              change_variables = NULL,
                                              change_value = NULL, rep = 200,
                                              units = 2, probs = c(0.2,0.8)),
                         NA)
})

test_that("Plotting with categorical dependent variable works", {
    library(datasets)
    data("iris")
    index <- sample(x = nrow(iris), size = nrow(iris)*0.5)
    train_model <- iris[index,]
    
    set.seed(1)
    model <- NeuralNetwork(
        Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        data = train_model, layers = c(5, 5), rep = 5, err.fct = "ce",
        linear.output = FALSE, stepmax = 1000000, threshold = 0.5)
        expect_error(plot_marginal_repres(model, predictor = "Sepal.Length", 
        predictor_value = c(2,3), class = "Species"))
        expect_error(plot_marginal_repres(model, predictor = "Sepal.Length", 
                                          predictor_value = c(2,3,3)))
        expect_error(plot_marginal_repres(model, predictor = c("Sepal.Length",
                                                               "Petal.Length"), 
                                          predictor_value = c(1,2,3,4,5,6,7),
                                          class = "Species"))
        expect_error(plot_marginal_repres(model, predictor = "Sepal.Length", 
                                          predictor_value = c(1,2,3),
                                          change_variables = c("Petal.Length",
                                                               "Petal.Width"),
                                          change_value = c(5,6,6,6,6,6,6),
                                          class = "Species"))
        expect_error(plot_marginal_repres(model, predictor = "Sepal.Length", 
                                          predictor_value = c(1,2,3),
                                          kind = "quantile",
                                          change_variables = c("Petal.Length",
                                                               "Petal.Width"),
                                          change_value = c(5,6,6,6,6,6),
                                          class = "Species"))
        expect_error(plot_marginal_repres(model, predictor = "Sepal.Length", 
                                          predictor_value = c(1,2,3),
                                          kind = "quantile",
                                          change_variables = "Sepal.Length",
                                          change_value = c(5,6,6),
                                          class = "Species")) 
        expect_error(plot_marginal_repres(model, predictor = "Sepal.Length", 
                                          predictor_value = c(1,2,3),
                                          kind = "median",
                                          change_variables = c("Sepal.Width",
                                                               "Petal.Length"),
                                          change_value = c(5,6,6,5,6,7),
                                          class = "Species"), NA) 
        expect_error(plot_marginal_repres(model, predictor = "Petal.Length", 
                                          predictor_value = c(1,2,3),
                                          kind = "mean",
                                          change_variables = "Sepal.Length",
                                          change_value = c(5,6,6),
                                          class = "Species", rep = 150), NA) 
        expect_error(plot_marginal_repres(model, predictor = c("Sepal.Length", 
                                                               "Sepal.Width"),
                                          predictor_value = c(1,2,3, 4,5,6),
                                          kind = "median",
                                          change_variables = "Petal.Length",
                                          change_value = c(5,6,6),
                                          class = "Species"), NA) 
        expect_error(plot_marginal_repres(model, predictor = c("Sepal.Length", 
                                                               "Sepal.Width"),
                                          predictor_value = NULL,
                                          kind = "median",
                                          change_variables = "Petal.Length",
                                          change_value = c(5,6,6),
                                          class = "Species"), NA) 
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
    pima_size <- floor(0.75 * nrow(pima))
    index <- sample(seq_len(nrow(pima)), size = pima_size)
    train <- pima[index, ]
    
    set.seed(1)
    model <- NeuralNetwork(test ~ pregnant + glucose + diastolic + triceps +
                               insulin + bmi + diabetes + age,
                           data = train, layers = 2, linear.output = FALSE,
                           threshold = 0.5, stepmax = 1e6)
    
    expect_error(plot_marginal_repres(model, predictor = "bmi", 
                                      predictor_value = 33, 
                                      kind = "quantile"))
    expect_error(plot_marginal_repres(model, 
                                      predictor = c("bmi", "insulin"),
                                      predictor_value = 90))
    expect_error(plot_marginal_repres(model, predictor =  "bmi",
                                      predictor_value = 33,
                                      change_variables = "age", 
                                      change_values = c(10, 40)))
    expect_error(plot_marginal_repres(model, predictor = "bmi", 
                                      predictor_value = 33,
                                      change_variables = "bmi",
                                      change_value = 40))
    expect_error(plot_marginal_repres(model, predictor = c("bmi", 
                                                           "age"), 
                                      predictor_value = 50,
                                      change_variables = "insulin",
                                      change_value = 100 ))
    
})
