# NeuralNetworkVisualization

[![Build Status](https://travis-ci.com/AlexAfanasev/NeuralNetworkVisualization.svg?branch=master)](https://travis-ci.com/AlexAfanasev/NeuralNetworkVisualization)
[![codecov](https://codecov.io/gh/AlexAfanasev/NeuralNetworkVisualization/branch/master/graph/badge.svg)](https://codecov.io/gh/AlexAfanasev/NeuralNetworkVisualization)

> *Copyright 2019 Jacqueline Dorothea Seufert, Alex Afanasev. Licensed under the MIT license.*

`NeuralNetworkVisualization` is a R Package for creating beautiful partial dependence plots for Neural Networks using ggplot2, plotly and shiny.

This software was developed by **Jacqueline Dorothea Seufert** and **Alex Afanasev** and is available freely.

## Installation
`NeuralNetworkVisualization` is available through GitHub:

To install the latest development version from GitHub:

``` r
install.packages("devtools")
devtools::install_github("AlexAfanasev/NeuralNetworkVisualization")
```

## Examples
Step by step procedure:
1. Fit  a neural network using the NeuralNetwork class (uses the famous neuralnet package for fitting the model)
2. Create visualizations of the partial dependencies using the plot_partial_dependencies function (creates visualizations in ggplot2 and if wanted in plotly, adds a confidence interval)

Confidence intervals for the partial dependencies can be created directly using the plot_partial_dependencies function or by providing the options parameter while creating the NeuralNetwork model.

The following example models can be obtained using the example_nn_model function!

**Numerical response variable:**
```r
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

plot_partial_dependencies(model, use_stored_data = TRUE, type = "ggplotly")
```
![Numerical Plot 1](inst/img/numerical_plot_1.png "fig:")

```r
plot_partial_dependencies(model, predictors = "crim", use_stored_data = TRUE)
```
![Numerical Plot 2](inst/img/numerical_plot_2.png "fig:")

```r
plot_partial_dependencies(model, predictors = "crim")
```
![Numerical Plot 3](inst/img/numerical_plot_3.png "fig:")

```r
plot_partial_dependencies(model, predictors = c("crim", "age"),
                          type = "ggplotly", probs = c(0.05, 0.95),
                          nrepetitions = 100, parallel = TRUE)
```
![Numerical Plot 4](inst/img/numerical_plot_4.png "fig:")

**Categorical response variable:**
```r
library(NeuralNetworkVisualization)

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

plot_partial_dependencies(model, type = "ggplotly", use_stored_data = TRUE)
```
![Categorical Plot 1](inst/img/categorical_plot_1.png "fig:")

```r
plot_partial_dependencies(model, predictors = "Sepal.Length",
                          use_stored_data = TRUE)
```
![Categorical Plot 2](inst/img/categorical_plot_2.png "fig:")

```r
plot_partial_dependencies(model, predictors = "Sepal.Length")
```
![Categorical Plot 3](inst/img/categorical_plot_3.png "fig:")

```r
plot_partial_dependencies(model, predictors = c("Sepal.Length", "Petal.Length"),
                          type = "ggplotly", probs = c(0.1, 0.9),
                          nrepetitions = 100, parallel = TRUE)
```
![Categorical Plot 4](inst/img/categorical_plot_4.png "fig:")

**Binary response variable:**
```r
library(NeuralNetworkVisualization)

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

plot_partial_dependencies(model, use_stored_data = TRUE, type = "ggplotly")
```
![Binary Plot 1](inst/img/binary_plot_1.png "fig:")

```r
plot_partial_dependencies(model, predictors = "glucose", use_stored_data = TRUE)
```
![Binary Plot 2](inst/img/binary_plot_2.png "fig:")

```r
plot_partial_dependencies(model, predictors = "glucose")
```
![Binary Plot 3](inst/img/binary_plot_3.png "fig:")

```r
plot_partial_dependencies(model, predictors = c("pregnant", "diastolic"),
                          type = "ggplotly", probs = c(0.05, 0.95),
                          nrepetitions = 100, parallel = TRUE)
```
![Binary Plot 4](inst/img/binary_plot_4.png "fig:")

## Shiny App

**Please run the shiny app within a modern browser. Dont be stupid trying to use Microsoft Internet Explorer or Edge.**

Within the shiny app NeuralNetwork models can be uploaded as a .rds file, selected from the global environment or the available example models can be used.

**Run the shiny app:**
```r
library(NeuralNetworkVisualization)
run_shiny_app()
```
![Shiny App](inst/img/shiny_app.png "fig:")

## Sources
* https://towardsdatascience.com/first-neural-network-for-beginners-explained-with-code-4cfd37e06eaf
* https://www.datacamp.com/community/tutorials/neural-network-models-r
* https://towardsdatascience.com/introducing-pdpbox-2aa820afd312
* https://christophm.github.io/interpretable-ml-book/pdp.html
* https://bgreenwell.github.io/pdp/articles/pdp.html

