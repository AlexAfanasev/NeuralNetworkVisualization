#' Function for plotting the partial dependencies of the created NeuralNetwork
#'
#' \code{plot_partial_dependencies} plots the partial dependencies for
#'   the specified predictors.
#'
#' @param neural_net The NeuralNetwork instance, see:
#'   \code{\link{NeuralNetwork}}
#' @param predictors Vector of predictors of the neural network for which to
#'   plot the partial dependencies.
#' @param probs Vector of lower and upper bound probabilities for the confidence
#'   interval. If booth are 0, intervals will not be plotted.
#' @param type Either 'ggplot' if the plot should be created using ggplot or
#'   'ggplotly' if plotly should be used.
#' @param nrepetitions Number of samples used within bootstrap for confidence
#'   intervals.
#'
#' @return Created figure
#'
#' @examples
#' # Example: Numeric or Binary
#' library(MASS)
#' neural_network <- NeuralNetwork(f = "medv ~ .", data = Boston,
#'                                 layers = c(5, 3), scale = TRUE,
#'                                 linear.output = TRUE)
#' plot_partial_dependencies(neural_network, predictors = "crim",
#'                           probs = c(0.2, 0.8), type = "ggplotly")
#' plot_partial_dependencies(neural_network, predictors = c("crim", "age"))
#' plot_partial_dependencies(neural_network, probs = c(0.1, 0.9))
#'
#' # Example: Categoric
#' library(datasets)
#' model <- NeuralNetwork(
#'    Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'    data = iris, layers = c(10, 10), rep = 5, err.fct = "ce",
#'    linear.output = FALSE, lifesign = "minimal", stepmax = 1000000,
#'    threshold = 0.001, scale = F)
#'
#' plot_partial_dependencies(model, predictors = "Petal.Length")
#' plot_partial_dependencies(model,
#'                           predictors = c("Sepal.Length", "Petal.Length"))
#' plot_partial_dependencies(model, type = "ggplotly")
#'
#' @importFrom plotly ggplotly
#' @name plot_partial_dependencies
#' @export
plot_partial_dependencies <- function (neural_net, predictors = "all",
                                       probs = c(0, 0), type = "ggplot",
                                       nrepetitions = 50) {
    if (!all(probs == 0)) is_valid_probs(probs); is_valid_type(type)
    predictors <- get_predictors(neural_net, predictors)

    if (length(predictors) > 1) {
        figure <- plot_multiple(neural_net, predictors, probs, nrepetitions)
    } else {
        figure <- plot_single(neural_net, predictors[[1]], probs, nrepetitions)
    }

    if (type == "ggplot") {
        return(figure)
    } else {
        return(ggplotly(figure))
    }
}

#' Checks for valid type of plot.
#'
#' @keywords internal
is_valid_type <- function (type) {
    allowed <- c("ggplot", "ggplotly")
    if (!(type %in% allowed)) {
        stop("Please specify allowed type for plot: either ggplot or ggplotly!")
    }
}

#' Checks for valid prediction interval probabilities.
#'
#' @keywords internal
is_valid_probs <- function (probs) {
    if (!is.numeric(probs) || length(probs) != 2) {
        stop("Please specify the prediction interval probabilities as a vector
             of two numbers!")
    }
    if (sum(probs) != 1) {
        stop("The prediction interval probabilities have to add up to one!")
    }
}

#' Returns the predictors for which to plot the partial dependencies.
#'
#' @importFrom dplyr syms
#' @keywords internal
get_predictors <- function (neural_net, predictors) {
    if (length(predictors) > 1) {
        if (any(predictors == "all")) {
            stop("You cannot use multiple predictors including 'all'
                 as a predictor!")
        }
    } else {
        if (predictors == "all") {
            return(syms(neural_net$neural_network$model.list$variables))
        }
    }
    if (any(!(predictors %in%
              neural_net$neural_network$model.list$variables))) {
        stop("Please specify predictors that were used in the
             NeuralNetwork!")
    } else {
        return(syms(predictors))
    }
}

#' Plots partial dependencies for multiple predictors.
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom tidyr gather
#' @importFrom dplyr bind_rows
#' @importFrom furrr future_map
#' @importFrom future plan multiprocess
#' @keywords internal
plot_multiple <- function (neural_net, predictors, probs, nrepetitions) {
    prediction_names <- ifelse(neural_net$type == "categorical",
                               yes = 2, no = 1)

    plan(multiprocess)
    prepared_data <- predictors %>%
        future_map(~ prepare_data(neural_net, .x, probs, nrepetitions)) %>%
        map(~ gather(.x, "predictor", "values", prediction_names)) %>%
        bind_rows()

    if (neural_net$type == "numerical") {
        return(plot_multiple_numerical(prepared_data, neural_net))
    } else if (neural_net$type == "categorical") {
        return(plot_multiple_categorical(prepared_data, neural_net))
    }
}

#' Plots partial dependencies for given predictors with numerical dependent
#' variable.
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap vars labs theme_minimal geom_point
#' @keywords internal
plot_multiple_numerical <- function (prepared_data, neural_net) {
    return(ggplot(aes(values, yhat, ymin = lwr, ymax = upr),
                  data = prepared_data) +
               geom_line(size = 1) + geom_point() + geom_ribbon(alpha = 0.25) +
               facet_wrap(vars((predictor)), scales = "free") +
               labs(title = paste("Partial dependence plots with response",
                                  neural_net$dependent),
                    y = "Marginal probability of predictor",
                    x = "Predictor") +
               theme_minimal())
}

#' Plots partial dependencies for given predictor with categorical dependent
#' variable.
#'
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap vars labs theme_minimal geom_point
#' @keywords internal
plot_multiple_categorical <- function (prepared_data, neural_net) {
  return(ggplot(data = prepared_data,
                aes(values, yhat, color = class, group = class, fill = class)) +
                geom_line(size = 1) + geom_point() + geom_ribbon(
                    aes(x = values, ymin = lwr, ymax = upr), alpha = 0.25) +
                 facet_wrap(vars(predictor), scales = "free") +
                 labs(title = paste("Partial dependence plots with response",
                                    neural_net$dependent),
                      y = "Marginal probability of predictor",
                      x = "Predictor") +
                theme_minimal())
}

#' Plots partial dependencies for single given predictor.
#'
#' @keywords internal
plot_single <- function (neural_net, predictor, probs, nrepetitions) {
    prepared_data <- prepare_data(neural_net, predictor, probs, nrepetitions)
    if (neural_net$type == "numerical") {
        return(plot_single_numerical(prepared_data, predictor, neural_net))
    } else if (neural_net$type == "categorical") {
        return(plot_single_categorical(prepared_data, predictor, neural_net))
    }
}

#' Plots partial dependencies for given predictor with numerical dependent
#' variable.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs theme_minimal geom_point
#' @keywords internal
plot_single_numerical <- function (prepared_data, predictor, neural_net) {
    return(ggplot(data = prepared_data,
                  aes(x = !!predictor, y = yhat, ymin = lwr, ymax = upr)) +
               geom_line(size = 1) + geom_point() + geom_ribbon(alpha = 0.25) +
               labs(title = paste("Partial dependence plot with response",
                                  neural_net$dependent),
                    y = paste("Marginal probability of", predictor),
                    x = paste(predictor)) + theme_minimal())
}

#' Plots partial dependencies for given predictor with categorical dependent
#' variable.
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal geom_point
#' @keywords internal
plot_single_categorical <- function (prepared_data, predictor, neural_net) {
    return(ggplot(data = prepared_data,
                  aes(x = !!predictor, y = yhat, colour = class)) +
               geom_line(size = 1) + geom_point() + geom_ribbon(aes(
                   x = !!predictor, ymin = lwr, ymax = upr),
                   alpha = 0.25) +
               labs(title = paste("Partial dependence plot for",
                                  neural_net$dependent),
                    y = paste("Marginal probability of", predictor),
                    x = paste(predictor)) +
               theme_minimal())
}
