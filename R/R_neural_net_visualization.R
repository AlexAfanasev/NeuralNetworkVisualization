#' Function for plotting the partial dependencies of the created NeuralNetwork
#' in R window.
#'
#' \code{plot_partial_dependencies} Plots the partial dependencies for
#'   the specified predictors.
#'
#' @param neural_net The NeuralNetwork instance, see:
#'   \code{\link{NeuralNetwork}}
#' @param predictors The predictors of the neural network for which to plot the
#'   partial dependencies
#' @param probs Vector of 2 probabilities for the confidence intervals.
#'   If booth are 0 intervals will not be plotted.
#'
#' @return Created figure
#'
#' @examples
#' \dontrun{
#' # Example: Numeric or Binary
#' library(MASS)
#' neural_network <- NeuralNetwork(f = "medv ~ .", data = Boston,
#'                                 layers = c(5, 3), scale = TRUE,
#'                                 linear.output = TRUE)
#' plot_partial_dependencies(neural_network, predictors = "crim",
#'                           probs = c(0.2, 0.8))
#' plot_partial_dependencies(neural_network, predictors = c("crim", "age"))
#' plot_partial_dependencies(neural_network, probs = c(0.1, 0.9))
#'
#' # Example: Categoric
#' library(datasets)
#' neural_network <- NeuralNetwork(f = "Species ~ .", data = iris,
#'                                 layers = c(10, 10), rep = 5, err.fct = "ce",
#'                                 linear.output = FALSE, lifesign = "minimal",
#'                                 stepmax = 1000000, threshold = 0.001)
#' plot_partial_dependencies(neural_network, predictors = "Sepal.Length")
#' plot_partial_dependencies(neural_network,
#'                           predictors = c("Sepal.Length", "Petal.Length"))
#' plot_partial_dependencies(neural_network)
#' }
#' @importFrom plotly ggplotly
#' @importFrom dplyr syms bind_rows
#' @importFrom purrr map
#' @importFrom tidyr gather
#' @importFrom ggplot2 aes geom_line geom_ribbon facet_wrap vars labs theme_grey ggplot
#' @importFrom magrittr %>%
#' @name plot_partial_dependencies
#' @export
plot_partial_dependencies <- function (neural_net, predictors = "all",
                                       probs = c(0, 0)) {
    predictors <- get_predictors(neural_net, predictors)
    # TODO: Add check for probs function!
    if (length(predictors) > 1) {
        return(ggplotly(plot_multiple(neural_net, predictors, probs)))
    } else {
        return(ggplotly(plot_single(neural_net, predictors[[1]], probs)))
    }
}

#' Returns the predictors for which to plot the partial dependencies.
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
        stop("Please specify predictors that where used in the
             NeuralNetwork!")
    } else {
        return(syms(predictors))
    }
}

#' Plots partial dependencies for multiple predictors.
#' @keywords internal
plot_multiple <- function (neural_net, predictors, probs) {
    prediction_names <- ifelse(neural_net$type == "categorical",
                                   yes = 2, no = 1)
    prepared_data <- predictors %>%
        map(~ prepare_data(neural_net, .x)) %>%
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
#' @keywords internal
plot_multiple_numerical <- function (prepared_data, neural_net) {
    return(ggplot(aes(values, yhat, ymin = lwr, ymax = upr), data = prepared_data) +
               geom_line(size = 1) + geom_ribbon(alpha = 0.25) +
               facet_wrap(vars((predictor)), scales = "free") +
               labs(title = paste("Partial dependence plots with response",
                                  neural_net$dependent),
                    y = "Marginal probability of predictor",
                    x = "Predictor") +
               theme_grey())
}

#' Plots partial dependencies for given predictor with categorical dependent
#' variable.
#' @keywords internal
plot_multiple_categorical <- function (prepared_data, neural_net) {
  return(ggplot(data = prepared_data, aes(values, yhat, color = class)) +
                 geom_line(size = 1) +
                 facet_wrap(vars(predictor), scales = "free") +
                 labs(title = paste("Partial dependence plots with response",
                                    neural_net$dependent),
                      y = "Marginal probability of predictor",
                      x = "Predictor") +
                 theme_grey())
}

#' Plots partial dependencies for single given predictor.
#' @keywords internal
plot_single <- function (neural_net, predictor, probs) {
    prepared_data <- prepare_data(neural_net, predictor, probs)
    if (neural_net$type == "numerical") {
        return(plot_single_numerical(prepared_data, predictor, neural_net))
    } else if (neural_net$type == "categorical") {
        return(plot_single_categorical(prepared_data, predictor, neural_net))
    }
}

#' Plots partial dependencies for given predictor with numerical dependent
#' variable.
#' @keywords internal
plot_single_numerical <- function (prepared_data, predictor, neural_net) {
    return(ggplot(data = prepared_data,
                  aes(x = !!predictor, y = yhat, ymin = lwr, ymax = upr)) +
               geom_line(size = 1) + geom_ribbon(alpha = 0.25) +
               labs(title = paste("Partial dependence plot with response",
                                  neural_net$dependent),
                    y = paste("Marginal probability of", predictor),
                    x = paste(predictor)) + theme_grey())
}

#' Plots partial dependencies for given predictor with categorical dependent
#' variable.
#' @keywords internal
plot_single_categorical <- function (prepared_data, predictor, neural_net) {
    return(ggplot(data = prepared_data,
                  aes(x = !!predictor, y = yhat, colour = class,
                      ymin = lwr, ymax = upr)) +
               geom_line(size = 1) + geom_ribbon(alpha = 0.25) +
               labs(title = paste("Partial dependence plot for",
                                  neural_net$dependent),
                    y = paste("Marginal probability of", predictor),
                    x = paste(predictor)) +
               theme_grey())
}

