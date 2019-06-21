#' Function for plotting the partial dependencies of the created NeuralNetwork
#' in R window.
#'
#' \code{plot_partial_dependencies} Plots the partial dependencies for
#'   the specified predictors
#'
#' @param neural_net The NeuralNetwork instance, see:
#'   \code{\link{NeuralNetwork}}
#' @param predictors The predictors of the neural network for which to plot the
#'   partial dependencies
#' @return Created figure
#'
#' @examples
#' \dontrun{
#' # Example: Numeric or Binary
#' library(MASS)
#' neural_network <- NeuralNetwork(f = "medv ~ .", data = Boston,
#'                                 layers = c(5, 3), scale = TRUE,
#'                                 linear.output = TRUE)
#' plot_partial_dependencies(neural_network, predictors = "crim")
#' plot_partial_dependencies(neural_network, predictors = c("crim", "age"))
#' plot_partial_dependencies(neural_network)
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
#' @import tidyverse
#' @import neuralnet
#' @import rlang
#' @import plotly
#' @name plot_partial_dependencies
#' @export
plot_partial_dependencies <- function (neural_net, predictors = "all") {
    predictors <- get_predictors(neural_net, predictors)
    if (length(predictors) > 1) {
        plot_multiple(neural_net, predictors)
    } else {
        plot_single(neural_net, predictors[[1]])
    }
}

#' @keywords internal
#' Returns the predictors for which to plot the partial dependencies
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

#' @keywords internal
#' Plots partial dependencies for multiple predictors
plot_multiple <- function (neural_net, predictors) {
    # TODO: Ask Jaqueline about that fucking magick number
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

#' @keywords internal
#' Plots partial dependencies for given predictors with numerical dependent
#' variable
plot_multiple_numerical <- function (prepared_data, neural_net) {
   pred_plot <- ggplot(aes(values, yhat), data = prepared_data) +
                geom_line(size = 1) +
                facet_wrap(vars((predictor)), scales = "free") +
                labs(title = paste("Partial dependence plots with response",
                                   neural_net$dependent),
                     y = "Marginal probability of predictor",
                     x = "Predictor") +
                theme_grey()
    return(ggplotly(pred_plot))
}

#' @keywords internal
#' Plots partial dependencies for given predictor with categorical dependent
#' variable
plot_multiple_categorical <- function (prepared_data, neural_net) {
  pred_plot <- ggplot(data = prepared_data, aes(values, yhat, color = class)) +
                 geom_line(size = 1) +
                 facet_wrap(vars(predictor), scales = "free") +
                 labs(title = paste("Partial dependence plots with response",
                                    neural_net$dependent),
                      y = "Marginal probability of predictor",
                      x = "Predictor") +
                 theme_grey()
  return(ggplotly(pred_plot))
}

#' @keywords internal
#' Plots partial dependencies for single given predictor
plot_single <- function (neural_net, predictor) {
    prepared_data <- prepare_data(neural_net, predictor)
    if (neural_net$type == "numerical") {
        return(plot_single_numerical(prepared_data, predictor, neural_net))
    } else if (neural_net$type == "categorical") {
        return(plot_single_categorical(prepared_data, predictor, neural_net))
    }
}

#' @keywords internal
#' Plots partial dependencies for given predictor with numerical dependent
#' variable
plot_single_numerical <- function (prepared_data, predictor, neural_net) {
    pred_plot <- ggplot(data = prepared_data,
                  aes(x = !!predictor, y = yhat)) + geom_line(size = 1) +
               labs(title = paste("Partial dependence plot with response",
                                  neural_net$dependent),
                    y = paste("Marginal probability of", predictor),
                    x = paste(predictor)) + theme_grey()
    return(ggplotly(pred_plot))
}

#' @keywords internal
#' Plots partial dependencies for given predictor with categorical dependent
#' variable
plot_single_categorical <- function (prepared_data, predictor, neural_net) {
    pred_plot <- ggplot(data = prepared_data,
                  aes(x = !!predictor, y = yhat, colour = class)) +
               geom_line(size = 1) +
               labs(title = paste("Partial dependence plot for",
                                  neural_net$dependent),
                    y = paste("Marginal probability of", predictor),
                    x = paste(predictor)) +
               theme_grey()
    return(ggplotly(pred_plot))
}

