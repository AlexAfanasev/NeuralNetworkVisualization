#' Create a NeuralNetwork for further visualization
#'
#' \code{NeuralNetwork} Returns the trained neural network
#'
#' This is a S3 class. It defines a neural network and the generic plot function
#' is overwritten to handle the NeuralNetwork class as input.

#' @param ... further parameters for neuralnet, see:
#'   \code{\link[neuralnet]{neuralnet}}
#' @param f A formula representing the model that should be fitted. Handles
#'   categorical, binary and numerical data.
#' @param data The data that should be used for training the neural network.
#' @param layers Vector representing the number of layers that should be used.
#' @param scale Boolean representing if the data should be scaled or not

#'
#' @return NeuralNetwork class containing the training data, the neural network
#'   and  the type of the dependent variable.
#'
#' @examples
#' # Example: Numeric or Binary
#' library(MASS)
#' neural_network <- NeuralNetwork(f = "medv ~ .", data = Boston,
#'                                 layers = c(5, 3), scale = TRUE,
#'                                 linear.output = TRUE)
#'
#' # Example: Categoric
#' library(datasets)
#' neural_network <- NeuralNetwork(f = "Species ~ .", data = iris,
#'                                 layers = c(10, 10), rep = 5, err.fct = "ce",
#'                                 linear.output = FALSE, lifesign = "minimal",
#'                                 stepmax = 1000000, threshold = 0.001)
#'
#' @import neuralnet
#' @import nnet
#' @name NeuralNetwork
#' @export
NeuralNetwork <- function (f, data, layers, scale = FALSE, ...) {
    f <- as.formula(f)
    dependent <- all.vars(f[[2]])
    independent <- all.vars(f[[3]])
    type <- get_type(data[[dependent]])
    if (isTRUE(scale)) {
        data <- scale_data(data)
    }
    neural_network <- fit_neural_network(f, data, layers, type, dependent,
                                         independent, ...)
    return(structure(
        list(neural_network = neural_network,
             type = type, dependent = dependent), class = "NeuralNetwork"))
}

#' @keywords internal
#' Returrns the type for the dependent variable (numerical or categorical)
get_type <- function (data) {
    if (is.factor(data)) {
        return("categorical")
    } else if (is.numeric(data)) {
        return("numerical")
    } else {
        stop("Dependent variable is not of class factor or numeric!")
    }

}

#' @keywords internal
#' Scales the data
scale_data <- function(data){
    maxs <- apply(data, 2, max)
    mins <- apply(data, 2, min)
    data <- as.data.frame(scale(data, center = mins,
                                scale = maxs - mins))
    return(data)
}

#' @keywords internal
#' Fits neural network for either numerical or categorical dependent variable
fit_neural_network <- function (f, data, layers, type, dependent, independent,
                                ...) {
    if (type == "numerical") {
        return(fit_neural_network_numeric(f, data, layers, ...))
    } else if (type == "categorical") {
        fit_neural_network_categorical(f, data, layers, dependent, independent,
                                       ...)
    }
}

#' @keywords internal
#' Fits neural network for numerical dependent variable
fit_neural_network_numeric <- function (f, data, layers, ...) {
    return(neuralnet(f, data = data, hidden = layers, ...))
}

#' @keywords internal
#' Fits neural network for cateogircal dependent variable
fit_neural_network_categorical <- function (f, data, layers, dependent,
                                            independent, ...) {
    identifier <- class.ind(data[[dependent]])
    rownames(identifier) <- rownames(data)
    data <- cbind(data, identifier)
    f <- as.formula(paste(paste(levels(data[[dependent]]), collapse = "+"),
                          "~", paste(independent, collapse = "+"), sep = " "))
    return(neuralnet(f, data = data, hidden = layers, ...))
}
