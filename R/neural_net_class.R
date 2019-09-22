#' Create a NeuralNetwork for further visualization
#'
#' \code{NeuralNetwork} returns the trained neural network
#'
#' This is a S3 class. It defines a neural network and has the
#' plot_partial_dependencies method for plotting marginal effects. Additionally,
#' you can use plot, predict and summary.
#'
#' @param ... further parameters for neuralnet, see:
#'   \code{\link[neuralnet]{neuralnet}}
#' @param f A formula representing the model that should be fitted. Handles
#'   categorical, binary and numerical data. Specify each column separately or
#'   all with y ~ . .
#' @param data The data that should be used for training the neural network.
#' @param layers Vector representing the number of hidden layers that should be
#'   used.
#' @param scale Boolean representing if the data should be scaled or not.
#' @param options List to specify that you want to run the bootstrap
#'   sampling directly in the model creation. Then this data can be used
#'   for creating the partial dependence plots.
#'
#' @return NeuralNetwork class containing the neuralnet, type of dependent
#'   variable, name of dependent variable, layers, min and max of each numeric
#'   column, additional parameters provided and stored data if specified.
#'
#' @examples
#' \dontrun{
#' # Example: Numeric
#' library(MASS)
#' neural_network <- NeuralNetwork(f = medv ~ ., data = Boston,
#'                                 layers = c(5, 3), scale = TRUE,
#'                                 linear.output = TRUE)
#'
#' # Example: Categoric
#' library(datasets)
#' model <- NeuralNetwork(
#'    Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'    data = iris, layers = c(10, 10), rep = 5, err.fct = "ce",
#'    linear.output = FALSE, lifesign = "minimal", stepmax = 1000000,
#'    threshold = 0.001, scale = T)
#' }
#'
#' @name NeuralNetwork
#' @export
NeuralNetwork <- function(f, data, layers, scale = FALSE,
                          options = list(store = FALSE, nrepetitions = 1000,
                                         probs = c(0.05, 0.95),
                                         parallel = TRUE), ...){
    f <- as.formula(f); row.names(data) <- NULL
    dependent <- all.vars(f[[2]])
    independent <- get_independent(data, dependent, all.vars(f[[3]]))
    factor_specification <- create_factor_specification(
        data, dependent, independent, f)
    data <- factor_specification$data; f <- factor_specification$f
    independent <- factor_specification$independent
    type <- get_type(data[[dependent]])

    numeric_columns <- sapply(data, is.numeric)
    min_and_max_numeric_columns <- min_max(data[, numeric_columns])

    if (isTRUE(scale)) {
        data[, numeric_columns] <- sapply(data[, numeric_columns], scale_column)
    }

    if (type == "categorical") {
        f <- as.formula(paste(
            dependent, "~", paste(independent, collapse = "+"), sep = " "))
    }


    neural_network <- fit_neural_network(f, data, layers, type, dependent,
                                         independent, ...)

    model <- structure(
        list(neural_network = neural_network,
             min_and_max_numeric_columns = min_and_max_numeric_columns,
             type = type, dependent = dependent, f = f, layers = layers,
             scale = scale, additional = list(...)), class = "NeuralNetwork")

    model <- add_bootstrap_data(model, options)

    return(model)
}


#' Returns the data with added factor columns, changed formula and all
#' independent variables.
#'
#' @keywords internal
create_factor_specification <- function(data, dependent, independent, f){
    factor_columns <- sort(which(sapply(data, is.factor) &
                                     (colnames(data) %in% independent)),
                           decreasing = TRUE)

    if (any(factor_columns)) {
        new_columns <- c()
        for (factor_column in factor_columns) {
            column <- data[, factor_column]
            levels(column) <- paste(colnames(data)[factor_column],
                                    levels(column), sep = "")

            identifier <- class.ind(column)[, 1:(length(levels(column)) - 1),
                                            drop = FALSE]
            new_columns <- c(new_columns, colnames(identifier))
            rownames(identifier) <- rownames(data)
            data <- cbind(data, identifier); data <- data[, -factor_column]
        }

        independent <- c(colnames(data)[which(colnames(data) %in% independent)],
                         new_columns)
        f <- paste(dependent, " ~ ", paste(independent, collapse = " + "),
                   sep = "")

    }
    return(list(data = data, f = f, independent = independent))
}

#' Returns the independent variables based on specification.
#'
#' @keywords internal
get_independent <- function(data, dependent_variable, specification){
    independent_variables <- colnames(data)[colnames(data) !=
                                                dependent_variable]
    if (any(specification == ".")) {
        return(independent_variables)
    } else {
        return(specification)
    }
}

#' Returns the type for the dependent variable (numerical or categorical).
#'
#' @keywords internal
get_type <- function(data){
    if (is.factor(data)) {
        return("categorical")
    } else if (is.numeric(data)) {
        return("numerical")
    } else {
        stop("Dependent variable is not of class factor or numeric!")
    }
}

#' Returns the scaled column.
#'
#' @keywords internal
scale_column <- function(col){
    maxs <- max(col)
    mins <- min(col)
    return(scale(col, center = mins, scale = maxs - mins))
}

#' Returns the min and max of a data.frame.
#'
#' @keywords internal
min_max <- function(data){
    mins <- apply(data, 2, min)
    maxs <- apply(data, 2, max)
    return(data.frame(min = mins, max = maxs))
}

#' Fits neural network for either numerical or categorical dependent variable.
#'
#' @keywords internal
fit_neural_network <- function(f, data, layers, type, dependent, independent,
                               ...){
    if (type == "numerical") {
        return(fit_neural_network_numeric(f, data, layers, ...))
    } else if (type == "categorical") {
        return(fit_neural_network_categorical(f, data, layers, dependent,
                                              independent, ...))
    }
}

#' Fits neural network for numerical dependent variable.
#'
#' @importFrom  neuralnet neuralnet
#' @keywords internal
fit_neural_network_numeric <- function(f, data, layers, ...){
    return(neuralnet(f, data = data, hidden = layers, ...))
}

#' Fits neural network for cateogircal dependent variable.
#'
#' @importFrom  neuralnet neuralnet
#' @importFrom  nnet class.ind
#' @keywords internal
fit_neural_network_categorical <- function(f, data, layers, dependent,
                                           independent, ...){
    # expand categorical dependent variable into dummy variables
    if (!(all(levels(data[[dependent]]) %in% colnames(data)))) {
        identifier <- class.ind(data[[dependent]])
        rownames(identifier) <- rownames(data)
        data <- cbind(data, identifier)
    }

    # recreate model formula with dummy variables
    f <- as.formula(paste(paste(
        levels(data[[dependent]]), collapse = " + "), "~",
        paste(independent, collapse = "+"), sep = " "))
    return(neuralnet(f, data = data, hidden = layers, ...))
}


#' Adds bootstrap data to the model that will be used for plotting.
#'
#' @importFrom future plan multiprocess tweak sequential availableCores
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom parallel makeCluster detectCores stopCluster
#' @keywords internal
add_bootstrap_data <- function(neural_net, options){
    if (isTRUE(options$store)) {
        is_valid_nrepetitions(options$nrepetitions)
        is_valid_probs(options$probs)

        prediction_names <- ifelse(neural_net$type == "categorical",
                                   yes = 2, no = 1)
        parallel <- ifelse(is.null(options$parallel), FALSE, options$parallel)

        num_clusters <- ifelse(isTRUE(parallel), detectCores(), 1)
        clusters <- makeCluster(num_clusters)
        plan_process(parallel, clusters)

        prepared_data <- get_predictors(neural_net, "all") %>%
            map(~ prepare_data(neural_net, .x, options$probs,
                               options$nrepetitions)) %>%
            map(~ gather(.x, "predictor", "values", prediction_names)) %>%
            bind_rows()

        stopCluster(clusters)

        neural_net$stored_data <- prepared_data
        neural_net$options <- options
    } else  if (is.null(options$store)) {
        stop("You have to specify the boolean store value inside the options
             list if you want to store the data!")
    }
    return(neural_net)
}

#' Plot method for neural network.
#'
#' @param x Fitted NeuralNetwork model
#' @param ... further parameters for plot neuralnet, see:
#'   \code{\link[neuralnet]{plot.nn}}
#'
#' @export
plot.NeuralNetwork <- function(x, ...){
    return(plot(x = x$neural_network, ...))
}

#' Predict method for neural network.
#'
#' @param object Fitted NeuralNetwork model
#' @param ... further parameters for predict neuralnet, see:
#'   \code{\link[neuralnet]{predict.nn}}
#'
#' @export
predict.NeuralNetwork <- function(object, ...){
    return(predict(object$neural_network, ...))
}

#' Summary method for neural network.
#'
#' @param object Fitted NeuralNetwork model
#' @param ... further parameters for summary method, see: \code{\link{summary}}
#' @export
summary.NeuralNetwork <- function(object, ...){
    ans <- list(
        formula = object$f, type = object$type, scale = object$scale,
        stored_data = head(object$stored_data, 5),  hidden = object$layers,
        result.matrix = object$neural_network$result.matrix)
    class(ans) <- "summary.NeuralNetwork"
    return(ans)
}
