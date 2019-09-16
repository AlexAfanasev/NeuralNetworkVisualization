#' NeuralNetworkVisualization: Create Partial Dependence Plots for Neural Networks.
#'
#' NeuralNetworkVisualization is a R Package for creating beautiful partial
#'   dependence plots for Neural Networks using ggplot2, plotly and shiny.
#'   It is possible to add confidence intervals created with a bootstrap
#'   procedure. There are three prefitted models that can be used for testing.
#'   You can acess them with the example_nn_model function.
#'
#' @docType package
#' @name NeuralNetworkVisualization
NULL

#' Returns example models by type of response.
#'
#' Each model has stored data available with 1000 bootstrap iterations and a
#' 90% confidence interval. The code that fitted the models can be found here:
#' \url{https://github.com/AlexAfanasev/NeuralNetworkVisualization/blob/master/inst/examples.R}
#'
#' @return NeuralNetwork class
#'
#' @examples
#' \dontrun{
#' # Example: Numeric
#' example_nn_model("numerical")
#'
#' # Example: Categoric
#' example_nn_model("categorical")
#'
#' Example: Binary
#' example_nn_model("binary")
#'
#' }
#'
#' @param type Either numerical, categorical or binary
#'
#' @name example_nn_model
#' @export
example_nn_model <- function(type){
    if (type %in% c("categorical", "binary", "numerical")) {
        model_file_directory <- system.file(
            "models", package = "NeuralNetworkVisualization")
        model_file <- list.files(model_file_directory, full.names = TRUE)[
            which(list.files(model_file_directory) == paste(
                type, ".rds", sep = ""))]
        model <- readRDS(file = model_file)
        return(model)
    } else {
        stop("Please specify either numerical, categorical or binary!")
    }
}
