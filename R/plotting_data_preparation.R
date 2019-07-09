#' Prepares the data used for further plotting.
#'
#' \code{prepare_data} returns the prepared data used for further plotting.
#'
#' @param neural_net Fitted NeuralNetwork, see: \code{\link{NeuralNetwork}}
#' @param predictor Predictor for which to prepare the plotting data (predictor
#'   should be transformed with sym).
#' @param probs Vector of ower and upper bound probabilities for the confidence
#'   interval.
#' @param nrepetitions Number of samples used within bootstrap for confidence
#'   intervals.
#'
#' @return Data.frame with the prepared data.
#'
#' @name prepare_data
#' @keywords internal
prepare_data <- function (neural_net, predictor, probs = c(0.05, 0.95),
                          nrepetitions = 300) {
    grid <- create_grid(neural_net, predictor)
    plotting_data <- create_plotting_data(grid, predictor, neural_net, probs,
                                          nrepetitions)
    return(plotting_data)
}

#' Creates the grid used for predicting yhat.
#'
#' @importFrom dplyr select
#' @importFrom tidyr crossing
#' @keywords internal
create_grid <- function (neural_net, predictor) {
    grid_predictor <- select(neural_net$neural_network$data, !!predictor)
    grid_input <- select(neural_net$neural_network$data, -!!predictor)
    grid <- crossing(grid_predictor, grid_input)
    return(grid)
}

#' Creates the plotting data.
#'
#' @keywords internal
create_plotting_data <- function (grid, predictor, neural_net, probs,
                                  nrepetitions) {
    if (neural_net$type == "numerical") {
        plotting_data <- prepare_data_numeric(grid, predictor, neural_net,
                                              probs, nrepetitions)
    } else {
        plotting_data <- prepare_data_categoric(grid, predictor, neural_net,
                                                probs, nrepetitions)
    }
    return(plotting_data)
}

#' Prepares the data with numerical dependent variable.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate summarize_at group_by ungroup
#' @importFrom neuralnet compute
#' @keywords internal
prepare_data_numeric <- function (grid, predictor, neural_net, probs,
                                  nrepetitions) {
    functions <- create_functions(probs, nrepetitions)
    grid <- grid %>% mutate(prediction = compute(
        neural_net$neural_network, grid)$net.result)
    partial_dependence <- grid %>%
        group_by(!!predictor) %>%
        summarize_at(vars(prediction), functions) %>%
        ungroup()
    return(partial_dependence)
}

#' Prepares the data with categorical dependent variable.
#'
#' @importFrom magrittr %>%
#' @importFrom neuralnet compute
#' @importFrom dplyr bind_cols mutate summarize_at group_by ends_with ungroup
#' @importFrom tidyr gather
#' @importFrom stringr str_replace
#' @keywords internal
prepare_data_categoric <- function (grid, predictor, neural_net, probs,
                                    nrepetitions) {
    prediction <- as.data.frame(
        compute(neural_net$neural_network, grid)$net.result)
    names(prediction) <- paste(neural_net$neural_network$model.list$response,
                               "_prediction", sep = "")
    functions <- create_functions(probs, nrepetitions)
    grid <- grid %>%
        bind_cols(prediction) %>%
        gather(class, prediction, ends_with("prediction")) %>%
        mutate(class = str_replace(class, "_prediction", ""))
    partial_dependence <- grid  %>%
        group_by(class, !!predictor) %>%
        summarize_at(vars(prediction), functions) %>%
        ungroup()
    partial_dependence$class <- as.factor(partial_dependence$class)
    return(partial_dependence)
}

#' Create functions used for calculating relevant metrics.
#'
#' @importFrom magrittr %>%
#' @importFrom purrr partial set_names
#' @keywords internal
create_functions <- function (probs, nrepetitions) {
    if (all(probs == 0)) {
        functions <- c(mean, mean, mean)
    } else {
        functions <- c(mean, map(probs, ~partial(compute_boot_ci, probs = .x,
                                                 nrepetitions = nrepetitions)))
    }
    functions <- functions  %>% set_names(nm = c("yhat", "lwr", "upr"))
    return(functions)
}

#' Computes bootstrap interval for given probability.
#'
#' @keywords internal
compute_boot_ci <- function (bootstrap_data, probs, nrepetitions) {
    number_of_data_points <- length(bootstrap_data)
    bootstrap_resulting_data <- matrix(nrow = nrepetitions, ncol = 1)
    for (current_rep in 1:nrepetitions) {
        indices <- sample(1:number_of_data_points, size = number_of_data_points,
                          replace = TRUE)
        bootstrap_resulting_data[current_rep, ] <- mean(bootstrap_data[indices])
    }
    return(quantile(bootstrap_resulting_data, probs = probs))
}
