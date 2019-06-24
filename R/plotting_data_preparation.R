#' Prepares the data used for plotting.
#'
#' \code{prepare_data} Returns the prepared data used for plotting.
#'
#' @param neural_net Fitted NeuralNetwork, see: \code{\link{NeuralNetwork}}
#' @param predictor Predictor for which to prepare the plotting data
#'   (predictor should be transformed with sym)
#' @param probs Vector of 2 probabilities for the confidence intervals
#'
#' @return Data.frame with the prepared data. This data will be used for
#'   plotting.
#'
#' @importFrom dplyr select mutate group_by summarize bind_cols ends_with summarize_at vars funs rename
#' @importFrom tidyr crossing gather
#' @importFrom magrittr %>%
#' @importFrom neuralnet compute
#' @importFrom stringr str_replace
#' @importFrom purrr partial set_names
#' @name prepare_data
#' @keywords internal
prepare_data <- function (neural_net, predictor, probs = c(0.05, 0.95)) {
    grid <- create_grid(neural_net, predictor)
    plotting_data <- create_plotting_data(grid, predictor, neural_net, probs)
    return(plotting_data)
}

#' Creates the grid used for predicting yhat.
#' @keywords internal
create_grid <- function (neural_net, predictor) {
    # dplyr:: is required as dplyr and neuralnet share functions
    grid_predictor <- dplyr::select(neural_net$neural_network$data, !!predictor)
    grid_input <- dplyr::select(neural_net$neural_network$data, -!!predictor)
    grid <- crossing(grid_predictor, grid_input)
    return(grid)
}

#' Creates the plotting data.
#' @keywords internal
create_plotting_data <- function (grid, predictor, neural_net, probs) {
    if (neural_net$type == "numerical") {
        plotting_data <- prepare_data_numeric(grid, predictor, neural_net,
                                              probs)
    } else {
        plotting_data <- prepare_data_categoric(grid, predictor, neural_net,
                                                probs)
    }
    return(plotting_data)
}

# TODO: Simplify this!!!
#' Prepares the data with numerical dependent variable.
#' @keywords internal
prepare_data_numeric <- function (grid, predictor, neural_net, probs) {
    grid <- grid %>% mutate(prediction = compute(
        neural_net$neural_network, grid)$net.result)

    quantile_functions <- map(
        probs, ~partial(compute_boot_ci, probs = .x))
    if (all(probs == 0)) {
        functions <- c(mean, mean, mean)
    } else {
        functions <- c(mean, quantile_functions)
    }
    functions <- functions  %>%
        set_names(nm = c("yhat", "lwr", "upr"))

    # TODO: This is a really ugly hack!!!
    library(multidplyr)
    library(dplyr)
    f1 <- paste("partial_dependence <- grid %>% partition(",
                paste(as.character(predictor), collapse = ""),
                ") %>% add_boot_ci(funcs = functions) %>% collect()", sep = "")
    f2 <- eval(parse(text = f1))
    # partial_dependence <- grid %>%
    #     partition(!!predictor, cluster = cluster) %>%
    #     add_boot_ci(probs = probs)
    return(f2)
}

#' Prepares the data with categorical dependent variable.
#' @keywords internal
prepare_data_categoric <- function (grid, predictor, neural_net, probs) {
    prediction <- as.data.frame(
        compute(neural_net$neural_network, grid)$net.result)
    names(prediction) <- paste(neural_net$neural_network$model.list$response,
                               "_prediction", sep = "")
    grid <- grid %>% bind_cols(prediction)
    partial_dependence <- grid %>%
        gather(class, prediction, ends_with("prediction")) %>%
        mutate(class = str_replace(class, "_prediction", ""))
    quantile_functions <- map(
        probs, ~partial(compute_boot_ci, probs = .x))
    if (all(probs == 0)) {
        functions <- c(mean, mean, mean)
    } else {
        functions <- c(mean, quantile_functions)
    }
    functions <- functions  %>%
        set_names(nm = c("yhat", "lwr", "upr"))

    library(multidplyr)
    library(dplyr)
    f1 <- paste("partial_dependence <- partial_dependence %>% partition(",
                paste("class, ", as.character(predictor), collapse = ""),
                ") %>% add_boot_ci(funcs = functions) %>% collect()", sep = "")
    f2 <- eval(parse(text = f1))
        # group_by(class, !!predictor) %>%
        # summarize(yhat = mean(prediction),
        #           lwr = ifelse(all(probs == 0), mean(prediction),
        #                        quantile(prediction, probs = probs[1])),
        #           upr = ifelse(all(probs == 0), mean(prediction),
        #                        quantile(prediction, probs = probs[2])))
    partial_dependence$class <- as.factor(partial_dependence$class)
    return(partial_dependence)
}

#' Adds bootstrap confidence intervals.
#' @keywords internal
add_boot_ci <- function (grouped_data, funcs) {
    return(summarize_at(grouped_data, vars(prediction), funcs))
}

#' Computes bootstrap interval for given probability.
#' @keywords internal
compute_boot_ci <- function (bootstrap_data, probs) {
    nrepititions <- 100
    number_of_data_points <- nrow(bootstrap_data)
    bootstrap_resulting_data <- matrix(nrow = nrepititions, ncol = 1)
    for (current_rep in 1:nrepititions) {
        indices <- sample(1:number_of_data_points, size = number_of_data_points,
                          replace = TRUE)
        bootstrap_resulting_data[current_rep, ] <- mean(bootstrap_data[indices])
    }
    return(quantile(bootstrap_resulting_data, probs = probs))
}
