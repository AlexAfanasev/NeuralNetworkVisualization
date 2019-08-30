#' Prepares the data used for further plotting.
#'
#' \code{prepare_data} returns the prepared data used for further plotting.
#'
#' @param neural_net Fitted NeuralNetwork, see: \code{\link{NeuralNetwork}}
#' @param predictor Predictor for which to prepare the plotting data (predictor
#'   should be transformed with sym).
#' @param probs Vector of lower and upper bound probabilities for the confidence
#'   interval.
#' @param nrepetitions Number of samples used within bootstrap for confidence
#'   intervals.
#'
#' @return Data.frame with the prepared data.
#'
#' @name prepare_data
#' @keywords internal
prepare_data <- function(neural_net, predictor, probs = c(0.05, 0.95),
                         nrepetitions = 20){
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
create_grid <- function(neural_net, predictor){
    grid_predictor <- select(neural_net$neural_network$data, !!predictor)
    grid_input <- select(neural_net$neural_network$data, -!!predictor)
    grid <- crossing(grid_predictor, grid_input)
    return(grid)
}

#' Creates the plotting data.
#'
#' @keywords internal
create_plotting_data <- function(grid, predictor, neural_net, probs,
                                 nrepetitions){
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
prepare_data_numeric <- function(grid, predictor, neural_net, probs,
                                 nrepetitions){
    partial_dependence <- compute_mean_prediction_numeric(
        grid, neural_net, predictor)

    if (isTRUE(all(probs == 0))) {
        partial_dependence[, c("lwr", "upr")] <- partial_dependence$yhat
    } else {
        partial_dependence <- compute_bootstrap_ci(
            partial_dependence, grid, predictor, neural_net, probs,
            nrepetitions)
    }

    return(partial_dependence)
}

#' Computes mean prediction for numeric response
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate group_by summarize
#' @importFrom plotly ungroup
#' @importFrom neuralnet compute
#' @importFrom rlang sym
#' @keywords internal
compute_mean_prediction_numeric <- function(grid, neural_net, predictor){
    grid <- grid %>% mutate(prediction = compute(
             neural_net$neural_network, grid)$net.result)

    if (isTRUE(neural_net$scale)) {
        grid <- descale_grid(predictor, neural_net, grid)
    }

    partial_dependence <- grid %>%
        group_by(!!predictor) %>%
        summarize(yhat = mean(prediction)) %>%
        ungroup()

    return(partial_dependence)
}

#' Descales selected predictor column and prediction value for grid.
#'
#' @keywords internal
descale_grid <- function(predictor, neural_net, grid){
    identifier <- as.character(predictor) == (rownames(
        neural_net$min_and_max_numeric_columns))

    predictor_min <- neural_net$min_and_max_numeric_columns$min[identifier]
    predictor_max <- neural_net$min_and_max_numeric_columns$max[identifier]
    difference <- predictor_max - predictor_min

    grid <- mutate(grid, !! sym(predictor) :=
                       !!sym(predictor) * difference + predictor_min)

    if (neural_net$type == "numerical") {
        identifier <- neural_net$dependent == (rownames(
            neural_net$min_and_max_numeric_columns))

        prediction_min <- neural_net$min_and_max_numeric_columns$min[identifier]
        prediction_max <- neural_net$min_and_max_numeric_columns$max[identifier]
        difference <- prediction_max - prediction_min

        grid <- mutate(grid, prediction = prediction * difference +
                           prediction_min)
    }
    return(grid)
}

#' Prepares the data with categorical dependent variable.
#'
#' @keywords internal
prepare_data_categoric <- function(grid, predictor, neural_net, probs,
                                   nrepetitions){
    partial_dependence <- compute_mean_prediction_categoric(
        grid, neural_net, predictor)

    if (all(probs == 0) == TRUE) {
        partial_dependence[, c("lwr", "upr")] <- partial_dependence$yhat
    } else {
        partial_dependence <- compute_bootstrap_ci(
            partial_dependence, grid, predictor, neural_net, probs,
            nrepetitions)
    }

    partial_dependence$class <- as.factor(partial_dependence$class)
    return(partial_dependence)
}

#' Computes mean prediction for categoric response.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate group_by summarize_at bind_cols ends_with rename
#' @importFrom stringr str_replace
#' @importFrom tidyr gather
#' @importFrom ggplot2 vars
#' @importFrom plotly ungroup
#' @importFrom neuralnet compute
#' @importFrom rlang sym
#' @keywords internal
compute_mean_prediction_categoric <- function(grid, neural_net, predictor){
    prediction <- as.data.frame(
        compute(neural_net$neural_network, grid)$net.result)
    names(prediction) <- paste(neural_net$neural_network$model.list$response,
                               "_prediction", sep = "")
    grid <- grid %>%
        bind_cols(prediction) %>%
        gather(class, prediction, ends_with("prediction")) %>%
        mutate(class = str_replace(class, "_prediction", ""))

    if (isTRUE(neural_net$scale)) {
        grid <- descale_grid(predictor, neural_net, grid)
    }

    partial_dependence <- grid  %>%
        group_by(class, !!predictor) %>%
        summarize_at(vars(prediction), mean) %>%
        rename(yhat = prediction) %>%
        ungroup()

    return(partial_dependence)
}

#' computes Bootstrap intervals.
#'
#' @importFrom magrittr %>%
#' @importFrom neuralnet compute
#' @importFrom dplyr bind_cols mutate summarize_at group_by ends_with ungroup
#' @importFrom tidyr gather
#' @importFrom stringr str_replace
#' @importFrom rlang sym
#' @keywords internal
compute_bootstrap_ci <- function(partial_dependence, grid, predictor,
                                 neural_net, probs, nrepetitions){
    number_of_data_points <- nrow(neural_net$neural_network$data)
    bootstrap_data <- matrix(nrow = nrow(partial_dependence),
                             ncol = nrepetitions)

    for (current_rep in 1:nrepetitions) {
        indices <- sample(1:number_of_data_points, size = number_of_data_points,
                          replace = TRUE)
        descaled_data <- descale_data(neural_net)
        resampled_data_set <- descaled_data[indices, ]
        args <- c(list(f = neural_net$f, data = resampled_data_set,
                       layers = neural_net$layers, scale = neural_net$scale),
                  neural_net$additional)
        new_neural_net <- do.call(NeuralNetwork, args)

        if (new_neural_net$type == "numerical") {
            new_partial_dependence <- compute_mean_prediction_numeric(
                grid, new_neural_net, predictor)
        } else {
            new_partial_dependence <- compute_mean_prediction_categoric(
                grid, new_neural_net, predictor)
        }

        bootstrap_data[, current_rep] <- new_partial_dependence$yhat
    }

    partial_dependence$yhat <- apply(bootstrap_data, 1, mean)
    partial_dependence[, c("lwr", "upr")] <- t(apply(bootstrap_data, 1,
                                                     quantile, probs = probs))
    return(partial_dependence)
}

#' Descales data for neural_net
#'
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
descale_data <- function(neural_net){
    data <- neural_net$neural_network$data
    if (isTRUE(neural_net$scale)) {
        for (column_name in rownames(neural_net$min_and_max_numeric_columns)) {
            identifier <- column_name == (rownames(
                neural_net$min_and_max_numeric_columns))

            predictor_min <- neural_net$min_and_max_numeric_columns$min[
                identifier]
            predictor_max <- neural_net$min_and_max_numeric_columns$max[
                identifier]
            difference <- predictor_max - predictor_min

            data <- mutate(data, !!sym(column_name) := !!sym(column_name) *
                               difference + predictor_min)
        }
    }
    return(data)
}
