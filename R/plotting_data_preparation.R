#' @keywords internal
#' Returns the prepared data used for plotting
prepare_data <- function (neural_net, predictor) {
    # dplyr:: is required as tidyverse and neuralnet share functions
    grid_predictor <- dplyr::select(neural_net$neural_network$data, !!predictor)
    grid_input <- dplyr::select(neural_net$neural_network$data, -!!predictor)
    grid <- crossing(grid_predictor, grid_input)
    if (neural_net$type == "numerical") {
        return(prepare_data_numeric(grid, predictor, neural_net))
    } else {
        return(prepare_data_categoric(grid, predictor, neural_net))
    }
}


#' @keywords internal
#' Prepares the data with numerical dependent variable
prepare_data_numeric <- function (grid, predictor, neural_net) {
    grid <- grid %>% mutate(prediction = neuralnet::compute(
        neural_net$neural_network, grid)$net.result)
    partial_dependence <- grid %>%
        group_by(!!predictor) %>%
        summarize(yhat := mean(prediction))
    return(partial_dependence)
}

#' @keywords internal
#' Prepares the data with categorical dependent variable
prepare_data_categoric <- function (grid, predictor, neural_net) {
    prediction <- as.data.frame(
        neuralnet::compute(neural_net$neural_network, grid)$net.result)
    names(prediction) <- paste(neural_net$neural_network$model.list$response,
                               "_prediction", sep = "")
    grid <- grid %>% bind_cols(prediction)
    partial_dependence <- grid %>%
        gather(class, prediction, ends_with("prediction")) %>%
        mutate(class = str_replace(class, "_prediction", ""))%>%
        group_by(class, !!predictor) %>%
        summarize(yhat = mean(prediction))
    partial_dependence$class <- as.factor(partial_dependence$class)
    return(partial_dependence)
}
