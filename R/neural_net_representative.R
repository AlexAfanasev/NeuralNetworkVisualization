#' create plotting for multiple predictors
#'
#' @importFrom purrr map2 map
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @keywords internal
plot_multiple <- function (
    neural_net,  predictor, predictor_value, rep, units, kind,
    change_variables = NULL, change_values = NULL, class = NULL) {

    prediction_names <- ifelse(neural_net$type == "categorical",
                               yes = 2, no = 1)

    result <- map2(predictor, predictor_value,
                   ~ prepare_data_repres(neural_net,data, .x ,
                                         predictor_value = .y,
                                         units = units, rep = rep, kind = kind,
                                         change_values = change_values,
                                         change_variables = change_variables,
                                         class = class)) %>%
        map(~ gather(.x, "predictor", "values", prediction_names)) %>%
        bind_rows()

    return(result)
}

#' create plotting data for categorical and numerical variables
#'
#' @keywords internal
prepare_data_repres <- function (
    neural_net, predictor, predictor_value, rep = 100, units = 1, kind = "mean",
    change_variables = NULL, change_values = NULL, class = NULL) {

    data <- create_data(neural_net)
    data <- cbind(neural_net$neural_network$data[class], data)

    if (type == "numerical") {
        result <- numeric_marg(neural_net, data = data, predictor,
                               predictor_value, rep, units, kind,
                               change_variables, change_values,
                               class, type = neural_net$type)
    } else {
        result <- categorical_marg(neural_net, data = data, predictor,
                                   predictor_value, rep, units, kind,
                                   change_variables, change_values,
                                   class, type = type)
    }

    return(result)
}

#' @importFrom purrr pmap_dfc
#' @keywords internal
create_data <- function (neural_net) {
    input_data <- neural_net$neural_network$data
    identifier <- names(input_data)[which(names(input_data) %in%
                                              rownames(neural_net$minmax))]

    diff <- sapply(identifier, function(x) descale(
        x, neural_net, input_data))[1]
    min <- sapply(identifier, function(x) descale(
        x, neural_net, input_data))[2]

    data <- pmap_dfc(list(min,diff, identifier), ~
                         rescale_total(..1,..2,..3,input_data))

    return(data)
}

#' Rescale all selected variables
#'
#' @keywords internal
rescale_total <- function (min, diff, predictor, data) {
    data[predictor] <- data[[predictor]] * diff + min
    return(data[predictor])
}


#' Change input data via change_variables and change_values
#'
#' @importFrom dplyr group_by
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#' @keywords internal
change <- function (data, change_variables, change_values, class,
                    new_data, rep, type) {
    if (type == "numerical") {
        values <- scale_change(data, change_variables, change_values)
        new_data[change_variables]  <- sapply(values, function(x) rep(x, rep))
    } else {
        class_var <- sym(class)
        data_grouped <- data %>% group_by(!!class_var)
        values <- group_scale(data = data_grouped, change_variables,
                              change_values)
        names(values) <- c(class, change_variables)
        new_data[c(class, change_variables)] <- map_dfr(seq(len = rep), ~ values)
    }
    return(new_data)
}

#' Scales grouped datasets
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_modify
#' @keywords internal
group_scale <- function (data, change_variables, change_values) {
    values <- data %>%
        group_modify(~ data.frame(t(scale_change(.x, change_variables,
                                                 change_values))))
    return(values)
}

#' Scale grouped datasets, multiple datasets at once
#'
#' @importFrom dplyr group_split
#' @importFrom purrr map2
#' @keywords internal
group_scale_multiple <- function (data, predictor, predictor_value) {
    data_split <- group_split(data)
    values <- unlist(map2(data_split, predictor_value,
                          ~ scale_change(.x, predictor, .y)))
    return(values)
}

#' Create input grid
#'
#' @keywords internal
make_grid <- function (neural_net, kind, class = NULL, type) {
    neural_input <- neural_net$neural_network$data
    cols_numeric <- sapply(neural_input, is.numeric)

    if (neural_net$type == "numerical") {
        mean_input <- grid_numerical(neural_input, cols_numeric, kind)
    } else {
        mean_input <- grid_categorical(class, neural_input, cols_numeric, kind)
    }

    return(mean_input)
}

#' Customize grid for numeric variables
#'
#' @keywords internal
grid_numerical <- function (neural_input, cols_numeric, kind) {
    mean_input <- data.frame(t(c(rep(NA, length(neural_input)))))
    names(mean_input) <- names(neural_input)

    if (kind == "mean"){
        mean_input[cols_numeric] <- data.frame(
            t(colMeans(neural_input[cols_numeric])))
    } else {
        mean_input[cols_numeric] <- data.frame(
            t(colMedians(neural_input[cols_numeric])))
    }

    return(mean_input)
}

#' customize grid for categorical variables
#'
#' @importFrom dplyr group_by group_modify
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' @keywords internal
grid_categorical <- function (class, neural_input, cols_numeric, kind) {
    class_var <- sym(class)
    index <- names(neural_input[cols_numeric])
    uni <- nrow(unique(neural_input[class]))
    input_matrix <- matrix(rep(NA, length(neural_input) * uni), nrow = uni)
    mean_input <- data.frame((input_matrix))
    names(mean_input) <- names(neural_input)
    process_input <- neural_input[c(class,index)] %>% group_by(!! class_var)

    if (kind == "mean") {
        mean_input[c(class, index)] <- process_input %>%
            group_modify(~ data.frame(t(colMeans(.x))))
    } else {
        mean_input[c(class, index)] <- process_input %>%
            group_modify(~ data.frame(t(colMedians(.x))))
    }

    return(mean_input)
}

#' Columnwise medians
#'
#' @keywords internal
colMedians <- function (x) {
    data <- apply(x, 2, median)
    return(data)
}

#' Create sequence for prediction
#'
#' @keywords internal
sequence <- function (x, rep, units) {
    x <- as.numeric(x)
    data <- seq(x, x + units, len = rep)
    return(data)
}

#' customize grid for dummy variables
#'
#' @importFrom purrr map_dfr
#' @importFrom rlang sym
#' @importFrom dplyr group_by
#' @importFrom magrittr %>%
#' @keywords internal
grid_factor <- function (neural_net, mean_input,class, type, rep) {
    neural_input <- neural_net$neural_network$data
    cols_dummy <- sapply(neural_input, function(x) length(unique(x)) == 2)

    if (type == "numerical") {
        mean_input[cols_dummy] <- data.frame(
            t(colModes(neural_input[cols_dummy])))
        new_data = map_dfr(seq_len(rep), ~ mean_input)
    } else {
        class_var <- sym(class)
        index <- names(neural_input[cols_dummy])
        neural_input <- neural_input %>% group_by(!! class_var)
        mean_input[c(class,index)] <- neural_input[c(class,index)] %>%
            group_modify(~ data.frame(t(colModes(.x))))
        new_data <- map_dfr(seq_len(rep), ~ mean_input)
    }

    return(new_data)
}

#' Columnwise modes
#'
#' @importFrom dplyr group_by
#' @keywords internal
colModes <- function (x) {
    data <- apply(x, 2, mode)
    return(data)
}

#' Mode
#'
#' @importFrom dplyr group_by
#' @keywords internal
mode <- function(x) {
    uni <- unique(x)
    return(uni[which.max(tabulate(match(x, uni)))])
}

#' Issue warnings
#'
#' @keywords internal
warning_message <- function (
    kind, predictor_value, data, predictor, change_variables, change_values) {
  if (!is.null(change_variables) && !is.null(change_values)) {
      if (length(change_variables) != length(change_values)) {
          stop("Change_values and change_variables must have same length!")
      }
  }

  if (!(kind %in% c("mean", "median"))) {
      stop("Please select either mean or median")
  }

  if (predictor %in% change_variables){
      stop("predictor cannot be part of change_variables")
  }
}

#' Scale change_variables
#'
#' @importFrom purrr map2
#' @keywords internal
scale_change <- function (data, change_variables, change_values) {
    scale <- unlist(map2(change_variables, change_values,
                         ~ as.numeric(scale(
                             .y, center = compact((min(data[[.x]]))),
                             scale = compact(max(data[[.x]]) -
                                                 min(data[[.x]]))))))
    return(scale)
}

#' Create plotting data for numeric
#'
#' @importFrom neuralnet compute
#' @keywords internal
numeric_marg <- function(
    neural_net, data, predictor, predictor_value, rep = 100, units = 1,
    kind = "mean", change_variables = NULL, change_values = NULL,
    class = NULL,type = NULL) {

    warning_message(kind, predictor_value, data, predictor,
                    change_variables, change_values)
    mean_input <- make_grid(neural_net, kind, type = type, class)
    new_data <- grid_factor(neural_net, mean_input, class, rep,type = type)

    if (!is.null(change_values) && !is.null(change_variables)) {
        new_data <- change(data, change_variables, change_values, class,
                           new_data, rep, type = type)
     }

    if (isTRUE(neural_net$scale)) {
        predictor_value_scale <- scale_change(data, predictor, predictor_value)
        new_data[predictor] <- seq(predictor_value_scale,
                                   predictor_value_scale + units, len = rep)
    } else {
        new_data[predictor] <- rep(predictor_values, rep)
    }

    new_data$yhat <- neuralnet::compute(neural_net$neural_network, new_data)$net.result
    new_data[predictor] <- sequence(predictor_value, rep, units)
    result <- data.frame(new_data[predictor], yhat = new_data$yhat)
    return(result)
}

#' Create plotting data for categorical variables
#'
#' @importFrom purrr map2
#' @importFrom rlang sym
#' @importFrom dplyr group_by arrange
#' @importFrom magrittr %>%
#' @keywords internal
categorical_marg <- function(
    neural_net, data, predictor, predictor_value, rep = 100, units = NULL,
    kind = "mean", change_variables = NULL, change_values = NULL, class = NULL,
    type) {

    warning_message(kind, predictor_value, data, predictor, change_variables,
                    change_values)
    mean_input <- make_grid(neural_net, kind, type = type, class)
    new_data <- grid_factor(neural_net, mean_input, class, rep,type = type)
    class_var <- sym(class)

    if (!is.null(change_values) && !is.null(change_variables)) {
        new_data <- change(data, change_variables, change_values, class,
                           new_data, rep, type = type)}

    if (isTRUE(neural_net$scale)) {
        data <- data %>% group_by(!! class_var)
        predictor_value_scale <- group_scale_multiple(data, predictor,
                                                      predictor_value)
        new_data <- new_data %>% arrange(!! class_var)
        new_data[predictor] = unlist(map2(predictor_value_scale, units,
                                          ~ sequence(.x,rep,.y)))
    } else {
        new_data[predictor] <- rep(predictor_value, rep)
        new_data <- new_data %>% arrange(!! class_var)
        new_data[predictor] <- unlist(map2(predictor_value, units,
                                           ~ sequence(.x,rep,.y)))
    }

    result <- categ_prediction(neural_net, new_data, class, predictor,
                               predictor_value, rep)
    return(result)
}


#' Create prediction data for categorical variables
#'
#' @importFrom purrr map2
#' @importFrom neuralnet compute
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace
#' @importFrom dplyr bind_cols ends_with
#' @keywords internal
categ_prediction <- function (
    neural_net, new_data, class, predictor, predictor_value, rep, units) {

    prediction <- data.frame(
        compute(neural_net$neural_network, new_data)$net.result)
    names(prediction) <- paste(neural_net$neural_network$model.list$response,
                               "_prediction", sep = "")
    new_data <- new_data %>%
        bind_cols(prediction) %>%
        gather(class, prediction, ends_with("prediction")) %>%
        mutate(class = str_replace(class, "_prediction", ""))
    new_data[predictor] <- unlist(map2(predictor_value, units,
                                       ~ sequence(.x,rep,.y)))

    return(new_data)
}
