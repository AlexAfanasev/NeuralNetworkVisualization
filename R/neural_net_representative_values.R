#' Function for plotting the marginal effect at representative values
#'  of the created NeuralNetwork
#'
#' \code{plot_marginal_repres} plots the MER for
#'   the specified predictors.
#'
#' @param neural_net The NeuralNetwork instance, see:
#'   \code{\link{NeuralNetwork}}
#' @param predictor Vector of predictors of the neural network for which to
#'   plot the partial dependencies.
#' @param probs Vector of lower and upper bound probabilities for the confidence
#'   interval. If both are 0, intervals will not be plotted.
#' @param nrepetitions Number of samples used within bootstrap for confidence
#'   intervals.
#' @param plot_type Either 'ggplot' if the plot should be created using ggplot or
#'   'ggplotly' if plotly should be used.
#' @param predictor_value Value of interest for marginal effect
#' @param change_values Value to hold change_variable constant
#' @param change_variables Variable to be held constant
#' @param units units of change for marginal effect
#' @param rep Number of data points to be plotted for marginal effect
#' @param class class for categorical variable
#' @param kind Type of holding the other variables constant (mean, median)
#' @return Created figure
#'
#' @examples
#' \dontrun{
#' # Example: Numeric or Binary
#' library(MASS)
#' neural_network <- NeuralNetwork(f = "medv ~ .", data = Boston,
#'                                 layers = c(5, 3), scale = TRUE,
#'                                 linear.output = TRUE)
#' plot_marginal_repres(neural_network, predictor = "crim", predictor_value = 2,
#' probs = c(0.1,0.9), nrepetitions = 3, kind = "mean",
#' rep = 100, units = 1, change_values = NULL, change_variables = NULL)
#' plot_marginal_repres(neural_network, predictor = c("crim", "age"),
#' predictor_value = c(2, 35), change_values = 190, change_variables = "tax")
#' plot_marginal_repres(neural_network, "crim", 3, kind = "median",
#' probs = c(0.2, 0.8))
#'
#' # Example: Categoric
#' library(datasets)
#' model <- NeuralNetwork(
#'    Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
#'    data = iris, layers = c(10, 10), rep = 5, err.fct = "ce",
#'    linear.output = FALSE, lifesign = "minimal", stepmax = 1000000,
#'    threshold = 0.001, scale = F)
#'
#' plot_marginal_repres(model, predictor = "Petal.Length",
#' predictor_value = c(5,6,5), class = "Species")
#' plot_marginal_repres(model, predictor = c("Sepal.Length", "Petal.Length"),
#' predictor_value = c(1,2,3,4,5,6), class = "Species")
#' plot_marginal_repres(model,"Sepal.Width", c(2,3), class = "Species")
#' }
#' @importFrom plotly ggplotly
#' @name plot_marginal_repres
#' @export
plot_marginal_repres <- function(neural_net, predictor, predictor_value = NULL,
                                  rep = 100, units = 1, kind = "mean",
                                  change_variables = NULL,
                                  change_values = NULL, class = NULL,
                                  probs = c(0, 0), plot_type = "ggplot",
                                  nrepetitions = 5) {
    if (is.null(predictor_value)) {
        predictor_value <- empty_predictor_value(neural_net, predictor,
                                                 class)
    }
    if (length(predictor) > 1) {
        figure <- plot_multiple_res(neural_net, predictor,
                                    predictor_value = predictor_value,
                                    rep, units, kind, change_variables,
                                    change_values, class, probs, nrepetitions)
    } else {

        figure <- plot_single_res(neural_net, predictor,
                                  predictor_value = predictor_value,
                                  rep, units, kind, change_variables,
                                  change_values, class, probs, nrepetitions)
    }
    if (plot_type == "ggplot") {
        return(figure)
    } else {
        return(ggplotly(figure))
    }
}

#' rescale all selected variables
#' @keywords internal
rescale_total <- function(min, diff, predictor, data) {
    data[predictor] <- data[[predictor]] * diff + min
    return(data[predictor])
}

#' rescale all selected variables
#' @importFrom purrr pmap_dfc
#' @keywords internal
#'
create_data <- function(neural_net) {
    if (isTRUE(neural_net$scale)) {
    input_data <- neural_net$neural_network$data
    index <- which(names(input_data) %in%
                       rownames(neural_net$min_and_max_numeric_columns))
    identifier <- names(input_data)[index]
    min <- neural_net$min_and_max_numeric_columns[identifier, "min"]
    max <- neural_net$min_and_max_numeric_columns[identifier, "max"]
    diff <- max - min
    data <- pmap_dfc(list(min, diff, identifier), ~ rescale_total(..1,..2,..3,
                                                               input_data))
    } else {
    data <- neural_net$neural_network$data
    }
    return(data)
}

#' change input data via change_variables and change_values
#' @importFrom dplyr group_by count
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#' @keywords internal
#'
change <- function(data, change_variables, change_values, class,
                   new_data, rep, type) {
    if (type == "numerical") {
        values <- scale_change(data, change_variables, change_values)
        new_data[change_variables] <- sapply(values, function(x) rep(x, rep))
    }
    if (type == "categorical") {
        class_var <- sym(class)
        count <- count(new_data %>% group_by(!! class_var))$n
        len <- length(change_variables)
        change_values <- data.frame(matrix(rep(change_values,
                                               rep(count, len)), ncol = len))
        names(change_values) <- change_variables
        min <- as.numeric(min_max(data)[change_variables, 1])
        diff <- as.numeric(min_max(data)[change_variables, 2])-
                as.numeric(min_max(data)[change_variables, 1])
        new_data[change_variables] <- change_values[change_variables]
        new_data[change_variables] <- pmap(list(change_variables, min, diff),~
                                        scale(new_data[..1], center = ..3,
                                              scale = ..2))
    }
    return(new_data)
}

#' scale grouped datasets, multiple datasets at once
#' @importFrom dplyr group_split
#' @importFrom purrr map2
#' @keywords internal
#'
group_scale_multiple <- function(data, predictor, predictor_value) {
    data_split <- group_split(data)
    values <- unlist(map2(data_split, predictor_value,
                        ~ scale_change(.x, predictor, .y)))
    return(values)
}

#' create input grid
#' @keywords internal
#'
make_grid <- function(neural_net, kind, class = NULL, type) {
    neural_input <- neural_net$neural_network$data
    cols_numeric <- sapply(neural_input, is.numeric)
    if (type == "numerical") {
        mean_input <- grid_numerical(neural_input, cols_numeric, kind)
    }
    if (type == "categorical") {
        mean_input <- grid_categorical(class, neural_input, cols_numeric, kind)
    }
    return(mean_input)
}

#' customize grid for numeric variables
#' @keywords internal
#'
grid_numerical <- function(neural_input, cols_numeric, kind) {
    mean_input <- data.frame(t(c(rep(NA, length(neural_input)))))
    names(mean_input) <- names(neural_input)
    if (kind == "mean") {
        mean_input[cols_numeric] <- data.frame(t(
        colMeans(neural_input[cols_numeric])))
    } else {
        col_means <- t(colMedians(neural_input[cols_numeric]))
        mean_input[cols_numeric] <- data.frame(col_means)
    }
    return(mean_input)
}

#' customize grid for categorical variables
#' @importFrom dplyr group_by group_modify
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' @keywords internal

grid_categorical <- function(class, neural_input, cols_numeric, kind) {
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

#' columnwise medians
#' @keywords internal
#'
colMedians <- function(x) {
    data <- apply(x, 2, median)
    return(data)
}

#' create sequence for prediction
#' @keywords internal
#'
sequence <- function(x, rep, units) {
    x <- as.numeric(x)
    data <- seq(x, x + units, len = rep)
    return(data)
}

#' customize grid for dummy variables
#' @importFrom purrr map_dfr
#' @importFrom rlang sym
#' @importFrom dplyr group_by
#' @importFrom magrittr %>%
#' @keywords internal
#'
grid_factor <- function(neural_net, mean_input, class, type, rep) {
    neural_input <- neural_net$neural_network$data
    cols_dummy <- sapply(neural_input, function(x) length(unique(x)) == 2)
    if (type == "numerical") {
        col_means <- t(colModes(neural_input[cols_dummy]))
        mean_input[cols_dummy] <- data.frame(col_means)
        new_data <- map_dfr(seq_len(rep), ~ mean_input)
    }
    if (type == "categorical") {
        class_var <- sym(class)
        index <- names(neural_input[cols_dummy])
        neural_input <- neural_input %>% group_by(!! class_var)
        mean_input[c(class,index)] <- neural_input[c(class,index)] %>%
        group_modify(~ data.frame(t(colModes(.x))))
        new_data <- map_dfr(seq_len(rep), ~ mean_input)
    }
    return(new_data)
}

#' columnwise modes
#' @importFrom dplyr group_by
#' @keywords internal
colModes <- function(x) {
    data <- apply(x, 2, mode)
    return(data)
}

#' mode
#' @importFrom dplyr group_by
#' @keywords internal
#'
mode <- function(x) {
    uni <- unique(x)
    return(uni[which.max(tabulate(match(x, uni)))])
}

#' issue warnings
#' @keywords internal
#'
warning_message <- function(neural_net, kind, predictor_value, predictor,
                           change_variables, change_values, type, class) {
    if (!kind %in% c("mean", "median")) {
        stop("Please select either mean or median")
    }
    if (predictor %in% change_variables) {
        stop("predictor cannot be part of change_variables")
    }
    if (type == "categorical") {
    input <- neural_net$neural_network$model.list$response
        if (is.null(class)) {
            stop("Please do not forget to specify class")
        }
        if (!is.null(change_variables) & !is.null(change_values)) {
            if (length(change_values) !=
                length(input) * length(change_variables)) {
                stop("Length of change_value must be multiple  of
                     length of classes")
            }
        }
    }
    if (type == "numerical") {
        if (!is.null(change_variables) & !is.null(change_values)) {
            if (length(change_variables) != length(change_values)) {
                stop("Change_values and change_variables must have same length")
            }
        }
    }
}


#' scale change_variables
#' @importFrom purrr map2
#' @keywords internal
#'
scale_change <- function(data, change_variables, change_values) {
    scale <- unlist(map2(change_variables, change_values,
                      ~ as.numeric(scale(.y,
                        center = compact((min(data[[.x]]))),
                        scale = compact(max(data[[.x]]) - min(data[[.x]]))))))
    return(scale)
}

#' create plotting data for numeric
#' @keywords internal
#'

numeric_marg <- function(neural_net, data, predictor,
                        predictor_value, rep, units, kind ,
                        change_variables , change_values ,
                        class , type) {

    warning_message(neural_net, kind, predictor_value = predictor_value ,
                    predictor, change_variables, change_values, type = type,
                    class )
    mean_input <- make_grid(neural_net, kind, type = type, class)
    new_data <- grid_factor(neural_net, mean_input, class, rep,type = type)
    if (!is.null(change_values) & !is.null(change_variables)) {
        new_data <- change(data, change_variables, change_values, class,
                        new_data, rep, type = type)
    }
    if (isTRUE(neural_net$scale)) {
        predictor_value_scale <- as.numeric(scale_change(data, predictor,
                                              predictor_value))
        new_data[predictor] <- seq(predictor_value_scale,
                             predictor_value_scale + units, len = rep)
    } else {
        new_data[predictor] <- seq(predictor_value, predictor_value + units,
                                   len = rep)
    }
    new_data$yhat <- neuralnet::compute(neural_net$neural_network,
                                      new_data)$net.result
    identifier <- neural_net$dependent == (rownames(
        neural_net$min_and_max_numeric_columns))
    prediction_min <- neural_net$min_and_max_numeric_columns$min[identifier]
    prediction_max <- neural_net$min_and_max_numeric_columns$max[identifier]
    difference <- prediction_max - prediction_min
    new_data <- mutate(new_data, yhat = yhat * difference +
                       prediction_min)
    new_data[predictor] <- sequence(predictor_value,
                                    rep, units)
    result <- data.frame(new_data[predictor], yhat = new_data$yhat)
    return(result)
}


#' create plotting data for categorical variables
#' @importFrom purrr map2
#' @importFrom rlang sym
#' @importFrom dplyr group_by arrange summarize
#' @importFrom magrittr %>%
#' @keywords internal
#'

categorical_marg <- function(neural_net, data, predictor,
                            predictor_value, rep, units,
                            kind, change_variables,
                            change_values, class, type) {
    class_var <- sym(class)
    predictor_var <- sym(predictor)
    warning_message(neural_net, kind, predictor_value = predictor_value,
                    predictor, change_variables, change_values, type = type,
                    class)
    mean_input <- make_grid(neural_net, kind, type = type, class)
    new_data <- grid_factor(neural_net, mean_input, class, rep, type = type)
    if (!is.null(change_values) & !is.null(change_variables)) {
         new_data <- change(data, change_variables, change_values, class,
                            new_data, rep, type = type)
    }
    if (isTRUE(neural_net$scale)) {
        data <- data %>% group_by(!! class_var)
        predictor_value_scale <- group_scale_multiple(data, predictor,
                                            predictor_value = predictor_value)
        new_data <- new_data %>% arrange(!! class_var)
        new_data[predictor] <- unlist(map2(predictor_value_scale,
                                      units, ~ sequence(.x, rep, .y)))
    } else {
        new_data[predictor] <- rep(predictor_value, rep)
        new_data <- new_data %>% arrange(!! class_var)
        new_data[predictor] <- unlist(map2(predictor_value,
                                       units, ~ sequence(.x, rep, .y)))
    }
    result <- categ_prediction(neural_net, new_data, class, predictor,
              predictor_value = predictor_value, rep, units)
    return(result)
}

#' @importFrom magrittr %>%
#' @importFrom stringr str_replace
#' @importFrom dplyr bind_cols ends_with
#' @keywords internal
#'
categ_prediction <- function(neural_net, new_data, class, predictor,
                            predictor_value, rep, units = NULL) {
    prediction <- data.frame(neuralnet::compute(neural_net$neural_network,
                                              new_data)$net.result)
    names(prediction) <- paste(neural_net$neural_network$model.list$response,
                             "_prediction", sep = "")
    new_data <- new_data %>%
    bind_cols(prediction) %>%
    gather(class, prediction, ends_with("prediction")) %>%
    mutate(class = str_replace(class, "_prediction", ""))
    new_data[predictor] <- unlist(map2(predictor_value,
                                     units, ~ sequence(.x, rep, .y)))
    result <- new_data[c("class", predictor, "prediction")]
    names(result)[3] <- "yhat"
    return(result)
}

#' create plotting data for categorical variables
#' @keywords internal
prepare_data_repres <- function(neural_net, predictor,
                               predictor_value, rep, units,
                               kind, change_variables,
                               change_values, class) {
    type <- neural_net$type
    data <- create_data(neural_net)
    if (type == "numerical") {
    result <- numeric_marg(neural_net = neural_net, data = data,
                           predictor = predictor,
                           predictor_value = predictor_value, rep = rep,
                           units = units, kind = kind ,
                           change_variables = change_variables,
                           change_values = change_values,
                           class = class, type = type)

    }
    if (type == "categorical") {
        if (isTRUE(neural_net)) {
            data <- cbind(neural_net$neural_network$data[class], data)
        }
        times <- length(neural_net$neural_network$model.list$response)
        units <- rep(units, times )
        result <- categorical_marg(neural_net, data = data, predictor,
                               predictor_value, rep, units = units, kind,
                               change_variables, change_values,
                               class, type = type)
    }
    return(result)
}
#' create bootstrap samples
#' @keywords internal
compute_bootstrap_ci_repres <- function(result, predictor,
                                         predictor_value, neural_net, kind , rep,
                                         units, change_values, change_variables,
                                         class, probs, nrepetitions) {
    number_of_data_points <- nrow(neural_net$neural_network$data)
    bootstrap_data <- matrix(nrow = nrow(result),
                             ncol = nrepetitions)
    for (current_rep in 1:nrepetitions) {
        indices <- sample(1:number_of_data_points, size = number_of_data_points,
                          replace = TRUE)
        resampled_data_set <- neural_net$neural_network$data[indices, ]
        args <- c(list(f = neural_net$f, data = resampled_data_set,
                       layers = neural_net$layers, scale = neural_net$scale),
                  neural_net$additional)
        new_neural_net <- do.call(NeuralNetwork, args)
        new_result <- prepare_data_repres(new_neural_net, predictor,
                                          predictor_value, rep, units, kind,
                                          change_variables, change_values,
                                          class)
        bootstrap_data[, current_rep] <- new_result$yhat

    }
    result[, c("lwr", "upr")] <- t(apply(bootstrap_data, 1,
                                                     quantile, probs = probs))
    return(result)
}
#' create input for plots
#' @keywords internal
plotting_input <- function(neural_net, predictor,
                          predictor_value, rep, units,
                          kind, change_variables,
                          change_values, class,
                          probs, nrepetitions) {
    result <- prepare_data_repres(neural_net, predictor,
                                  predictor_value, rep, units,
                                  kind, change_variables,
                                  change_values, class)
    if (all(probs == 0) == TRUE) {
        result[, c("lwr", "upr")] <- result$yhat
        } else {
            result <- compute_bootstrap_ci_repres(result = result, predictor,
                                                  predictor_value, neural_net,
                                                  kind, rep, units,
                                                  change_values,
                                                  change_variables,class, probs,
                                                  nrepetitions)
        }
    return(result)
}

#' create plotting for multiple predictors
#' @importFrom purrr pmap map
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @keywords internal
plot_multiple_res <- function(neural_net, predictor, predictor_value, rep,
                              units, kind, change_variables, change_values,
                              class, probs, nrepetitions) {
    prediction_names <- ifelse(neural_net$type == "categorical",
                             yes = 2, no = 1)
    type <- neural_net$type
    len <- length(neural_net$neural_network$model.list$response)

    warn_plot(type = type, predictor, predictor_value, len = len)
    predictor_value <- split(predictor_value,
                                 ceiling(seq_along(predictor_value)/len))
    prepared_data <- pmap(list(predictor, predictor_value),
                ~ plotting_input(neural_net, predictor = .x,
                                     predictor_value = .y,
                                     units = units, rep = rep, kind = kind,
                                     change_values = change_values,
                                     change_variables = change_variables,
                                     class = class, probs = probs,
                                     nrepetitions = nrepetitions)) %>%
            map(~ gather(.x, "predictor", "values", prediction_names)) %>%
            bind_rows()
    if (neural_net$type == "numerical") {
        return(plot_multiple_numerical(prepared_data, neural_net))
    } else if (neural_net$type == "categorical") {
        return(plot_multiple_categorical(prepared_data, neural_net))
    }
}

#' create plotting for single predictor
#' @keywords internal

plot_single_res <- function(neural_net, predictor,
                        predictor_value,
                        rep, units, kind, change_variables,
                        change_values, class, probs, nrepetitions) {
    type <- neural_net$type
    len <- length(neural_net$neural_network$model.list$response)
    warn_plot(type = type, predictor, predictor_value, len =len)
    prepared_data <- plotting_input(neural_net, predictor,
                                    predictor_value, rep, units,
                                    kind, change_variables,
                                    change_values, class,
                                    probs, nrepetitions)
    if (neural_net$type == "numerical") {
        return(plot_single_numerical(prepared_data, predictor, neural_net))
    } else if (neural_net$type == "categorical") {
        return(plot_single_categorical(prepared_data, predictor, neural_net))
    }
}

#' warns about length of predictors for plotting
#' @keywords internal

warn_plot <- function(type, predictor, predictor_value, len) {
    if (!is.null(predictor_value)) {
        if (type == "numerical") {
            if (length(predictor) != length(predictor_value)) {
                stop("Predictor and predictor value must have same length")
            }
         }
        if (type == "categorical") {
            if (length(predictor_value) != length(predictor) * len) {
                stop("Length of predictor_value must be
                     multiple of class length")
            }
        }
    }
}


#' warns about length of predictors for plotting
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize_at vars funs
#' @keywords internal

empty_predictor_value <- function(neural_net, predictor, class) {
    data <- create_data(neural_net)
    type <- neural_net$type
    if (type == "numerical") {
        predictor_value <- as.numeric(colMeans(data[predictor]))
    }
    if (type == "categorical") {
        data_aux = data %>%
            group_by(!! sym(class)) %>%
            summarize_at(vars(predictor),funs(mean))
        predictor_value <- as.numeric(unlist(data_aux[predictor]))
    }
    return(predictor_value)
}


