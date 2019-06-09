#' Preparation for the case of numeric dependenant variable in the
#' neural network
#'
#' Prepares the data for further plotting
#'
#' @param predictor Vector of strings representing the independant variable
#'  (as a column of the training set)
#' @param train Training data set
#' @param neural_network Trained neural network
#'
#' @return data.frame
#'
#' @name prepare_numeric_data
#'
#' @import tidyverse
#' @import neuralnet
#' @import rlang
#'
#' @export

prepare_numeric_data <- function(predictor, train, neural_network) {

    predictor_unquoted <- sym(predictor)
    # dplyr:: is required as tidyverse and neuralnet share functions
    # with same name
    # select the predictor for prepare_numeric_data
    grid_predictor <- dplyr::select(train, !!predictor_unquoted)
    # select the remaining independent variables
    grid_input <- dplyr::select(train, -!!predictor_unquoted)
    # create a grid with all combinations of the remaining independent variables
    # for every unique value of the prepare_numeric_data variable of interest
    grid <- crossing(grid_predictor, grid_input)
    # add a column with the respective predictions of the neural network
    grid <- grid %>%
            mutate(pred = neuralnet::compute(neural_network,grid)$net.result)
    # for every unique value of the variable of interest, calculate the mean
    partial_dependence <- grid %>%
                          group_by(!!predictor_unquoted) %>%
                          summarize(yhat = mean(pred))
    return(partial_dependence)
}

#' Plot partial dependencies for the case of numeric dependenant variable
#' in the neural network
#'
#' Plot the data
#'
#' @param predictor Vector of strings or numerics representing the independant variables
#' (as columns of the training set), default is "all"
#' @param train Training data set
#' @param neural_network Trained neural network
#'
#' @return plot
#'
#' @name plot_partial_dependencies_numeric
#'
#' @import tidyverse
#' @import neuralnet
#' @import rlang
#'
#' @export


plot_partial_dependencies_numeric <- function(predictor = "all",
                                              train, neural_network) {
    # transform the numeric input into a string (column name of training set)
    if (is.numeric(predictor)) {
        predictor = colnames(train)[predictor]
    }

    # detect non-existent predictor names
    if (any(! predictor %in% colnames(train)) ) {
        stop(paste("Selected predictor is not found in the training data.","\n",
                   " Also beware: ", "\n",
                   " Do not mix numeric and character inputs for the predictor argument!"))
    }
    # check whether the selected predictor is equal to the dependent variable
    if (any(predictor %in% colnames(neural_network$response))) {
        stop("Predictor cannot be the dependent variable of the network")
    }
    # name cleaning for dplyr and ggplot2
    dependent_paste <- quo_name(colnames(neural_network$response))
    if (length(predictor) == 1) {
        if (predictor != "all") {
            predictor_unquoted <- sym(predictor)
            predictor_paste <- quo_name(predictor_unquoted)
            pd <- prepare_numeric_data(predictor, train = train,
                                       neural_network = neural_network)
            pd <- pd %>%
                  ggplot(aes(!!predictor_unquoted, yhat)) +
                  geom_line(size = 1) +
                  labs(title = paste("Partial dependence plot with response",
                                   dependent_paste),
                       y = paste("Marginal probability of", predictor_paste),
                       x = paste(predictor_paste)) +
                theme_grey()
        } else {
            pred_cond <- colnames(train) != colnames(neural_network$response)
            predictor <- colnames(train)[pred_cond]
            pd <- predictor %>%
                  map(~ prepare_numeric_data(
                      .x, train, neural_network)) %>%
                  map(~ gather(.x,"predictor", "values", 1)) %>%
                  bind_rows()
            pd <- pd %>%
                  ggplot(aes(values, yhat)) +
                  geom_line(size = 1) +
                  facet_wrap(vars((predictor)), scales = "free") +
                  labs(title = paste("Partial dependence plots with response",
                                   dependent_paste),
                       y = "Marginal probability of predictor",
                       x = "Predictor") +
                  theme_grey()
        }

    }

    if (length(predictor) > 1) {
        if (! "all" %in% predictor) {
            pd <- predictor %>%
                  map(~ prepare_numeric_data(.x, train, neural_network)) %>%
                  map(~ gather(.x,"predictor", "values", 1)) %>%
                  bind_rows()
            pd <- pd %>%
                  ggplot(aes(values, yhat)) +
                  geom_line(size = 1) +
                  facet_wrap(vars((predictor)), scales = "free") +
                  labs(title = paste("Partial dependence plots with response",
                                   dependent_paste),
                       y = "Marginal probability of predictor",
                       x = "Predictor") +
                  theme_grey()
        } else {
            stop("Predictors of length > 1 cannot contain 'all'")
        }
    }
    return(pd)
}

# TODO: Finish this Documentation
#' Plot partial dependencies for the case of numeric dependent variable
#' in the neural network
#'
#' Plot the data
#'
#' @param predictor Vector of strings representing the independant variables
#' (as columns of the training set), default is "all"
#' @param train Training data set
#' @param neural_network Trained neural network
#'
#' @return plot
#'
#' @name plot_partial_dependencies_numeric
#'
#' @import tidyvers
#' @import neuralnet
#' @import rlang
#'
#' @export


pdp_class = function(predictor,train, nn, class){

            predictor_unquoted <- sym(predictor)

            grid_predictor <- dplyr::select(train, !!predictor_unquoted)
            # select the remaining independent variables
            grid_input <- dplyr::select(train, -!!predictor_unquoted)
            grid <- crossing(grid_predictor, grid_input)
            pred <- as_tibble(neuralnet::compute(nn,grid)$net.result)
            # name the predictions after their categories
            names(pred) = paste(unlist(unique(train[class])),"_pred",sep = "")
            # append predictions to the grid
            grid <- grid %>%
                    bind_cols(pred)
            # gather the different predictions into one column ("pred")
            # assign the names of categories to "class"
            # group by category and unique values of the respective predictor
            pd <- grid %>%
                  gather(class, pred, ends_with("pred")) %>%
                  mutate(class = str_replace(class,"_pred",""))%>%
                  group_by(class, !!predictor_unquoted) %>%
                  summarize(yhat = mean(pred))
            return(pd)
}
#### wrapper function ######

pdp_class_wrapper = function(predictor = NULL, train, nn, class){
    # transform the numeric input into a string (column name of training set)
    if (is.numeric(predictor)) {
        predictor = colnames(train)[predictor]
    }

    # detect non-existent predictor names
    if (any(! predictor %in% colnames(train)) ) {
        stop(paste("Selected predictor is not found in the training data.","\n",
                   " Also beware: ", "\n",
                   " Do not mix numeric and character inputs for the predictor argument!"))
    }
    # check whether the selected predictor is equal to the dependent variable
    if (any(predictor %in% colnames(neural_network$response))) {
        stop("Predictor cannot be the dependent variable of the network")
    }
    # assess the case of a predictor input of length 1
    if (length(predictor) == 1){
    # for the case only one predictor is selected
    if(predictor != "all"){
    # clean names to dplyr and ggplot2
          predictor_unquoted <- sym(predictor)
          predictor_paste <- quo_name(predictor)
# execute pdp_class
          pd <- pdp_class(predictor = predictor,train = train, nn = nn,
                                  class = class)
# plot
          pd <- pd %>%
                ggplot(aes(!!predictor_unquoted, yhat, color = class)) +
                geom_line(size = 1) +
                labs(title = paste("Partial dependence plot for", predictor_paste),
                     y = paste("Marginal probability of", predictor_paste),
                     x = paste(predictor_paste)) +
                theme_grey()

# if all the predictors are selected
      } else {
# the class and the dummy variables of the categorical variable should be omitted
        omit <- train %>%
                dplyr::select(Species,!!! syms(colnames(iris.net$response)))
# execute pdp on everything but class and dummy
        omit_names <- colnames(omit)
        predictor = train[, ! names(train) %in% omit_names, drop = F]
        predictor = names(predictor)

        pd <- predictor %>%
              map(~ pdp_class(.x, train, nn, class)) %>%
              map(~ gather(.x,"predictor", "values", 2)) %>%
              bind_rows()
# plot
        pd <- pd %>%
              ggplot(aes(values, yhat, color = class)) +
              geom_line(size = 1) +
              facet_wrap(vars((predictor)), scales = "free") +
              labs(title = "Partial dependence plots for predictors",
                   y = "Marginal probability of predictor",
                   x = "Predictor") +
              theme_grey()

      }
  }
# multiple predictors
      if (length(predictor) > 1){
# selected predictors ( but not "all")
          if (!"all" %in% predictor){
# execute pdp on every predictor and merge
      pd <- predictor %>%
            map(~ pdp_class(.x, train, nn, class)) %>%
            map(~ gather(.x,"predictor", "values", 2)) %>%
            bind_rows()
# plot
      pd <- pd %>%
            ggplot(aes(values, yhat, color = class)) +
            geom_line(size = 1) +
            facet_wrap(vars((predictor)), scales = "free") +
            labs(title = "Partial dependence plots for predictors",
                 y = "Marginal probability of predictor",
                 x = "Predictor") +
            theme_grey()

  }else{
# throw error message if "all" is included
  stop(" Predictors of length >1 cannot contain 'all'")
  }
}

return(pd)
}
