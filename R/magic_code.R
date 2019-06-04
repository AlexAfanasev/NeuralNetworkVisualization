### libraries ####
library(tidyverse)
library(neuralnet)
library(MASS)
library(rlang)

### sample data ####
data <- Boston
index <- sample(1:nrow(data), round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
# scale for neuralnet
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]
n <- names(train_)
# formula 
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
# construct nn
nn <- neuralnet(f,data = train_,hidden = c(5,3),linear.output = T)



### pdp ####

pdp = function(predictor, train, nn){
            
            predictor_unquoted <- sym(predictor)
            # dplyr:: is required as tidyverse and neuralnet share functions with same name
            # select the predictor for pdp
            grid_predictor <- dplyr::select(train, !!predictor_unquoted)   
            # select the remaining independent variables
            grid_input <- dplyr::select(train, -!!predictor_unquoted)
            # create a grid with all combinations of the remaining independent variables
            # for every unique value of the pdp variable of interest
            grid <- crossing(grid_predictor, grid_input)
            # add a column with the respective predictions of the neural network
            grid <- grid %>%
                    mutate(pred = neuralnet::compute(nn,grid)$net.result)
            # for every unique value of the variable of interest, calculate the mean
            pd <- grid %>%
                  group_by(!!predictor_unquoted) %>%
                  summarize(yhat = mean(pred))
                
            return(pd)
            
}
### pdp_wrapper ####
# pdp_wrapper
# predictor is the variable of interest
# train is the training set used on the neural network
# nn is the used neural network
# dependent_nn is the dependent variable used in the neural network 
#(only required if "all" is selected)

pdp_wrapper <- function(predictor = NULL, train, nn, dependent_nn){
               # plot  pdp for a single function
  
                         if (length(predictor) == 1){
               # case where one predictor is specified by name
                              if (predictor != "all"){
               # name cleaning for dplyr and ggplot2
                               predictor_unquoted = sym(predictor)
                               predictor_paste = quo_name(predictor_unquoted)
               # execute pdp function               
                               pd <- pdp(predictor, train = train, nn = nn)
               # plot                
                               pd <- pd %>%
                                     ggplot(aes(!!predictor_unquoted, yhat)) +
                                     geom_line(size = 1) +
                                     labs(title = paste("Partial dependence plot for", predictor_paste),
                                          y = paste("Marginal probability of", predictor_paste),
                                          x = paste(predictor_paste)) +
                                     theme_grey()
               # case when "all" is selected
                           } else {
               # select every independent variable apart from the dependent one in the training set
                                   pred_cond <- colnames(train) != dependent_nn
                                   predictor <- colnames(train)[pred_cond]
               # apply pdp function to every predictor              
                                   pd <- predictor %>%
                                         map(~ pdp(.x, train_, nn)) %>% 
                                         map(~ gather(.x,"predictor", "values", 1)) %>%
                                         bind_rows()
               # plot              
                                         pd <- pd %>% 
                                               ggplot(aes(values, yhat)) +
                                               geom_line(size = 1) +
                                               facet_wrap(vars((predictor)), scales = "free") +
                                               labs(title = "Partial dependence plots for predictors",
                                                    y = "Marginal probability of predictor",
                                                    x = "Predictor") +
                                               theme_grey()
                                         }
                        
                          }
               # plot multiple pdps 
                           if(length(predictor) > 1 ){
               # case where "all" is not contained in the selected predictors              
                              if(! "all" %in% predictor){
               # apply the pdp function to every specified predictor
                               pd <- predictor %>%
                                     map(~ pdp(.x, train, nn)) %>% 
                                     map(~ gather(.x,"predictor", "values", 1)) %>%
                                     bind_rows()
               # plot        
                               pd <- pd %>% 
                                     ggplot(aes(values, yhat)) +
                                     geom_line(size = 1) +
                                     facet_wrap(vars((predictor)), scales = "free") +
                                     labs(title = "Partial dependence plots for predictors",
                                          y = "Marginal probability of predictor",
                                          x = "Predictor") +
                                     theme_grey() 
               # throw error message if "all" is contained in vector of predictors
                              } else {
                                      stop("Predictors of length >1 cannot contain 'all'")
                           }
                         }
                          
                     
               return(pd)
}
#### test for pdp_wrapper ####

pdp_wrapper(predictor = "all", train_, nn, dependent_nn = "medv")
pdp_wrapper(c("crim","indus", "nox"), train_,nn)
pdp_wrapper("crim", train_,nn)
pdp_wrapper(c("crim","all"), train_nn)
#### categorical data ####

data(iris)
iris$setosa <- iris$Species=="setosa"
iris$virginica <- iris$Species == "virginica"
iris$versicolor <- iris$Species == "versicolor"
iris.train.idx <- sample(x = nrow(iris), size = nrow(iris)*0.5)
iris.train <- iris[iris.train.idx,]
iris.valid <- iris[-iris.train.idx,]
iris.net <- neuralnet(setosa+versicolor+virginica ~ 
                        Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                      data=iris.train, hidden=c(10,10), rep = 5, err.fct = "ce", 
                      linear.output = F, lifesign = "minimal", stepmax = 1000000,
                      threshold = 0.001)

### pdp_class ####
# predictor is the variable of interest
# train is the training set of the neural network
# nn is the neural network
# class is the variable where the different values of the categorical dependent variable are stored


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

pdp_class_wrapper("Sepal.Length", iris.train, iris.net, "Species")
pdp_class_wrapper(c("Sepal.Length","Sepal.Width"), iris.train, iris.net, "Species")
pdp_class_wrapper(predictor = "all",iris.train, iris.net, "Species")
pdp_class_wrapper(predictor = c("all", "Sepal.Length"),iris.train,iris.net, "Species")



### test for binary data####
setwd("C:/Users/Jacky/Documents/University/Göttingen/SS2019/Statistical Programming with R")
dataset <- read.csv("creditset.csv")
## extract a set to train the NN
trainset <- dataset[1:800, ]

## select the test set
testset <- dataset[801:2000, ]
creditnet <- neuralnet(default10yr ~ LTI + age, trainset, hidden = 2, 
                       linear.output = FALSE, threshold = 0.1)

pdp_wrapper("LTI", trainset, creditnet, "default10yr")
pdp_wrapper(c("LTI", "age"), trainset, creditnet, "default10yr")
