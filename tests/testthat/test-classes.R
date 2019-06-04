test_that("plot_partial_dependencies_numeric throws error if 'all'
          is contained in multiple predictors ", {
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
    expect_error(plot_partial_dependencies_numeric(c("crim","all"), train_, nn))
})
