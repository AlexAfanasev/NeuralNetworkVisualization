# load the package before running the examples !


### sample data ####
data <- Boston
index <- sample(1:nrow(data), round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]


# TODO: add this to our package :)
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

#### test for pdp_wrapper ####
plot_partial_dependencies_numeric(
    predictor = "all", train_, nn)
plot_partial_dependencies_numeric(c("crim","indus", "nox"), train_, nn)
plot_partial_dependencies_numeric("crim", train_, nn)
plot_partial_dependencies_numeric(c("crim","all"), train_, nn)

### categorical data ####
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

pdp_class_wrapper("Sepal.Length", iris.train, iris.net, "Species")
pdp_class_wrapper(c("Sepal.Length","Sepal.Width"), iris.train, iris.net, "Species")
pdp_class_wrapper(predictor = "all",iris.train, iris.net, "Species")
pdp_class_wrapper(predictor = c("all", "Sepal.Length"),iris.train,iris.net, "Species")



# TODO: Finish test for binary data --> and add dataset to repository
### test for binary data####
#setwd("C:/Users/Jacky/Documents/University/G?ttingen/SS2019/Statistical Programming with R")
#dataset <- read.csv("creditset.csv")
## extract a set to train the NN
#trainset <- dataset[1:800, ]

## select the test set
#testset <- dataset[801:2000, ]
#creditnet <- neuralnet(default10yr ~ LTI + age, trainset, hidden = 2,
#                       linear.output = FALSE, threshold = 0.1)

#pdp_wrapper("LTI", trainset, creditnet, "default10yr")
#pdp_wrapper(c("LTI", "age"), trainset, creditnet, "default10yr")
