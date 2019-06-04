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

### test for binary data ###
library(faraway)
library(DMwR)
str(pima)
pima$glucose[pima$glucose == 0] <- NA
pima$diastolic[pima$diastolic == 0] <- NA
pima$triceps[pima$triceps == 0] <- NA
pima$insulin[pima$insulin == 0] <- NA
pima$bmi[pima$bmi == 0] <- NA
sapply(pima, function(x) sum(is.na(x)))
pima <- pima[-manyNAs(pima),]
pima.clean <- knnImputation(pima, k = 10)
pima.clean$test <- as.factor(pima.clean$test)
levels(pima.clean$test) <- c("Negative", "Positive")
scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

pima2 <- pima.clean
pima2[,9] <- unclass(pima2[,9])

pima.norm <- pima2 %>%
             mutate_all(scale01)

pima.size <- floor(0.75 * nrow(pima.norm))
train <- sample(seq_len(nrow(pima.norm)), size = pima.size)

pima.train.n <- pima.norm[train, ]
pima.test.n <- pima.norm[-train, ]
pima.nn <- neuralnet(test ~ pregnant + glucose + diastolic + triceps + insulin + bmi + diabetes + age, 
                     hidden = 4, data = pima.train.n, linear.output = TRUE)

plot_partial_dependencies_numeric("glucose",pima.train.n, pima.nn)
plot_partial_dependencies_numeric(c("glucose","pregnant"),pima.train.n, pima.nn)
plot_partial_dependencies_numeric("all", pima.train.n, pima.nn)