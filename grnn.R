calcR <- function(trainMatr, x) {
  apply(trainMatr, 1, function(row) sqrt(sum(( row - x )^2)))
}
calcD <- function(r, sigma = 1) {
  exp(-(r/sigma)^2)
}

calcY <- function(d, y) sum(d*y)/sum(d)

grnn <- function(trainWithResults, x, sigma = 1) {
  train <- trainWithResults[, 1:2]
  y_train <- trainWithResults[,3]
  
  r <- calcR(train, x)
  d <- calcD(r, sigma)
  y <- calcY(d, y_train)
  y
}

generateInputData <- function(n) {
  set.seed(101)
  x1 <- runif(n, -1, 1)
  x2 <- runif(n, -1, 1)
  y <- (1-x1^2)+2*(1-x2)^2
  x <- data.frame(v1=x1, v2=x2, v3=y)
}

n <- 10
train_coef <- 0.75

input <- generateInputData(n)

smp_size <- floor(train_coef * nrow(input))
set.seed(123)
train_ind <- sample(seq_len(nrow(input)), size = smp_size)

train <- input[train_ind, ]
test <- input[-train_ind, ]

result <- apply(test, 1, function(x) grnn(train, as.numeric(x[1:2])) )

test["y_r"] = result
