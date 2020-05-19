source("./getKnn.r")

private.predictOne <- function(train, x, formula, n, dist_method="euclidean") {
  nn_train <- getKnn(train, x, n, dist_method)
  model <- lm(formula, data=nn_train)
  # this gives us a warning but its fine
  predict(model, x)[[1]]
}

# doest this even work? 
# It will probably break getKnn because it weren't supposed to be used with dataframes.
localLinear <- function(train, test, formula, n, dist_method="euclidean") {
    apply(test, 1, function(row) private.predictOne(train, row, formula, n, dist_method))
}
