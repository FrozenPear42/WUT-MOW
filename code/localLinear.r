source("./getKnn.r")

# Prywatna funkcja dokonująca predykcji jak localLinear dla pojedyńczej wartości.
private.predictOne <- function(train, x, formula, n, dist_method="euclidean") {
  nn_train <- getKnn(train, x, n, dist_method)
  model <- lm(formula, data=nn_train)
  # this gives us a warning but its fine
  predict(model, x)[[1]]
}

# doest this even work? 
# It will probably break getKnn because it weren't supposed to be used with dataframes.

# Dokonuje predykcji wartości dla zbioru `test`.
# Model lokalny tworzony jest dla `n` najbliższych sąsiadów licząc dystanś metodą `dist_method`
# ze zbioru `train`.
# Model liniowy tworzony jest dla wybranego modelu lokalnego z użyciem formuły `formula`.
localLinear <- function(train, test, formula, n, dist_method="euclidean") {
    apply(test, 1, function(row) private.predictOne(train, row, formula, n, dist_method))
}
