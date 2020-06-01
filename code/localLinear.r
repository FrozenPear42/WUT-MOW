# This module requires `rpart` library loaded for
# the `regressionTree.wrap` function to work.

private.wrapOne <- function(train, x, n, dist_method, func, ...) {
    nn_train <- get.knn(train, x, n, dist_method)
    model <- func(..., data=nn_train)
    predict(model, data.frame(as.list(x)))[[1]]
}

#' Predict values for `test` based on the `train` set with
#' function `func` calculating regression model on
#' some local data of length `n` provided by KNN-algorithm
#' with distance calculated using builtin `dist` 
#' function with `dist_method` argument.
#'
#' The `func` will be called with `data` argument already provided
#' based on the `train` set. Any additional arguments can be
#' passed with `...` variadic argument.
#' 
#' If you are passing a formula then the `test` set should not
#' have predicted column.
#' 
#' For usage examples look at implementation of
#' `localLinear.wrap` and `refressionTree.wrap` functions.
wrap <- function(train, test, n, dist_method, func, ...) {
    apply(test, 1, function(row) private.wrapOne(train, row, n, dist_method, func, ...))
}

#' Calls `wrap` function with predefined arguments
#' for the KNN-alghoritm (n = 100 and dist_method="euclidean".
default.wrap <- function(train, test, func, ...) {
    wrap(train, test, 100, "euclidean", func, ...)
}

#' Pedict values in `test` using `formula` with
#' regression model beeing fit with builtin `lm` function.
localLinear.wrap <- function(train, test, formula) {
    default.wrap(train, test, lm, formula)
}

#' Pedict values in `test` using `formula` with
#' regression model beeing fit with builtin `rpart`
#' function from `rpart` package. 
#' 
#' As a method "anove" is passed to the `rpart` function.
regressionTree.wrap <- function(train, test, formula) {
    default.wrap(train, test, rpart, formula=formula, method="anova")
}