library(rpart)
library(purrr)
library(FNN)

# LocalARegressionModel S4 class generator
# Reserved for internal use, to create LocalRegressionModel use `createLocalRegressionModel` function
LocalRegressionModel <- setClass("LocalRegressionModel", slots=list(dataset="data.frame",
                                                                    normalizedDataset="data.frame",
                                                                    normalizationParams="list"))

#
# Creates local regression model with normalized data
#
createLocalRegressionModel <- function(dataset, normalizationTypes) {
  norm <- normalizeDataFrame(dataset, normalizationTypes)
  LocalRegressionModel(dataset=dataset, normalizedDataset=norm[[1]], normalizationParams=norm[[2]])
}

#
#
#
normalizeVector <- function(localModel, v) {
  for(i in 1:length(v))
  {
    params <- localModel@normalizationParams[[i]]
    if(params[["dataType"]] == "chr") {
      v[[i]] <- normalizeChrVec(v[[i]])
    } else {
      if(params[["type"]] == "omit") {
      } else if(params[["type"]] == "minmax") {
        v[[i]] <- (v[[i]] - params[["min"]]) / (params[["max"]] - params[["min"]])
      } else if(params[["type"]] == "zscore") {
        v[[i]] <- (v[[i]] - params[["mean"]]) / params[["std"]]
      }
    }
  }
  return(v)
}

#
#
#
stripDependentVariable <- function(dataset, formula) {
  depVar <- all.vars(formula)[1]
  if (depVar %in% colnames(dataset)) {
    stripped <- dataset[depVar]
    dataset[depVar] <- NULL
  } else {
    stripped <- NULL
  }
  list(dataset=dataset, stripped=stripped)
}

#
#
#
.wrapOne <- function(localModel, x, n, knnAlgorithm, func, formula, ...) {
  normalizedX <- normalizeVector(localModel, x)
  normalizedX <- data.frame(as.list(normalizedX))

  ds <- stripDependentVariable(localModel@normalizedDataset, formula)
  dx <- stripDependentVariable(normalizedX, formula)

  nn <- get.knnx(ds$dataset, dx$dataset, k=n, algorithm=knnAlgorithm)
  indexes <-unlist(nn$nn.index[1,], use.names = FALSE)
  nn_train <- localModel@normalizedDataset[indexes,]
  model <- func(..., formula=formula, data=nn_train)
  predict(model, normalizedX)
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
#' `localLinear.wrap` and `regressionTree.wrap` functions.
wrap <- function(localModel, test, n, knnAlgorithm, func, formula, ...) {
  apply(test, 1, function(row) .wrapOne(localModel, row, n, knnAlgorithm, func, formula, ...))
}

#' Calls `wrap` function with predefined arguments
#' for the KNN-alghoritm (n = 100 and dist_method="euclidean".
defaultWrap <- function(localModel, test, func, formula, ...) {
  wrap(localModel, test, 100, "cover_tree", func, formula, ...)
}

#' Pedict values in `test` using `formula` with
#' regression model beeing fit with builtin `lm` function.
localLinearWrap <- function(localModel, test, formula) {
  defaultWrap(localModel, test, lm, formula)
}

#' Pedict values in `test` using `formula` with
#' regression model beeing fit with builtin `rpart`
#' function from `rpart` package.
#'
#' As a method "anove" is passed to the `rpart` function.
regressionTreeWrap <- function(localModel, test, formula) {
  defaultWrap(localModel, test, rpart, formula, method="anova")
}


#
#
#
normalizeChrVec <- function(v) {
  i <- 0
  dict <- list()
  for(b in unique(v)) {
    dict[[b]] <- i
    i <- i + 1
  }

  v <- unlist(v, use.names=FALSE)
  vNorm <- map(v, function(x) dict[[x]])
  map(vNorm, function(x) x/(i - 1))
}

#
# Normalizes dataframe for LocalRegressionModel
#
normalizeDataFrame <- function(dataset, normalizationType) {
  nCols <- ncol(dataset)
  normalizationParams <-list()
  for(i in 1:nCols) {
    colType = class(dataset[,i])
    if (colType == "character") {
      dataset[, i] <- normalizeChrVec(dataset[, i])
      normalizationParams[[i]] <- list(dataType="chr")
    }
    if (colType == "integer" || colType == "float" || colType == "double" || colType == "numeric") {
      if (normalizationType[[i]] == "zscore") {
        m = mean(dataset[,i])
        s = sd(dataset[,i])
        dataset[,i] <- (dataset[,i] - m) / s
        normalizationParams[[i]] <- list(dataType="numeric", type=normalizationType[[i]], mean=m, std=s)
      } else if (normalizationType[[i]] == "minmax") {
        mi = min(dataset[, i])
        mx = max(dataset[, i])
        dataset[,i] <- (dataset[,i] - mi) / (mx - mi)
        normalizationParams[[i]] <- list(dataType="numeric", type=normalizationType[[i]], min=mi, max=mx)
      } else {
        normalizationParams[[i]] <- list(dataType="any", type=normalizationType[[i]])
      }
    }
  }
  list(dataset, normalizationParams)
}

.classifierErrorOnSigleSet <- function(dataset, predictions, formula) {
  partitioned <- stripDependentVariable(dataset, formula)
  mean((as.vector(partitioned$stripped) - predictions)^2) # TODO: will it even like work?
}

#' Mark classifier returning errors along with predictions
#' on train and test datasets.
#' 
#' @param localModel model created with \code{createLocalRegressionModel} function
#' @param test dataset containing test data (along with the predicted column)
#' @param wrappedFunc prediction function that will be called - should return a vector of predictions
#' @param formula formula to be passed to \code{wrappedFunc}
#' @param ... rest of the params passed to the \code{wrappedFunc}
#' 
#' @return list of four element sqared mean error on test and train data
#' along with predictions of the \code{wrappedFunc} for the train and test data.
#' @examples
#' classInfo <- .markClassifier(localModel, test, regressionTreeWrap, Sepal.Length~.)
.markClassifier <- function(localModel, test, wrappedFunc, formula, ...) {
  predictionsForTrain <- wrappedFunc(localModel, localModel$dataset, formula, ...)
  predictionsForTest <- wrappedFunc(localModel, test, formula, ...)
  errorForTrain <- .classifierErrorOnSigleSet(localModel$dataset, predictionsForTrain, formula)
  errorForTest <- .classifierErrorOnSigleSet(train, predictionsForTest, formula)
  list(
    testPred = predictionsForTest,
    testError = errorForTest,
    trainPred = predictionsForTrain,
    trainError = errorForTrain,
  )
}





