library(rpart)
library(purrr)
library(FNN)

#' LocalRegressionModel S4 class generator
#' Reserved for internal use, to create LocalRegressionModel use `createLocalRegressionModel` function
LocalRegressionModel <- setClass("LocalRegressionModel", slots=list(dataset="data.frame",
                                                                    normalizedDataset="data.frame",
                                                                    normalizationParams="list"))

#' Creates local regression model with normalized data
#'
#' @param dataset dataframe of training dataset; it will be normalized using `normalizeDataFrame` function
#' @param normalizationTypes vector of normalization type for each column.
#' Allowed values:
#'  - `minmax` - use min-max normalization
#'  - `zscore` - use Z-score normalization
#'  - `omit` - do not normalize
#' Character collumns will be normalized by default with `minmax` algorithm, normalization type value has no effect in that case
#'
#' @return Local regression model object
createLocalRegressionModel <- function(dataset, normalizationTypes) {
  norm <- normalizeDataFrame(dataset, normalizationTypes)
  LocalRegressionModel(dataset=dataset, normalizedDataset=norm[[1]], normalizationParams=norm[[2]])
}

#' Normalizes vector using provided `LocalRegressionModel`
#'
#' @param localModel LocalRegressionModel object
#' @param v vector to be normalised; it is required for vector to have same column order as training set specified in regression model.
#' Provided vector can be shorter than vectors in regression model.
#'
#' @return normalized vector
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

#' Removes column with dependent value from dataframe based on formula
#'
#' @param dataset dataframe
#' @param formula formula with variable names coresponding to dataframe
#'
#' @return list containing:
#' - `dataset` - dataframe without column of dependent variable
#' - `stripped` - removed column
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

#' Internal function allowing to perform wrapping on single observation
#'
#'
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

#' Wrap regression model
#'
#' Predict values for `test` based on the `train` set with
#' function `func` calculating regression model on
#' some local data of length `n` provided by KNN-algorithm
#' The `func` will be called with `data` argument already provided
#' based on the `train` set. Any additional arguments can be
#' passed with `...` variadic argument.
#'
#' @param localModel LocalRegressionModel object builded with training data
#' @param test dataframe with test data for prediction
#' @param n number of neighbours for kNN algoritm
#' @param knnAlgorithm algorithm used in kNN algorithm; allowed values: `kd_tree`, `cover_tree`, `CR`, `brute`\
#' @param func function to be used in prediction
#' @param formula formula of predicted value
#' @param ... additional params to be passed to `func`
#'
#' @return dataframe with prediction results
#'
#' @examples
#' For usage examples look at implementation of
#' `localLinear.wrap` and `regressionTree.wrap` functions.
wrap <- function(localModel, test, n, knnAlgorithm, func, formula, ...) {
  apply(test, 1, function(row) .wrapOne(localModel, row, n, knnAlgorithm, func, formula, ...))
}

#' `wrap` function using linear model as regression model.
#'  Predicts values in test dataframe based on passed formula and LocalRegressionModel.
#'
#' @param localModel LocalRegressionModel object builded with training data
#' @param test dataframe with test data for prediction
#' @param n number of neighbours for kNN algoritm
#' @param formula formula of predicted value
#'
#' @return dataframe of predicted values
localLinearWrap <- function(localModel, test, n, formula) {
  wrap(localModel, test, n, "cover_tree", lm, formula)
}

#' `wrap` function using regression tree to build regression model.
#'  Predicts values in test dataframe based on passed formula and LocalRegressionModel.
#'
#' @param localModel LocalRegressionModel object builded with training data
#' @param test dataframe with test data for prediction
#' @param n number of neighbours for kNN algoritm
#' @param formula formula of predicted value
#' @param method regression tree building method from `rpart` package
#'
#' @return dataframe of predicted values
regressionTreeWrap <- function(localModel, test, n, formula, method="anova") {
  wrap(localModel, test, n, "cover_tree", rpart, formula, method)
}


#' Converts vector of enum values (eg chr ["bmw", "bmw", "ferrari"]) to vector of normalized values in range [0,1]
#'
#' @param v vector to be normalized
#'
#' @return list (dict) containing of `$v` which is normalized vector and `$map` which is list of mappings enum-to-assigned value
#'
#' @examples
#' r <- normalizeEnum(c("a", "a", "b"))
#' # r$v = [0, 0, 1]
#' # r$map = list(a=0, b=1)
normalizeEnumVector <- function(v) {
  i <- 0
  dict <- list()
  for(b in unique(v)) {
    dict[[b]] <- i
    i <- i + 1
  }

  v <- unlist(v, use.names=FALSE)
  vNorm <- map(v, function(x) dict[[x]]/(i - 1))
  enumMap <- map(dict, function(x) x/(i-1))
  list(v=vNorm, map=enumMap)
}

#' Normalizes dataframe for LocalRegressionModel
#'
#' @param dataset dataframe with data to be normalized
#' @param normalizationType vector of normalization method for each column.
#' Allowed values:
#'  - `minmax` - use min-max normalization
#'  - `zscore` - use Z-score normalization
#'  - `omit` - do not normalize
#' Character collumns will be normalized by default with `minmax` algorithm, normalization type value has no effect in that case
#'
#' @return list containing:
#' - `dataset` - normalized dataframe
#' - `normalizationParams` - params used for normalization. Those values can be used to normalize vector outside dataset with same params.
#'
normalizeDataFrame <- function(dataset, normalizationType) {
  nCols <- ncol(dataset)
  normalizationParams <-list()
  for(i in 1:nCols) {
    colType = class(dataset[,i])
    if (colType == "character") {
      dataset[, i] <- normalizeEnumVector(dataset[, i])
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
  list(dataset=dataset, normalizationParams=normalizationParams)
}


#'
#'
#'
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





