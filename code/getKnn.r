#' Resturns matrix containing `n` nearest neighbours of sample `x`
#' from dataset `train` using builtin `dist` method with 
#' `dist_method` to compute distances.
get.knn <- function(train, x, n, dist_method="euclidean") {
    train_with_x <- rbind(x, train, deparse.level=0)
    distances <- dist(train_with_x, method=dist_method)
    distances <- as.matrix(distances)[1, ] # distences from the first row to the ech
    distances <- distances[-1] # skip the first distance to itself
    names(distances) <- strtoi(names(distances))-1 # get indexes in the train set
    distances <- sort(distances)
    n_indexes <- strtoi(names(distances)[1:n])
    train[n_indexes, ]
}