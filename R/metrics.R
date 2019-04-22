
#' @export

metrics <- function(x) {
    UseMethod("metrics", x)
}

#' @export

metrics.default <- function(x) {
    return(x)
}

#' @export

metrics.movieMetrics <- function(x) {
    return(x$metrics)
}

#' @export

metrics.movieMetricsLongList <- function(x) {
    return(x$Metrics)
}

#' @export

infos <- function(x) {
    UseMethod("infos", x)
}

#' @export

infos.default <- function(x) {
    return(x)
}

#' @export

infos.movieMetrics <- function(x) {
    unclass(x)[names(unclass(x)) != 'metrics']
}

#' @export

infos.movieMetricsLongList <- function(x) {
    return(x$Infos)
}