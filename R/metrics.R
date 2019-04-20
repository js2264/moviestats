metrics <- function(x) {
    UseMethod("metrics", x)
}

metrics.default <- function(x) {
    return(x)
}

metrics.movieMetrics <- function(x) {
    return(x$metrics)
}

metrics.movieMetricsLongList <- function(x) {
    return(x$Metrics)
}

infos <- function(x) {
    UseMethod("infos", x)
}

infos.default <- function(x) {
    return(x)
}

infos.movieMetrics <- function(x) {
    unclass(x)[names(unclass(x)) != 'metrics']
}

infos.movieMetricsLongList <- function(x) {
    return(x$Infos)
}
