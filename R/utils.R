setColNames <- function(df, names) {
    colnames(df) <- names
    return(df)
}

value.replace <- function(x, val, newval) {
    which.val <- x == val
    x[which.val] <- newval
    return(x)
}

metrics <- function(movieMetrics) {
    return(movieMetrics$metrics)
}

`%notin%` <- Negate(`%in%`)
