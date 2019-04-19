value.replace <- function(x, val, newval) {
    which.val <- x == val
    x[which.val] <- newval
    return(x)
}

metrics <- function(movieMetrics) {
    return(movieMetrics$metrics)
}

`%notin%` <- Negate(`%in%`)

convertToCurrency <- function(vec) {
    result <- vec %>%
        gsub(".*\\$|,", "", .) %>%
        as.numeric() %>% 
        add.class("Currency")
}
