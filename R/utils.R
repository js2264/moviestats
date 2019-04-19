`%notin%` <- Negate(`%in%`)

valueReplace <- function(x, val, newval) {
    which.val <- x == val
    x[which.val] <- newval
    return(x)
}

addClass <- function(x, newclass) {
    
    class(x) <- c(newclass, class(x))
    return(x)
    
}

capitalizeFirstLetter <- function(string) {
    return(paste0(toupper(substr(string, 1, 1)), substr(string, 2, nchar(string))))
}

# -------------

metrics <- function(movieMetrics) {
    return(movieMetrics$metrics)
}

convertToCurrency <- function(vec) {
    result <- vec %>%
        gsub(".*\\$|,", "", .) %>%
        as.numeric() %>% 
        addClass("Currency")
}

fixDuplicatedMovies <- function(moviesDB) {
    
    doublets <- moviesDB[[1]][duplicated(moviesDB[[1]])]
    for (doublet in doublets) {
        years <- moviesDB[[2]][moviesDB[[1]] == doublet] %>% 
            gsub(".*(\\(\\d\\d\\d\\d\\)).*|.*(\\(Remake\\)).*", "\\1\\2", .) %>% 
            gsub("^http.*", "", .)
        moviesDB[[1]][moviesDB[[1]] == doublet] <- paste(moviesDB[[1]][moviesDB[[1]] == doublet], years)
    }
    
    return(moviesDB)
    
}

#' @importFrom magrittr "%>%" "%<>%"

fixMetrics <- function(metrics.df) {
    
    metrics.df$Date %<>%
        as.Date(., format = "%b %d, %Y")
    metrics.df$Rank %<>%
        valueReplace("-", NA) %>% 
        as.numeric()
    metrics.df$Gross %<>%
        convertToCurrency()
    metrics.df[, grep("Change", colnames(metrics.df))] %<>% 
        gsub(",|\\%|n/c", "", .) %>% 
        valueReplace("", NA) %>% 
        as.numeric()
    metrics.df$Theaters %<>% 
        gsub(",", "", .) %>%
        as.numeric()
    metrics.df[, "Per Theater"] %<>%
        convertToCurrency()
    metrics.df[, grep("Total.Gross", colnames(metrics.df))] %<>%
        convertToCurrency()
    metrics.df$weekday <- metrics.df$Date %>%
        format("%A") %>%
        factor(levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
    
    metrics.df %<>% 
        stats::setNames(c('date', 'rank', 'domestic_daily_gross', 'percent_change', 'theaters', 'per_theater', 'domestic_total_gross', 'day', 'weekday')) %>%
        .[, c('day', 'date', 'weekday', 'rank', 'domestic_total_gross', 'domestic_daily_gross', 'percent_change', 'theaters', 'per_theater')]
    
    return(metrics.df)
    
}
