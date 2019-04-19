`%notin%` <- Negate(`%in%`)

# -------------

metrics <- function(movieMetrics) {
    return(movieMetrics$metrics)
}

convertToCurrency <- function(vec) {
    result <- vec %>%
        gsub(".*\\$|,", "", .) %>%
        as.numeric() %>% 
        add.class("Currency")
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