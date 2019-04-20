#' @importFrom utils head
#' @importFrom utils tail

print.Currency <- function(value, currency.sym = "$", digits = 0, sep = ",", decimal = ".") {
    print(paste(
        currency.sym,
        formatC(value, format = "f", big.mark = sep, digits = digits, decimal.mark = decimal),
        sep=""
    ))
}

# --------------

#' @importFrom utils head
#' @importFrom utils tail

print.moviesDB <- function(x) {
    message(
        "Nb of movies in database: ", length(x[[1]])
    )
    df.h <- utils::head(x[[1]], 3)
    if (length(x[[1]]) > 3) {
        print(df.h)
        message("\t\t...")
    } else {
        print(x[[1]])
    }
    invisible(x)
}

'[.moviesDB' <- function(moviesDB, vec) {
    moviesDB <- list(
        'movies.titles' = moviesDB$movies.titles[vec], 
        'movies.urls' = moviesDB$movies.urls[vec] 
    ) %>% 
        addClass("moviesDB")
    return(moviesDB)
}

# --------------

print.movieMetrics <- function(x) {
    message("# Movie: ", x$title)
    message("# Release date: ", x$release_date)
    message("# Total gross: ", paste0('$', formatC(x$total_gross, format = "f", big.mark = ',', digits = 0, decimal.mark = '.')))
    message("# Genre: ", x$genre)
    message("# Runtime: ", x$runtime)
    message("# Budget: ", paste0('$', formatC(x$budget, format = "f", big.mark = ',', digits = 0, decimal.mark = '.')))
    message("# ... Other infos: ", paste(names(x)[names(x) %notin% c("metrics", "title", "id", "release_date", "total_gross", "genre", "runtime", "budget")], collapse = ', '))
    message("")
    print(utils::head(x$metrics, 3))
    message("\t\t...\t\t...\t\t...\t\t...")
}

'[.movieMetrics' <- function(movieMetrics, range) {
    metrics.df <- metrics(movieMetrics)
    if (is.numeric(range)) {
        metrics.df %<>% .[range,]
        movieMetrics$metrics <- metrics.df
        return(movieMetrics)
    } 
}

filter.movieMetrics <- function(movieMetrics, range) {
    metrics.df <- metrics(movieMetrics)
    if (is.numeric(range)) {
        metrics.df %<>% .[range,]
        movieMetrics$metrics <- metrics.df
        return(movieMetrics)
    } else if (class(range) == 'Date' & length(range) == 2) {
        metrics.df %<>% '['(.$date %in% range[1]:range[2], )
        movieMetrics$metrics <- metrics.df
        return(movieMetrics)
    }
}

length.movieMetrics <- function(movieMetrics) {
    nrow(metrics(movieMetrics))
}

str.movieMetrics <- function(movieMetrics) {
    class(movieMetrics) <- "list"
    str(movieMetrics)
}

# ---------

names.movieMetricsLongList <- function(movieMetricsLongList) {
    movieMetricsLongList$Infos$title
}

'[.movieMetricsLongList' <- function(movieMetricsLongList, titles) {
    
    titles %<>%
        '['(. %in% names(movieMetricsLongList))
    
    df1 <- movieMetricsLongList$Infos[titles, ]
    df2 <- movieMetricsLongList$Metrics[names(movieMetricsLongList) %in% titles, ]
    movieMetricsLongList <- list("Infos" = df1, "Metrics" = df2) %>%
        addClass("movieMetricsLongList")
        
    return(movieMetricsLongList)
}

print.movieMetricsLongList <- function(x) {
    n.movies <- nrow(x$Infos)
    
    message("# ", n.movies, " movies in movieMetricsLongList object")
    print(names(x))
    
    invisible(x)
}

head.movieMetricsLongList <- function(movieMetricsLongList) {
    head(movieMetricsLongList$Infos)
}

length.movieMetricsLongList <- function(movieMetricsLongList) {
    length(names(movieMetricsLongList))
}

str.movieMetricsLongList <- function(movieMetricsLongList) {
    class(movieMetricsLongList) <- "list"
    str(movieMetricsLongList)
}