#' @importFrom utils head
#' @importFrom utils tail
#' @export

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
#' @export

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

#' @export

`[.moviesDB` <- function(moviesDB, vec) {
    moviesDB <- list(
        'movies.titles' = moviesDB$movies.titles[vec], 
        'movies.urls' = moviesDB$movies.urls[vec] 
    ) %>% 
        addClass("moviesDB")
    return(moviesDB)
}

# --------------

#' @export

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

#' @export

`[.movieMetrics` <- function(movieMetrics, range) {
    metrics.df <- metrics(movieMetrics)
    if (is.numeric(range)) {
        metrics.df %<>% .[range,]
        movieMetrics$metrics <- metrics.df
        return(movieMetrics)
    } 
}

#' @export

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

#' @export

length.movieMetrics <- function(movieMetrics) {
    nrow(metrics(movieMetrics))
}

#' @export

str.movieMetrics <- function(movieMetrics) {
    class(movieMetrics) <- "list"
    str(movieMetrics)
}

# ---------

#' @export

names.movieMetricsLongList <- function(movieMetricsLongList) {
    movieMetricsLongList$Infos$title
}

#' @export

`[.movieMetricsLongList` <- function(movieMetricsLongList, titles) {
    
    if (class(titles) == "character") {
        titles %<>%
        '['(. %in% names(movieMetricsLongList))
    } else if (class(titles) == "integer" || class(titles) == "numeric") {
        titles <- names(movieMetricsLongList)[titles]
    }
    
    df1 <- infos(movieMetricsLongList)[names(movieMetricsLongList) %in% titles, ]
    df2 <- metrics(movieMetricsLongList)[metrics(movieMetricsLongList)$title %in% titles, ]
    movieMetricsLongList <- list("Infos" = df1, "Metrics" = df2) %>%
        addClass("movieMetricsLongList")
    
    if (length(movieMetricsLongList) == 1) {
        movieMetricsLongList %<>% coerceToMovieMetrics()
    }
    
    return(movieMetricsLongList)

}

#' @export

print.movieMetricsLongList <- function(x) {
    n.movies <- nrow(x$Infos)
    
    message("# ", n.movies, " movies in movieMetricsLongList object")
    print(names(x))
    
    invisible(x)
}

#' @export

head.movieMetricsLongList <- function(movieMetricsLongList) {
    head(movieMetricsLongList$Infos)
}

#' @export

length.movieMetricsLongList <- function(movieMetricsLongList) {
    length(names(movieMetricsLongList))
}

#' @export

str.movieMetricsLongList <- function(movieMetricsLongList) {
    class(movieMetricsLongList) <- "list"
    str(movieMetricsLongList)
}
