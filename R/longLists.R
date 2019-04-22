coerceToLongList <- function(movieMetricsList) {
    
    df1 <- data.frame(matrix(NA, ncol = 11, nrow = length(movieMetricsList))) %>%
        setNames(c(
            "title", 
            "release_date",
            "domestic_gross", 
            "total_gross",
            "distributor", 
            "genre", 
            "runtime", 
            "MPAA_rating", 
            "budget",
            "days_in_theaters", 
            "days_in_boxoffice"
        )) %>% 
        setRowNames(names(movieMetricsList)) 
    for (movieMetrics in movieMetricsList) {
        #print(movieMetrics)
        df1[movieMetrics$title, ] <- list(
            movieMetrics$title,
            as.character(movieMetrics$release_date), 
            max(0, movieMetrics$domestic_gross),
            max(0, movieMetrics$total_gross),
            max("", movieMetrics$distributor),
            max("", movieMetrics$genre),
            max(movieMetrics$runtime, 0),
            max("", movieMetrics$MPAA_rating),
            max(0, movieMetrics$budget),
            max(0, length(movieMetrics)),
            sum((metrics(movieMetrics)$rank %>% na.remove())> 0)
        )
    }
    df1$release_date <- as.Date(df1$release_date)
    df1$distributor <- factor(df1$distributor)
    df1$genre <- factor(df1$genre)
    df1$MPAA_rating <- factor(df1$MPAA_rating)
    row.names(df1) <- NULL
    
    df2 <- lapply(movieMetricsList, function(movieMetrics) {
        as.data.frame(cbind(metrics(movieMetrics), 'title' = movieMetrics$title))
    }) 
    
    incomplete.titles <- names(df2)[lengths(df2) != 10]
    df1 %<>% 
        '['(.$title %notin% incomplete.titles, ) 
    df1 <- df1[!(is.na(df1$title)), ]
    df2 %<>% 
        '['(names(.) %notin% incomplete.titles) %>%
        do.call(rbind, .) 
    
    movieMetricsLongList <- list("Infos" = df1, "Metrics" = df2) %>%
        addClass("movieMetricsLongList")
    
    return(movieMetricsLongList)
    
}

coerceToMovieMetrics <- function(movieMetricsLongList) {
    
    stopifnot(length(movieMetricsLongList) == 1)
    
    list.results <- vector(mode = "list", length = 1 + ncol(infos(movieMetricsLongList)))
    list.results[[1]] <- metrics(movieMetricsLongList)[colnames(metrics(movieMetricsLongList)) != "title"] %>% 
        setRowNames(NULL)
    list.results[2:length(list.results)] <- infos(movieMetricsLongList) %>% 
        as.list()
    names(list.results) <- c("metrics", colnames(infos(movieMetricsLongList)))
    
    list.results %<>% addClass("movieMetrics")
    
    return(list.results)
    
}

addFeature <- function(movieMetricsLongList, feature, feature_name, append.to.metrics = TRUE, overwrite = FALSE) {
    stopifnot(length(feature) == length(movieMetricsLongList))
    if (feature_name %in% colnames(infos(movieMetricsLongList)) & overwrite == FALSE) {
        stop("Feature name already assigned. Use overwrite == TRUE to overwrite it.")
    } else if (feature_name %in% colnames(infos(movieMetricsLongList)) & overwrite == TRUE) {
        movieMetricsLongList %<>% removeFeature(feature_name)
    }
    movieMetricsLongList$Infos$newvar <- feature
    if (append.to.metrics == TRUE) {
        movieMetricsLongList$Metrics$newvar <- factor(NA, levels = levels(feature))
        for (title in names(movieMetricsLongList)) {
            movieMetricsLongList$Metrics$newvar[movieMetricsLongList$Metrics$title == title] <- as.character(movieMetricsLongList$Infos$newvar[movieMetricsLongList$Infos$title == title])
        }
        colnames(movieMetricsLongList$Metrics)[colnames(movieMetricsLongList$Metrics) == 'newvar'] = feature_name
    }
    colnames(movieMetricsLongList$Infos)[colnames(movieMetricsLongList$Infos) == 'newvar'] = feature_name
    return(movieMetricsLongList)
}

removeFeature <- function(movieMetricsLongList, feature_name) {
    movieMetricsLongList$Metrics <- movieMetricsLongList$Metrics[names(movieMetricsLongList$Metrics) != feature_name]
    movieMetricsLongList$Infos <- movieMetricsLongList$Infos[colnames(movieMetricsLongList$Infos) != feature_name]
    return(movieMetricsLongList)
}
