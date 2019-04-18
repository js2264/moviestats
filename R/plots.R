

plot.metric <- function(movieMetrics, metric = "gross", plotly = T) {
    
    require(ggplot2)
    
    metrics$day <- lubridate::wday(metrics$date, label = T, week_start = 1)
    p <- ggplot2::ggplot(metrics, aes_string(x = "date", y = metric)) + 
        geom_point(aes(col = day)) + 
        geom_smooth() +
        theme_classic() +
        labs(title = attr(metrics, 'movie'))
    if (plotly)
        p <- ggplotly(p)
    return(p)
}
