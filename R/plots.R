#' @import lubridate ggplot2
#' @rawNamespace import(plotly, except = last_plot)

plot.metric <- function(movieMetrics, metric = "gross", plotly = TRUE) {
    
    movieMetrics$day <- lubridate::wday(movieMetrics$date, label = T, week_start = 1)
    p <- ggplot2::ggplot(movieMetrics, ggplot2::aes_string(x = "date", y = metric)) + 
        ggplot2::geom_point(aes(col = day)) + 
        ggplot2::geom_smooth() +
        ggplot2::theme_classic() +
        ggplot2::labs(title = attr(movieMetrics, 'movie'))
    if (plotly)
        p <- plotly::ggplotly(p)
    return(p)
}
