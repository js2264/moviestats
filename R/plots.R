#' @import ggplot2
#' @importFrom plotly ggplotly

plot.movieMetrics <- function(
    
    movieMetrics, 
    xaxis = "date", 
    yaxis = "domestic_daily_gross", 
    time.range = NULL,
    plotly = TRUE
    
    ) {
    
    metrics.df <- metrics(movieMetrics)
    metrics.df$domestic_daily_gross %<>% 
        "/"(1e6) %>% 
        as.numeric() %>% 
        round(1) 
    metrics.df$domestic_total_gross %<>% 
        "/"(1e6) %>% 
        as.numeric() %>% 
        round(1) 
    
    p <- ggplot(metrics.df, aes_string(
        x = xaxis, 
        y = yaxis, 
        "date" = "date", 
        "consecutive days" = "day", 
        "domestic total gross (in M.)" = "domestic_total_gross", 
        "theaters" = "theaters"
    )) + 
        geom_point(aes_string(col = "weekday")) + 
        geom_smooth() +
        theme_classic() +
        labs(
            title = movieMetrics$title, 
            y = gsub("_", " ", yaxis) %>% capitalizeFirstLetter(), 
            x = gsub("_", " ", xaxis) %>% capitalizeFirstLetter(),
            col = "Day of week"
        )
    
    if (xaxis == "date") {
        if (is.null(time.range)) {
            p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")
        } else {
            if (length(time.range) == 2) {
                p <- p + 
                    scale_x_date(
                        date_breaks = "1 month", 
                        date_labels =  "%b %Y",
                        limits = as.Date(time.range)
                    )
            } else if (length(time.range) == 1 && time.range > 0 && is.numeric(time.range)) {
                p <- p + 
                    scale_x_date(
                        date_breaks = "1 month", 
                        date_labels =  "%b %Y",
                        limits = as.Date(c(metrics.df[1,'date'], (metrics.df[1,'date'] + time.range)))
                    )
            }
        }
    }
    if (yaxis == "domestic_daily_gross" || yaxis == "domestic_total_gross")
        p <- p + labs(y = paste(gsub("_", " ", yaxis) %>% capitalizeFirstLetter(), "(in M.)"))
    
    if (plotly)
        p <- plotly::ggplotly(
            p,
            tooltip = c("date", "consecutive days", "domestic total gross (in M.)", "theaters")
        )
    
    return(p)
    
}

plot.movieMetricsLongList <- function(movieMetricsLongList, x = "day", y = "domestic_total_gross", col = "title", fill = NULL, facet = NULL, xlim = NULL, ylim = NULL) {
    
    p <- ggplot(metrics(movieMetricsLongList), aes_string(x, y, col = col, fill = fill)) + 
        geom_point(aes_string(col = "title")) + 
        theme_bw() +
        theme(legend.position = "bottom")
    if (!is.null(facet)) 
        p <- p + facet_wrap(facet)
    if (!is.null(xlim))
        p <- p + xlim(xlim)
    if (!is.null(ylim))
        p <- p + xlim(ylim)
    
    return(p)
    
}