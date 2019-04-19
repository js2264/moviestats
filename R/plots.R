#' @import ggplot2
#' @importFrom plotly ggplotly

plot.movieMetrics <- function(
    
    movieMetrics, 
    xaxis = "date", 
    yaxis = "domestic_daily_gross", 
    plotly = TRUE,
    time.range = NULL
    
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
            p <- p + 
                scale_x_date(
                    date_breaks = "1 month", 
                    date_labels =  "%b %Y",
                    limits = as.Date(time.range)
                )
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
