add.class <- function(x, newclass) {
    
    class(x) <- c(newclass, class(x))
    return(x)
    
}

print.movieMetrics <- function(x) {
   message("# Movie: ", x$title)
   message("# Release date: ", x$release_date)
   message("# Total gross: ", paste0('$', formatC(x$total_gross, format = "f", big.mark = ',', digits = 0, decimal.mark = '.')))
   message("# Genre: ", x$genre)
   message("# Runtime: ", x$runtime)
   message("# Budget: ", paste0('$', formatC(x$budget, format = "f", big.mark = ',', digits = 0, decimal.mark = '.')))
   message("# ... Other infos: ", paste(names(x)[names(x) %notin% c("metrics", "title", "id", "release_date", "total_gross", "genre", "runtime", "budget")], collapse = ', '))
   message("")
   df.h <- head(x$metrics, 3)
   df.t <- tail(x$metrics, 3)
   if (nrow(x$metrics) > 3) {
       print(df.h)
       message("\t\t...\t\t...\t\t...\t\t...")
       print(df.t)
   } else {
       print(x$metrics)
   }
   invisible(x)
}

print.movieMetricsList <- function(x) {
   
   n.movies <- length(x)
   
   message("# ", n.movies, " in movieMetricsList object")
   lapply(names(x), function(NAME) {
       message(NAME)
   })

}

print.Currency <- function(value, currency.sym = "$", digits = 0, sep = ",", decimal = ".") {
    print(paste(
        currency.sym,
        formatC(value, format = "f", big.mark = sep, digits = digits, decimal.mark = decimal),
        sep=""
    ))
}
