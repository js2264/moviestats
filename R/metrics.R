#' A useful search function
#' 
#' Used to find all titles in MOJO database matching a specific pattern
#' 
#' @param pattern a character vector of length 1
#' @return a vector of matching titles
#' @keywords search title
#' @seealso fetchMovie()
#' @export
#' @examples
#' searchTitles('star wars')
#' 
#' @import rvest xml2 magrittr
#' @importFrom magrittr "%>%"

searchTitles <- function(pattern) {
    
    query <- gsub(' ', '%20', tolower(pattern))
    url_search <- paste0("https://www.boxofficemojo.com/search/?q=", query)
    search_results <- xml2::read_html(url_search) %>% 
        rvest::html_nodes("table+ table tr+ tr td:nth-child(1)")
    available.titles <- search_results %>%
        rvest::html_text() %>% 
        grep("\n\t\t", ., value = T) %>% 
        gsub("\n\t\t", "", .)
    
    return(available.titles)

}

#' An internal function
#' 
#' Used to transform the data retrieved from MOJO intro an organized dataframe.
#'   This function should only be called within fetchMovie().
#' 
#' @seealso fetchMovie()
#' @export
#' 
#' @importFrom magrittr "%>%"

fixMetrics <- function(title.results) {
    
    results <- vector("list", 9) %>%
        setNames(colnames(title.results))
    
    first.day <- title.results[1, 'date'][[1]]
    year <- first.day %>% gsub(".*, ", "", .)
    month <- match(first.day %>% gsub("\\.| .*", "", .), month.abb)
    day <- first.day %>% gsub(",.*", "", .) %>% gsub(".* ", "", .)
    first.day <- as.Date(paste(year, month, day, sep = '-'))
    results[['date']] <- c(first.day + seq(1:nrow(title.results)) - 1)
    
    results[['rank']] <- title.results[,'rank'] %>% 
        value.replace("-", NA) %>% 
        as.numeric()
    
    results[['gross']] <- title.results[,'gross'] %>% 
        gsub("\\$|,", "", .) %>% 
        as.numeric() %>% 
        add.class("Currency")
        
    results[['daily_percent']] <- title.results[,'daily_percent'] %>% 
        gsub("\\%|,", "", .) %>% 
        value.replace("-", 0) %>%
        as.numeric()

    results[['weekly_percent']] <- title.results[,'weekly_percent'] %>% 
        gsub("\\%|,", "", .) %>% 
        value.replace("-", 0) %>%
        as.numeric()
    
    results[['theaters']] <- title.results[,'theaters'] %>% 
        gsub(",", "", .) %>% 
        as.numeric()

    results[['per_theater']] <- title.results[,'per_theater'] %>% 
        gsub("\\$|,", "", .) %>% 
        as.numeric()

    results[['cum_gross']] <- title.results[,'cum_gross'] %>% 
        gsub("\\$|,", "", .) %>% 
        as.numeric() %>% 
        add.class("Currency")
        
    results[['day']] <- title.results[,'day'] %>% 
        as.numeric()
    
    return(data.frame(results))

}

#' An internal function
#' 
#' Used to add some extra information to a movieMetrics object.
#'   This function should only be called within fetchMovie().
#' 
#' @seealso fetchMovie()
#' @export
#' @import rvest xml2
#' 
#' @importFrom magrittr "%>%"

enrichMetrics <- function(title.results, title.page, title.id) {
    
    url <- paste0("https://www.boxofficemojo.com/movies/?id=", title.id, ".htm")
    
    domestic_gross <- title.page %>%
        rvest::html_nodes("td td td tr:nth-child(1) td") %>% 
        rvest::html_text() %>% 
        gsub(".*\\$|,", "", .) %>%
        as.numeric() %>% 
        add.class("Currency")
    
    total_gross <- url %>% 
         xml2::read_html() %>% 
         rvest::html_nodes("td td td td tr+ tr td+ td b") %>% 
         rvest::html_text() %>% 
         gsub(".*\\$|,", "", .) %>%
         as.numeric() %>% 
         add.class("Currency")
    
    distributor <- title.page %>%
        rvest::html_nodes("center b > a") %>% 
        rvest::html_text()
    
    release_date <- title.page %>%
        rvest::html_nodes("nobr a") %>% 
        rvest::html_text()
    
    genre <- title.page %>%
        rvest::html_nodes("tr:nth-child(3) td:nth-child(1) b") %>% 
        rvest::html_text()
    
    runtime <- title.page %>%
        rvest::html_nodes("td td td tr:nth-child(3) td+ td b") %>% 
        rvest::html_text()
    
    MPAA_rating <- title.page %>%
        rvest::html_nodes("tr:nth-child(4) td:nth-child(1) b") %>% 
        rvest::html_text()
    
    budget <- title.page %>%
        rvest::html_nodes("tr:nth-child(4) td+ td b") %>% 
        rvest::html_text() %>% 
        .[1] %>%
        gsub("\\$| ", "", .) %>%
        gsub("million", "000000", .) %>% 
        gsub("thousand", "000", .) %>% 
        as.numeric() %>%
        add.class("Currency")

    title <- title.page %>%
        rvest::html_nodes("br+ font b") %>% 
        rvest::html_text()
    
    list.results <- list(
        'metrics' = title.results,
        'title' = title,
        'id' = title.id,
        'release_date' = release_date,
        'domestic_gross' = domestic_gross,
        'total_gross' = total_gross,
        'distributor' = distributor,
        'genre' = genre,
        'runtime' = runtime,
        'MPAA_rating' = MPAA_rating,
        'budget' = budget,
        'url' = url
    )
    
    class(list.results) <- c('movieMetrics', class(list.results))
    
    return(list.results)
    
}

#' Core function
#' 
#' Used to retrieve data about a movie using MOJO database
#' 
#' @param title a character vector of length 1
#' @return a movieMetrics object
#' @keywords movieMetrics
#' @seealso fetchMovie() fetchMoviesList()
#' @export
#' @examples 
#' fetchMovie('Star Wars')
#' 
#' @import rvest xml2 
#' @importFrom magrittr "%>%"

fetchMovie <- function(title) {
    
    require(magrittr)
    
    query <- gsub(' ', '%20', tolower(title))
    url_search <- paste0("https://www.boxofficemojo.com/search/?q=", query)
    search_results <- xml2::read_html(url_search) %>% 
        rvest::html_nodes("table+ table tr+ tr td:nth-child(1)")
    available.titles <- search_results %>%
        rvest::html_text() %>% 
        grep("\n\t\t", ., value = T) %>% 
        gsub("\n\t\t", "", .)
    which.title <- which(available.titles == title)
    if (length(which.title) < 1) {
        message("Be more specific in your movie title. The following titles matched your query:")
        return(available.titles)
    } else if (length(which.title) > 1) {
        warning("[WARNING]: More than one movie have this exact title. Fetching results for the most recent one.")
        which.title <- which.title[1]
    }
    title.id <- search_results %>% 
        .[which.title] %>%
        sub('.*"(.*)".*', "\\1", .) %>% 
        gsub(".*=|.htm", "", .)
    title.url <- paste0("https://www.boxofficemojo.com/movies/?page=daily&view=chart&id=", title.id, ".htm")
    title.page <- xml2::read_html(title.url) 
    title.results <- title.page %>%
        rvest::html_nodes(".chart-wide td+ td") %>% 
        rvest::html_text() %>% 
        tail(-7) %>%
        matrix(., ncol = 9, byrow = T) %>% 
        setColNames(c("date", "rank", "gross", "daily_percent", "weekly_percent", "theaters", "per_theater", "cum_gross", "day"))
    if (nrow(title.results) < 1) {
        warning(paste0(title, " is referenced in Boxoffice MOJO database but does not have any data yet. Come back later!"))
        return(NA)
    }
    title.results %>% 
        fixMetrics() %>%
        enrichMetrics(title.page, title.id) -> title.results
    return(title.results)

}

#' Core function
#' 
#' Used to retrieve data about a list of movies using MOJO database
#' 
#' @param title a character vector of any length
#' @return a movieMetricsList object
#' @keywords movieMetricsList
#' @seealso fetchMovie() fetchMoviesList()
#' @export
#' @examples 
#' titles <- searchTitles('star wars')
#' fetchMoviesList(titles)
#' 
#' @importFrom magrittr "%>%"

fetchMoviesList <- function(titles) {
    
    movieMetricsList <- lapply(titles, fetchMovie) %>% 
        setNames(titles) %>% 
        add.class("movieMetricsList")
    
    return(movieMetricsList)
    
}

