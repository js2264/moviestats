#' @export

findIMDbIndex <- function(title, movieMetricsLongList) {
    
    movie_title <- gsub(" \\(.*", "", title) %>% gsub("\\s", "\\+", .) %>% tolower()
    movie_year <- movieMetricsLongList[title]$release %>% substr(start = 1, stop = 4)
    search_url <- paste0(
        "https://www.imdb.com/search/title?title=", 
        paste(movie_title, collapse = '+'), 
        "&release_date=", movie_year, "-01-01,", movie_year, "-12-31"
    )
    movieID <- xml2::read_html(search_url) %>%
        rvest::html_nodes(".mode-advanced:nth-child(1) .lister-item-header a") %>% 
        gsub(".*/title/(tt.......)/.*","\\1", .)
    movie_url <- paste0("https://www.imdb.com/title/", movieID)
    movie_page <- xml2::read_html(movie_url)
    
    metascore <- movie_page %>% 
        rvest::html_nodes(".score_favorable span") %>% 
        rvest::html_text()
    score <- movie_page %>% 
        rvest::html_nodes("strong span") %>% 
        rvest::html_text()
    
    director <- movie_page %>% 
        rvest::html_nodes(".credit_summary_item") %>% 
        .[[1]] %>%
        rvest::html_nodes("a") %>% 
        rvest::html_text()
    
    stars <- movie_page %>% 
        rvest::html_nodes(".credit_summary_item") %>% 
        .[[3]] %>%
        rvest::html_nodes("a") %>% 
        rvest::html_text() %>% 
        '['(-length(.))
    
    production <- xml2::read_html(paste0(movie_url, "/companycredits")) %>% 
        rvest::html_nodes("#production+ .simpleList a") %>% 
        rvest::html_text()
    
    
    l <- list(
        "director" = director,
        "stars" = stars,
        "production" = production, 
        "score" = score,
        "metascore" = metascore,
        "IMDb_idx" = movieID
    )
    
    return(l)
    
}
