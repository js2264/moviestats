actors <- function(title.id) {
    
    title.url <- paste0("https://www.boxofficemojo.com/movies/?id=", title.id, ".htm")
    title.page <- xml2::read_html(title.url)
    actors <- title.page %>%
        rvest::html_nodes(".mp_box_content tr:nth-child(1) td+ td a") %>%
        rvest::html_text()
    
}

getTitles <- function(n.pages) {
    
    titles <- vector(mode = "character", length = 0)
    
    for (page in 1:n.pages) {
        
        if (page %% 10 == 0) 
            message('Page ', page, ' ...')
        title.url <- paste0("https://www.boxofficemojo.com/alltime/domestic.htm?page=", page, "&p=.htm")
        title.page <- xml2::read_html(title.url)
        title.page %>%
            rvest::html_nodes("tr+ tr a b") %>%
            rvest::html_text() %>% 
            tail(-2) %>%
            c(titles, .) -> titles
    }
    
    return(titles)
}

