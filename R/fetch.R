#' A core function
#' 
#' Used to find all titles available in the first n.pages of The Numbers database
#' 
#' @param n.pages an integer. The number of pages to scan
#' @param verbose an logical. Should the pages scraping process be verbose?
#' @return a moviesDB object. A list of length 2, the first element of the list is a vector 
#'   of available titles, the second element contains the corresponding urls. 
#' @keywords searchTitle
#' @seealso fetchMovie() searchTitle()
#' @export
#' @examples
#' moviesDB <- availableTitles(n.pages = 10, verbose = TRUE)
#' 
#' @import rvest xml2 
#' @importFrom magrittr "%>%"

availableTitles <- function(n.pages = 52, verbose = TRUE) {
    
    if (n.pages > 52)
    stop("There are only 52 pages of movies (5200 movies!) in The Numbers database.")
    
    titles <- vector(mode = "list", length = n.pages)
    urls <- vector(mode = "list", length = n.pages)
    
    for (page in 1:n.pages) {
        if (page %% 10 == 0 & verbose)
        message("Page ", page, "... ")
        if (page == 1) {
            url_search <- "https://www.the-numbers.com/movie/budgets/all"
        } else {
            url_search <- paste0(
                "https://www.the-numbers.com/movie/budgets/all/", 
                100 * (page - 1) + 1
            )
        }
        search_table <- xml2::read_html(url_search) %>% 
        rvest::html_nodes("td:nth-child(3)")
        titles[[page]] <- search_table %>%
        rvest::html_text() %>%
        gsub(" *$", "", .)
        urls[[page]] <- search_table %>% 
        rvest::html_nodes('a') %>% 
        gsub(".*/movie/", "/movie/", .) %>% 
        grep("#tab=summary", ., value = TRUE) %>% 
        gsub("#tab=summary.*", "#tab=box-office", .) %>%
        paste0("https://www.the-numbers.com", .)
    }
    
    titles <- do.call(c, titles)
    urls <- do.call(c, urls)
    
    moviesDB <- list(
        "movies.titles" = titles,
        "movies.urls" = urls
    ) %>% 
    fixDuplicatedMovies() %>%
    addClass("moviesDB")
    
    return(moviesDB)
}

#' A useful search function
#' 
#' Used to find all titles in MOJO database matching a specific pattern
#' 
#' @param pattern a character vector of length 1
#' @param moviesDB a list of length 2, the first element containing movies 
#'   names and the second associated URLs.
#' @param exact.match logical. Whether to perform an exact match 
#' @param return.list logical. Whether to return the list of matched titles
#' @param return.url logical. Whether to return URLs instead of matched titles 
#'   names (cannot be used with `return.list = T`)
#' @return a vector of matching title or matching URLs
#' @seealso fetchMovie()
#' @export
#' @examples
#' searchTitle('star wars', moviesDB)
#' 

searchTitle <- function(pattern, moviesDB, exact.match = FALSE, return.list = FALSE, return.url = FALSE) {
    if (exact.match) {
        return.list <- FALSE
        results <- grep(paste0("^", gsub("\\(", "\\\\(", pattern) %>% gsub("\\)", "\\\\)", .), "$"), moviesDB$movies.titles, ignore.case = F, value = TRUE)
        if (length(results) > 1) {
            print(results)
            message("You used the option 'exact.match = T' and your research returned multiple results.")
            message("Try again.")
        } else if (length(results) == 0) {
            message("No movie was found. Try without the 'exact.match = T' option.")
        } else {
            if (return.url) {
                return(moviesDB$movies.urls[moviesDB$movies.titles == results][1])
            } else {
                return(results)
            }
        }
    } else {
        results <- grep(pattern, moviesDB$movies.titles, ignore.case = T, value = TRUE)
        if (length(results) > 1) {
            if (return.list) {
                return(results)
            } else {
                print(results)
                message("More than one file found. Be more specific!")
            }
        } else if (length(results) == 0) {
            message("No movie was found. Try a simpler pattern.")
            message("For instance, to find 'Star Wars Ep. III: Revenge of the Sith',")
            message("try 'star wars' instead of 'Star Wars 7'.")
        } else {
            return(results)
        }
    }
}

#' Core function
#' 
#' Used to retrieve data about a movie using MOJO database
#' 
#' @param title a character vector of length 1
#' @param moviesDB a list of length 2, the first element containing movies 
#'   names and the second associated URLs.
#' @return a movieMetrics object
#' @keywords movieMetrics
#' @seealso fetchMovie() fetchMoviesList()
#' @return a movieMetrics object
#' @export
#' @examples 
#' fetchMovie('The Greatest Showman', moviesDB)
#' 
#' @import rvest xml2 
#' @importFrom magrittr "%>%" "%<>%"

fetchMovie <- function(title, moviesDB) {
    
    title_url <- searchTitle(title, moviesDB, exact.match = TRUE, return.url = TRUE)
    if (length(title_url) > 0) {
        title_summary <- xml2::read_html(gsub("box-office", "summary", title_url)) 
        title_boxoffice <- xml2::read_html(title_url) 
        title_metrics <- title_boxoffice %>%
            rvest::html_nodes("#box_office_chart:nth-child(9) table") %>% 
            rvest::html_table()
        if (length(title_metrics) > 0) {
            title_metrics %<>% 
                .[[1]] %>%
                fixMetrics()
        } else {
            NA
        }
        
        domestic_gross <- title_boxoffice %>%
            rvest::html_nodes("#movie_finances tr:nth-child(2) .data") %>% 
            rvest::html_text() %>% 
            convertToCurrency() 
        
        total_gross <- title_boxoffice %>% 
            rvest::html_nodes("tr:nth-child(3) .sum") %>% 
            rvest::html_text() %>% 
            convertToCurrency() 
            
        distributor <- title_summary %>%
            rvest::html_nodes("td") %>% 
            grep("/market/distributor/", ., value = TRUE) %>% 
            .[[1]] %>%
            gsub('.*\\\">|</a.*', "", .)
        
        release_date <- title_summary %>%
            rvest::html_nodes("td") %>% 
            grep("/market/distributor/", ., value = TRUE) %>% 
            .[[1]] %>%
            gsub('<td>', "", .) %>% 
            gsub(" \\D.*", "", .)
            year <- release_date %>% 
            gsub(".*, ", "", .)
        month <- release_date %>% 
            strsplit(" ") %>% .[[1]] %>% .[1] %>% match(., month.name)
        day <- release_date %>%
            strsplit(" ") %>% .[[1]] %>% .[2] %>% gsub("..,", "", .)
        release_date <- as.Date(paste(year, month, day, sep = '-'))
        
        genre <- title_summary %>%
            rvest::html_nodes("td") %>% 
            grep("genre", ., value = TRUE) %>% 
            .[[1]] %>%
            gsub('.*\\\">|</a.*', "", .)
        
        runtime <- title_summary %>%
            rvest::html_nodes("td") %>% 
            .[title_summary %>% rvest::html_nodes("td") %>% grep('Running Time', .)+1] %>%
            rvest::html_text() %>%
            gsub(" minutes", "", .)
        
        MPAA_rating <- title_summary %>%
            rvest::html_nodes("td") %>% 
            .[title_summary %>% rvest::html_nodes("td") %>% grep('MPAA', .)+1] %>%
            rvest::html_text() %>%
            gsub(" .*", "", .)
        
        budget <- title_summary %>%
            rvest::html_nodes("td") %>% 
            .[title_summary %>% rvest::html_nodes("td") %>% grep('Budget', .)+1] %>%
            rvest::html_text() %>%
            convertToCurrency()
        
        title_results <- structure(
            list(
                'metrics' = title_metrics,
                'title' = title,
                'release_date' = release_date,
                'domestic_gross' = domestic_gross,
                'total_gross' = total_gross,
                'distributor' = distributor,
                'genre' = genre,
                'runtime' = runtime,
                'MPAA_rating' = MPAA_rating,
                'budget' = budget,
                'url' = title_url
            ), 
            class = c("list", "movieMetrics")
        )
        
        return(title_results)
    }
}

#' Core function
#' 
#' Used to retrieve data about a list of movies using MOJO database
#' 
#' @param titles a character vector of any length
#' @param moviesDB a list of length 2, the first element containing movies 
#'   names and the second associated URLs.
#' @return a movieMetricsList object
#' @keywords movieMetricsList
#' @seealso fetchMovie() fetchMoviesList()
#' @return a movieMetricsList object
#' @export
#' @examples 
#' moviesDB <- availableTitles(n.pages = 20, verbose = TRUE)
#' starwarsmovies <- searchTitle('star wars', moviesDB, return.list = TRUE)
#' starwarsmovies <- fetchMoviesList(starwarsmovies, moviesDB)
#' 
#' @importFrom magrittr "%>%"
#' @importFrom stats setNames

fetchMoviesList <- function(titles, moviesDB) {
    
    l <- length(titles)
    movieMetricsList <- lapply(
        seq_along(titles),
        function(i) {
            message("Fetching [", i, "/", l, "]: ", titles[i])
            fetchMovie(titles[i], moviesDB)
        }
    ) %>% 
        setNames(titles) %>% 
        coerceToLongList()
    
    return(movieMetricsList)
    
}

