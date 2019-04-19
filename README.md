# movistats R package

Still in development

To install the dev version from Github, run 

```{r}
    install.packages("devtools")
    devtools::install_github("js2264/moviestats")
    library(moviestats)
```

```{r}
    # Quick search to find the exact title of the movie The Greatest Showman
    searchTitle(pattern = 'greatest showman', moviesDB = moviesDB)
    greatestshowman <- fetchMovie(title = 'The Greatest Showman', moviesDB = moviesDB)
```