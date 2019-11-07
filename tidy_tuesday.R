#' @title Browse #TidyTuesday Repo And Load csv files 
#' 
#' @details getting data from #TidyTuesday repo with ease! No more 
#' excessive url slug passed to \code{read_csv()}. It opens a browser
#' being directed to most recent week of tidytuesday's data folder when
#' only \code{NULL} are passed to arguments. It recognizes iso week number
#' so you don't pass date explicitly. Only week 22 to 51 are available
#' for year 2018, as for previous weeks they are all on Mondays.
#' 
#' 
#' @param filename Single character for csv file
#' @param year numeric. 2018 or 2019
#' @param week numeric. Between 1 and 52
#' @param save boolean. Do you want to save a local copy?
#' 
#' @rdname TidyTuesday
#' 
tidy_tuesday <- function(filename = NULL, year = NULL, week = NULL, 
                         save = FALSE) {
        `%||%` <- function(lhs, rhs) if(!is.null(lhs)) lhs else rhs
        
        today <- Sys.Date()
        year <- year %||% format(today, "%Y")
        date_origin <- as.Date(paste0(year, "-01-01"))
        weekday <- lubridate::wday(date_origin + 0:6)
        first_tue <- (date_origin + 0:6)[which(weekday == 3)]
        week <- week %||% lubridate::week(today)
        if(!week > 0 | !week < 53) {
                stop("Please specify a week number between 1 and 52.")
        }
        
        if(year == 2018 & !week %in% 22:51) {
                stop("Week 22 to 51 is available in 2018.")
        }
        
        host <- "https://raw.githubusercontent.com/"
        repo <- "rfordatascience/"
        proj <- "tidytuesday/master/data/"
        year <- paste0(year, "/")
        date <- paste0((first_tue + (week - 1) * 7), "/")
        addr <- paste0(
                "https://github.com/", repo, 
                "tidytuesday/tree/master/data/", year, date
        )
        url <- paste0(host, repo, proj, year, date, filename)
        if(is.null(filename)) {
                browseURL(addr)
        } else if(length(filename) != 1) {
                stop("`filename` is a character of length 1.")
        } else if(!grepl("\\.csv", filename)) {
                stop("Incorrect file type.")
        } else io <- readr::read_csv(url)
        
        if(save) {
                thisweek <- paste("[W|w]eek[.|\\s|-]?", week)
                directories <- list.files()
                folder_exists <- grepl(thisweek, directories)
                if(any(folder_exists)) {
                        path <- here::here(directories[which(folder_exists)], 
                                           filename)
                } else {
                        thisweek <- paste("Week", week)
                        dir.create(here::here(thisweek))
                        path <- here::here(thisweek, filename)
                }
                
                # skip writing files if it already existed
                isduplicate <- list.files(here::here(thisweek))
                isduplicate <- grepl(filename, isduplicate)
                isduplicate <- any(isduplicate)
                if(!isduplicate) readr::write_csv(x = io, path = path)
                return(io)
        }
        
        if(exists("io")) return(io)
}
