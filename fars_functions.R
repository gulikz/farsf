#' @title Fatality Analysis Report System
#'
#' Data from the US National Highway Traffic Safety Administration
#' @url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#' @note nationwide census providing the American public yearly data regarding fatal injuries
#' @note suffered in motor vehicle traffic crashes.

#' @description Dataset contains yearly data about fatal injuries during motor vehicle traffic crashes.
#' The first chunck of functions were written to import csv file with all fatal injuries
#' @param data frame
#' @examples function filename looks for the specific file
#' @examples to import dataset into R function read_csv(filename) will be used.
#' @return dataframe with annual fatal motorbike injuries
#' @export fars_map_state

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' @parameter year column in data frame
#' @examples year <- as.integer(year)
#' @return formatted file
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' next code is dedicated for reading years, creating new column with month and year
#' @parameter year as integer
#' @examples fars_read(year)
#' @examples creates new column: mutate(dat, year = year)
#'
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' The next functions allows to summarize an filter by year and months
#' @parameter integer
#' @example group_by, summarize, spread, filter: %>%
#' @return summarized rows by year and month
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' fars_map_state creates copies year data to new file
#' then filtering function among states, then clear NA values from longitude and latitudes
#' the last function create map of states, then draws motorbike accidents by their locations - longitude and latitude
#' @function make_filename(year)
#' @function fast_read(filename)
#' @function filter data from dplyr library
#' @function map ("state") - create map of states
#' @function points (LONGITUD, LATITUDE, pch = 46) - draw points on previous created map
#'
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
#' testing functions with testthat:
library('testthat')
testthat::test_dir('Fars/R')

