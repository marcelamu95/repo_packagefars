#' Title 1 fars_read
#'
#' This function receives a .csv file and returns tbl_df of the data
#' This function print an error message when  .csv file does not exist
#' @param filename a file path that may or may not exist
#'
#' @return a dplyr tbl_df of the data or an error message if the file path does not exist
#'
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @source extdate/accident_year.csv.bz2
#'
#' @examples
#'
#' \dontrun{x <- setwd(system.file("extdata", package = "packagefars"))}
#' \dontrun{fars_read("accident_2013.csv.bz2")}
#' \dontrun{setwd(x)}

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Title make_filename
#'
#' This function allows you to create accident data files per year
#' receive the file in csv.bz2 format
#'
#' @param year as a string
#'
#' @return a filename in the format 'accident_year.csv.bz2'. The function prints the file name.
#'
#' @export
#'
#' @examples
#' \dontrun{x <- setwd(system.file("extdata", package = "packagefars"))}
#' \dontrun{make_filename(2013)}
#' \dontrun{setwd(x)}

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Title fars_read_years
#'
#' This function receives a list of years (one or several years)
#' and calls the make_filename()
#' function for each year and then fills those files with
#' the corresponding year data from the main data set.
#'
#' if one year is not valid an error will
#' be generated and the function will stop
#'
#' Uses make_filename(year)
#'
#' @param years of one or more years
#'
#' @return Creates one or more datasets based on year number.
#' Returns NULL if there is an error
#'
#' @export
#'
#' @importFrom dplyr mutate select %>%
#'
#' @examples
#' \dontrun{x <- setwd(system.file("extdata", package = "packagefars"))}
#' # Data with the car fatalities in 2013.
#' \dontrun{fars_read_years(2013)}
#' # Data with the car fatalities in 2013 and 2014.
#' \dontrun{fars_read_years(c(2013, 2014))}
#' \dontrun{setwd(x)}

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

#' Title fars_summarize_years
#'
#' Receive a list of years (one or several years) and
#' pass it to the function fars_read_years ().
#' This function receives a set of data with the month and year columns.
#' It uses several functions of dplyr to count the number of observations
#' per month for each year.
#'
#' Uses fars_read_years(years)
#'
#' @param  years One or more years, no error checking
#'
#' @return A data frame of counts by month and year
#'
#' @export
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#'
#' \dontrun{x <- setwd(system.file("extdata", package = "packagefars"))}
#' # Summary with the tallies of car fatalities in 2013.
#' \dontrun{fars_summarize_years(2013)}
#' # Summary with the tallies of car fatalities in 2013 and 2014.
#' \dontrun{fars_summarize_years(c(2013, 2014))}
#' \dontrun{setwd(x)}

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Title fars_map_state
#'
#' Receive a state number and one year.
#' make the appropriate file name using the year
#' and the make_filename function obtains a data frame from fars_read()
#'
#' Error checks to make sure the state number exists
#'
#' Uses make_filename(year)
#'      fars_read(filename)
#'
#' @param  state.num Number of a state
#' @param year The year in question
#'
#' @return A plot or set of plots based
#'  on latitude and longitude from the data file
#'
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' # We set the working directory where the compressed files are stored within
#' # the package.
#' \dontrun{x <- setwd(system.file("extdata", package = "packagefars"))}
#' \dontrun{setwd(x)}

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

if(getRversion() >= "2.15.1")  utils::globalVariables(c("MONTH", "STATE", "n", "year"))

