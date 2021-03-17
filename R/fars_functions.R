#' Display file content in tabular format
#'
#' @description This function takes a file name and if it does not exist,
#' it exits with an error message, saying the file does not exist.
#' Otherwise, it reads in the file sing the read_csv function from readr
#' and then displays it in tabular format as a data frame.
#' The function could be made better because the file itself may not be a csv file.
#'
#' @param filename The name of the file you want to read in and display.
#' @return This function dsiplays a data frame in tabular format.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create file name from year
#'
#' @description Create file name from year
#'
#' This function takes a year and prints the file name with the year at the end.
#' The user may supply a string instead, which needs to be error handled.
#'
#' @param year A number denoting the year of the data being processed
#' @return prints a file name in the format accident_%d.csv.bz where %d means the year.
#' @examples
#' make_filename(2017)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
#  sprintf("data/accident_%d.csv.bz2", year)
}

#' Reads in list of years
#'
#' @description Reads in a list of years and for each file whose name ends in a year within the
#' provided year range changes the year in that file.
#' Throws an error if the year range contains a year which doesn't correspond to
#' a file name with that year in its name.
#'
#' @param years A list of years
#' @return edits the data files with names ending in provided year range
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @examples
#' @export
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

#' Unknown description
#'
#' @description ?
#' @param years a list or vector of years
#' @return
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @examples
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Prints out map of accidents
#'
#' @description Prints a map of accidents in a given state for a given year
#' @param state.num the number id of a given state
#' @param year a year with accident data for it
#' @return plots the map of a state with locations of accidents in it
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' @export
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
