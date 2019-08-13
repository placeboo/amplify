#' Generate a continuous sequence dates
#'
#' @param start.date A number represents a starting date
#' @param end.date A number represents a ending date
#' @return A data frame with one column called date, and the date column displays a continouse sequence of dates from the starting date to the ending date.
#' @examples
#' build_date(20190101, 20190301)


build_date = function(start.date,
                      end.date) {
    start.date = ymd(start.date)
    end.date = ymd(end.date)

    if (start.date > end.date) {
        stop("starting date should be early than ending date!")
    } else {
        date = seq(from = start.date, to = end.date, by = "day")
        data.frame(date = date)
    }
}
