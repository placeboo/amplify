#' Nth weekday  of the month
#'
#' @description The given date is the Nth weekday of the month
#' @param date A numeric vector represents dates. The format is year month day, for example, 20190801.
#' @return The data frame with two columns, \code{date} (date formate y-m-d), \code{weekday}, and the \code{Nth} weekday of the month.
#' @examples
#' nweek(c(20190101: 20190131))
#' @importFrom lubridate ymd year month wday
#' @import dplyr
#' @export

nweek = function(date) {
    date = ymd(date)
    dat = data.frame(date = date)
    dat = dat %>%
        mutate(weekday = wday(date, label = TRUE),
               Nth = ceiling(day(date) / 7))
    return(dat)
}

