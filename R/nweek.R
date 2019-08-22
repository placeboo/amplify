#' Nth weekday  of the month
#'
#' @description The given date is the Nth weekday of the month
#' @param date A numeric vector represents dates. The format is year month day, for example, 20190801
#' @return The data frame with two columns, date (date formate y-m-d), weekday, and the Nth weekday of the month.
#' @examples
#' nweek(c(20190101: 20190131))
#' @importFrom lubridate ymd year month wday
#' @import dplyr
#' @export

nweek = function(date) {
    date = ymd(date)
    year = year(date)
    month = month(date)
    weekday = wday(date)

    dat = data.frame(date = date,
                     year = year,
                     month = month,
                     weekday = wday(date, label = TRUE))

    dat = dat %>%
        group_by(year, month, weekday) %>%
        mutate(Nth = rank(date))

    dat = dat %>%
        ungroup() %>%
        select(date, weekday, Nth)
    return(dat)
}

