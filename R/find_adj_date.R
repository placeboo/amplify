#' find the the date which needs to be adjusted further
#'
#' @param data A data frame.
#' @param vars The column name of the data frame, date and "distance".
#' @param lambda A number. See \code{\link{find_outlier}}.
#' @return A list contains two data frame. One is date with fixed date, and the other one is the date with fixed weekday. In the fixed date data frame, which is called \code{fixD.dat}, there are three columns and they are the month of a year, the day of the month, and number of times the date has been treated as an outlier. In the fixed weekday data frame, which is called \code{fixWD.dat}, there are four columns and they are the month of a day, the nth week of the month, the weekday of the weekday, and the number of times such day has been treated as an outlier.


find_adj_date = function(data,
                         vars = c("date", "y"),
                         lambda = 1.5) {
    data$date = pull(data, vars[1])
    data$y = pull(data, vars[2])

    Nth = nweek(data$date)$Nth
    data = data %>%
        mutate(month = month(date),
               day = mday(date),
               Nth = Nth,
               weekday = wday(date))

    outlier.ls = find_outlier(x = data$y, lambda = lambda)

    is_outlier = outlier.ls$is_outlier
    data$is_outlier = is_outlier
    # two different types
    ## case 1: fixed date
    ## case 2: fixed weekdays

    dat1 = data %>%
        filter(is_outlier == "yes") %>%
        group_by(month, day) %>%
        summarise(n = n())

    dat2 = data %>%
        filter(is_outlier == "yes") %>%
        group_by(month, Nth, weekday) %>%
        summarise(n = n())

    return(list("fixD.dat" = dat1, "fixWD.dat" = dat2))
}
