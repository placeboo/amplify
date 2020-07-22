#' Find the the date which needs to be adjusted further
#'
#' @param data A data frame.
#' @param vars The column name of the data frame, date and "distance".
#' @param lambda A number. See \code{\link{find_outlier}}.
#' @param weekday.sep A logical value indicating whether find outliers separately by weekdays
#'
#' @return A list contains two data frame. One is date with fixed date, and the other one is the date with fixed weekday. In the fixed date data frame, which is called \code{fixD.dat}, there are three columns and they are the month of a year, the day of the month, and number of times the date has been treated as an outlier. In the fixed weekday data frame, which is called \code{fixWD.dat}, there are four columns and they are the month of a day, the nth week of the month, the weekday of the weekday, and the number of times such day has been treated as an outlier.
#' @export


find_adj_date = function(data,
                         vars = c("date", "y"),
                         lambda = 1.5,
                         weekday.sep = FALSE) {

    op = options(dplyr.summarise.inform = FALSE)
    on.exit(options(op))

    data$date = pull(data, vars[1])
    data$y = pull(data, vars[2])

    Nth = nweek(data$date)$Nth
    data = data %>%
        mutate(month = month(date),
               day = mday(date),
               Nth = Nth,
               weekday = wday(date))


    if (weekday.sep) { # model each weekday separately

        is_outlier = rep("no", nrow(data))

        for (i in 1:7) {
            idx = which(data$weekday == i)
            tmp.dat = data[idx, ]

            # find the outlier
            tmp.outlier.ls = find_outlier(x = tmp.dat$y, lambda = lambda)
            tmp.is_outlier = tmp.outlier.ls$is_outlier

            is_outlier[idx[tmp.is_outlier == "yes"]] = "yes"


        }

    } else {
        outlier.ls = find_outlier(x = data$y, lambda = lambda)
        is_outlier = outlier.ls$is_outlier
    }

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
