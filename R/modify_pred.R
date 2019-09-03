#' Modify the prediction by auto-adjusting
#'
#' @param data A data framem includes a column called \code{date}.
#' @param vars A charater vector represents the names of date and the numeric vector of proportion error.
#' @param time.window A numeric vector shows the begining date and ending date, see \code{\link{build_date}}.
#' @param par A list of nuisance parameters. \code{lambda=1.5}, a number decribes the "distance" between the outliers and numeric variable \code{var}. \code{n}, the threshold number of times the fixed date or the fixed weekday is treated as outliers. If \code{n = NULL}, it takes the half number of years.
#' @holiday A data frame includes \code{date} and \code{name} columns. If users want to adjust more holidays, simply add the date and name of the holidays. The \code{date} should be in date format, and the \code{name} should be charater.
#' @return A list includes:
#' \itemize{
#' \item \code{h.mat} A data frame with two columns \code{date} and \code{h.hat}. \code{h.hat} is the deducating/increasing proportion of estimated time series.
#' \item \code{fix.date}, \code{fix.weekday}, \code{holiday}. Proprotion erorr for the past several years.
#' }

#' @export
modify_pred = function(data,
                        vars = c("date", "y"),
                        time.window = NULL,
                        par = list(lambda = 1.5,
                                   n = NULL),
                        holiday = NULL) {
    # organize input ----------------------------------
    data$date = pull(data, vars[1])
    data$y = pull(data, vars[2])

    data = data %>%
        mutate(year = year(date),
               month = month(date),
               weekday = wday(date),
               day = mday(date),
               is_weekend = weekday %in% c(1,7))

    data$Nth = nweek(data$date)$Nth

    if (is.null(par$n)) {
        par$n = floor(length(unique(year(data$date)))/2)
    }

    # generate futher date for prediction----------------------------------
    if (is.null(time.window)) {
        pred.date = build_date(max(data$date), max(data$date) + 7)
    } else {
        pred.date = build_date(time.window[1], time.window[2])
    }

    pred.dat = data.frame(date = pred.date)
    pred.dat = pred.dat %>%
        mutate(year = year(date),
               month = month(date),
               weekday = wday(date),
               day = mday(date),
               is_weekend = weekday %in% c(1,7))
    pred.dat$Nth = nweek(pred.dat$date)$Nth

    #------------ adjuste date ---------------------------#
    adjDate = find_adj_date(data,
                         vars = vars,
                         lambda = par$lambda)

    fixD.dat = adjDate$fixD.dat
    fixWD.dat = adjDate$fixWD.dat

    # select the fixed date
    tmp = data %>%
        group_by(month, day, is_weekend) %>%
        summarise(h.hat = mean(y)) %>%
        right_join(fixD.dat, by = c("month", "day")) %>%
        filter(n >= par$n)

    hD.hat = pred.dat %>%
        left_join(tmp, by = c("month", "day","is_weekend")) %>%
        mutate(h.hat = replace_na(h.hat, 0))

    fixD.hhat.mat = data %>%
        group_by(month, day, is_weekend) %>%
        right_join(fixD.dat, by = c("month", "day")) %>%
        ungroup() %>%
        select(date, is_weekend, y) %>%
        mutate(mday = format(date, "%m-%d")) # output

    # input holiday -----------------------------
    if (!is.null(holiday)) {
        # future holiday
        holiday.hhat.mat = holiday %>%
            inner_join(data, by = "date") %>%
            group_by(name, is_weekend) %>%
            select(date, name, is_weekend, y) %>%
            arrange(name) # output

        tmp = holiday %>%
            inner_join(data, by = "date") %>%
            group_by(name, is_weekend) %>%
            summarise(h.hat = mean(y))

        pred.holiday = holiday %>%
            right_join(pred.dat, by = "date")%>%
            inner_join(tmp, by = c("name", "is_weekend")) %>%
            select(date, name, h.hat)

        # combine holiday and fixed date effect

        pred.holiday = pred.holiday %>%
            right_join(pred.dat, by = "date") %>%
            mutate(h.hat = replace_na(h.hat, 0))

        tmp = hD.hat$h.hat

        tmp[which(pred.holiday$h.hat != 0)] = 0

        hD.hat$h.hat = tmp + pred.holiday$h.hat
    } else {
        holiday.hhat.mat = NULL
    }


    # select fixed weekday
    fixWD.hhat.mat = data %>%
        group_by(month, Nth, weekday) %>%
        right_join(fixWD.dat, by = c("month", "Nth", "weekday")) %>%
        ungroup() %>%
        select(year, month, Nth, weekday, y) #output

    tmp = fixWD.dat %>%
        left_join(data, by = c("month", "Nth", "weekday")) %>%
        filter(n >= par$n) %>%
        group_by(month, Nth, weekday) %>%
        summarise(h.hat = mean(y))


    hWD.hat = pred.dat %>%
        left_join(tmp, by = c("month", "Nth","weekday")) %>%
        mutate(h.hat = replace_na(h.hat, 0))

    # combine fixed date with fixed weekday
    tmp = hWD.hat$h.hat

    tmp[which(hD.hat$h.hat != 0)] = 0

    h.hat = tmp + hD.hat$h.hat

    h.hat[h.hat < -1] = 0

    h.mat = data.frame(date = pred.date, h.hat)

    list(h.mat = h.mat,fix.date = fixD.hhat.mat, fix.weekday = fixWD.hhat.mat, holiday = holiday.hhat.mat)
}
