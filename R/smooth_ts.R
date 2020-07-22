#' Smooth time series
#'
#' @description Smooth the time series by impute the outlers.
#' @param data A data frame includes dates and time series columns
#' @param vars A character vector with variable names with respect to date and time series
#' @param par A list of parameters. \code{lambda = 1.5}, the "distance" describes the outlier and the main population. \code{s = 7}, the most obvious seasonality of the time series is 7 (days).
#' @param week.sep A logical value indicating whether model each weekday time series separately.
#' @return A list with:
#' \itemize{
#' \item ts.dat. A data frame with column name \code{is_outlier} and \code{ts}. \code{is_outlier} is a character vector that indicates whether the observation is outier ("yes", "no"). \code{ts} is a numeric vector that stores the time series after smoothing.
#' \item model. An object of class "ets", see \code{\link[forecast]{ets}}.
#' }
#'
#' @importFrom forecast ets
#' @importFrom dplyr pull
#' @details Use \code{find_outlier} to find the outliers and use one seasonal exponential smoothing to imput outliers
#' @seealso \code{\link{find_outlier}}
#' @examples
#' data(tickets)
#' smooth.dat = smooth_ts(tickets, vars = c("date", "ticket"))
#' cbind(tickets, smooth.dat)
#' @export

smooth_ts = function(data,
                  vars = c("date", "ts"),
                  par = list(lambda = 1.5,
                             s = 7),
                  week.sep = FALSE) {

    data$date = as.Date(pull(data, vars[1]))
    data$ts = pull(data, vars[2])

    if (week.sep) { # model each weekday separately

        data$weekday = wday(data$date)
        is_outlier = rep("no", nrow(data))
        ts_new = data$ts
        ets.m = vector('list', par$s)


        for (i in 1:par$s) {
            idx = which(data$weekday == i)
            tmp.dat = data[idx, ]
            tmp.ts = ts(tmp.dat$ts)

            # find the outlier
            tmp.ets.m = ets(tmp.ts)
            tmp.outlier.ls = find_outlier(x = resid(tmp.ets.m), lambda = par$lambda)
            tmp.is_outlier = tmp.outlier.ls$is_outlier

            ts_new[idx[tmp.is_outlier == "yes"]] = fitted(tmp.ets.m)[tmp.is_outlier == "yes"]
            is_outlier[idx[tmp.is_outlier == "yes"]] = "yes"

            ets.m[[i]] = tmp.ets.m
        }
        return(list(ts.dat = data.frame(is_outlier = is_outlier, ts = ts_new),
                    model = ets.m))

    }

    ts = ts(data$ts, frequency = par$s)

    #-------------- ETS finds outlier and imputation-----------------------------------#
    ets.m = ets(ts)

    outlier.ls = find_outlier(x = resid(ets.m), lambda = par$lambda)
    is_outlier = outlier.ls$is_outlier

    ts_new = data$ts
    ts_new[is_outlier == "yes"] = fitted(ets.m)[is_outlier == "yes"]

    list(ts.dat = data.frame(is_outlier = is_outlier, ts = ts_new),
         model = ets.m)
}
