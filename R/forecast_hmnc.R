#' Forecasting via Harmonic Regression Model
#'
#' @description Forecasting based on the double seasonal harmonic regression
#'
#' @param model An object of class \code{forecast}, the model after applying the double seasonal harmonic regression
#' @param par  A list contains:
#' \itemize{
#' \item season. A vector displays the seasonality of the times series. The default 7 and 365.25 are for the weekly and yearly seasonality respectively
#' \item K. A vector displays the number of Fourier term for weekly seasonality and yearly seasonality.
#' }
#' @param h A numeric number shows the number of periods ahead to forecast (optional)



forecast_hmnc = function(model,
                         par = list(season = c(7, 365.25),
                                    K = c(3, 30)),
                         h = 30) {

    ts_weekly = ts(model$x, frequency = par$season[1])
    ts_yearly = ts(model$x, frequency = par$season[2])

    xreg1 = fourier(ts_weekly, K = par$K[1], h = h)
    xreg2 = fourier(ts_yearly, K = par$K[2], h = h)

    x = cbind(xreg1, xreg2)

    forecast(model, xreg = x)
}
