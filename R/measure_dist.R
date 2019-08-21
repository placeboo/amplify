#' Measure the distance between estimated time series and actual time series
#'
#' @param est A numeric vector of estimated time series
#' @param truth  A numeric vector of actual time series
#' @return A matrix with mean error (me), root mean square error (rmse), mean percentage error (pe), and mean absolute percentage error (mape)
#' @export


measure_dist = function(est, truth) {
    error = truth - est

    me = mean(error)
    rmse = sqrt(mean(error^2))
    mpe = mean(error / est)
    mape = mean(abs(error / est))

    dist.mat = t(matrix(c(me, rmse, mpe, mape)))
    colnames(dist.mat) = c("me", "rmse", "mpe", "mape")
    dist.mat
}
