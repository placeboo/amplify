#' Select the optimal model
#'
#' @description double seasonal harmonic regression is implemented. If grid.search = TRUE, grid search is applied by searching the number of Fourier terms for yearly seasonality. \code{search.length} and \code{length.out} are for grid search. The search is based on the minimum mean absolute percentage error with parallel computing. More details about parallel computing can be found in \code{doParallel}.
#'
#' @param train.y A numeric vector for training.
#' @param valid.y A numeric vector for validating If \code{grid.search = FALSE}, \code{valid.y = NULL}.
#' @param s1 Period of the shorter seasonal period.
#' @param s2 Period of the longer seasonal period.
#' @param grid.search If TRUE, a grid search is applied.
#' @param search.length Grid search parameter. Only used if \code{grid.search = TRUE}. It is the range of the number of the Fourier term of yearly seasonality.
#' @param length.out Grid search parameter. Only used if \code{grid.search = TRUE}. It is the desired length of search sequence.
#'
#' @return a list contains:
#' \itemize{
#' \item model. An object of class \code{forecast}. The model is built by combining training and testing;
#' \item K. A vector display the number of Fourier term for weekly seaonality and yearly seasonality.
#' \item cv. A data frame of searched parameters with respect to its distance measures (see \code{\link{measure_dist}})
#' }
#' @importFrom forecast fourier
#' @importFrom forecast forecast
#' @import doParallel
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel
#' @import foreach
#' @export
#'
#' @examples
#' data(tickets)
#' data.ls = train_test_split(tickets, var = "date", train.window = c(20140701, 20180630), test.window = c(20180701, 20190630))
#' train.y = data.ls$train.dat[,2]
#' valid.y = data.ls$test.dat[,2]
#' select.ls = select_model_hmnc(train.y, valid.y, grid.search = TRUE, length.out = 5)
#'
select_model_hmnc = function(train.y,
                             valid.y = NULL,
                             s1 = 7,
                             s2 = 365.25,
                             grid.search = FALSE,
                             search.length = c(20, 40),
                             length.out = 5) {

    k1 = floor(s1/2) # the number of Fourier term for week
    k2 = floor(s2/12) # the number of Fourier term for year

    train_ts_weekly = ts(train.y, frequency = s1)
    train_ts_yearly = ts(train.y, frequency = s2)

    valid_ts_weekly = ts(valid.y, frequency = s1)
    valid_ts_yearly = ts(valid.y, frequency = s2)

    train_xreg1 = fourier(train_ts_weekly, K = 3)

    valid_xreg1 = fourier(valid_ts_yearly, K = 3)

    cv = NULL

    if(grid.search) { # grid search based on the minimum mape with parallel computing

        if((search.length[2] - search.length[1]) %% length.out != 0) {
            stop("The number of Fourier term should be integer!")
        }

        yK.vec = seq(search.length[1], search.length[2], length.out)

        no_cores = detectCores()
        cl = makeCluster(no_cores)
        registerDoParallel(cl)

        cv = foreach(k = yK.vec, .combine = rbind, .packages = c("forecast", "amplify")) %dopar% {

            train_xreg2 = fourier(train_ts_yearly, K=k)
            valid_xreg2 = fourier(valid_ts_yearly, K=k)
            xtrain = cbind(train_xreg1, train_xreg2)

            fitma1 = auto.arima(train.y, xreg = xtrain, seasonal = FALSE, lambda = 0)

            # validate
            xvalid = cbind(valid_xreg1, valid_xreg2)
            valid.ls = forecast(fitma1, xreg = xvalid)

            valid.dist = measure_dist(valid.y, valid.ls$mean)

            data.frame(K = k,
                       me = valid.dist[1],
                       rmse = valid.dist[2],
                       mpe = valid.dist[3],
                       mape = valid.dist[4])
        }
        k2 = cv[which.min(cv$mape),"K"]


    }

    train_xreg2 = fourier(train_ts_yearly, K=k2)
    valid_xreg2 = fourier(valid_ts_yearly, K=k2)

    xtrain = cbind(train_xreg1, train_xreg2)
    xvalid = cbind(valid_xreg1, valid_xreg2)

    xreg = rbind(xtrain, xvalid)

    fitma = auto.arima(c(train.y, valid.y), xreg = xreg, seasonal = FALSE, lambda = 0)

    list(model = fitma, K = c(K1 = k1, K2 = k2), cv = cv)

}
