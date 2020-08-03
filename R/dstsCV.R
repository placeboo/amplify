#' Double seasonal exponential time series cross validation
#'
#' @description The cross-validation procedure based on a rolling forecasting.
#'
#' @param train.y A numeric vector for training.
#' @param valid.y A numeric vector for testing.
#' @param s1 Period of the shorter seasonal period.
#' @param s2 Period of the longer seasonal period.
#' @param kfold The number of subsets by spliting \code{valid.y}
#' @param par The list of parameters for Double seasonal exponential time series model
#'
#' @importFrom forecast dshw
#' @importFrom forecast forecast
#' @import doParallel
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel
#' @import foreach
#'
#'
#'


dstsCV = function(train.y,
                  valid.y,
                  s1 = 7,
                  s2 = 7 * 52,
                  kfold = 4,
                  par = list(alpha, beta, gamma, omega, phi)
) {

    if (kfold == 1) {
        org.m = dshw(train.y, period1 = s1, period2 = s2,
                     alpha= par[[1]],
                     beta = par[[2]],
                     gamma = par[[3]],
                     omega = par[[4]],
                     phi = par[[5]])
        org.vec  = measure_dist(forecast(org.m, h=length(valid.y))$mean, valid.y)
        return(org.vec)
    }

    # no_cores = detectCores()
    # c = makeCluster(no_cores)
    # registerDoParallel(c)


    valid_length = length(valid.y)
    cv_length = floor(valid_length / kfold)

    cv_idx = c(c(1:(kfold-1)) * cv_length, valid_length) # validation ending index
    i_idx = 1 : (kfold - 1)

    org.m = dshw(train.y, period1 = s1, period2 = s2,
                 alpha= par[[1]],
                 beta = par[[2]],
                 gamma = par[[3]],
                 omega = par[[4]],
                 phi = par[[5]])


    valid1.ts = valid.y[1 : cv_idx[1]]

    org.vec  = measure_dist(forecast(org.m, h=length(valid1.ts))$mean, valid1.ts)


    # cv = foreach(i = i_idx, .combine = rbind, .packages = c("foreach", "forecast", "amplify")) %dopar% {
    #
    #     tmp =  valid.y[1 : cv_idx[i]]
    #     cv_test = valid.y[(cv_idx[i] + 1) : cv_idx[i + 1]]
    #
    #     cv_train = c(train.y, tmp)
    #
    #     model = dshw(cv_train,
    #                  period1 = s1,
    #                  period2 = s2,
    #                  alpha= par[[1]],
    #                  beta = par[[2]],
    #                  gamma = par[[3]],
    #                  omega = par[[4]],
    #                  phi = par[[5]])
    #
    #     pred.ls = forecast(model, h = length(cv_test))
    #     cv_test.dist = measure_dist(pred.ls$mean, cv_test)
    #
    # }

    cv_dist.vec = c()
    for (i in i_idx) {
        tmp =  valid.y[1 : cv_idx[i]]
        cv_test = valid.y[(cv_idx[i] + 1) : cv_idx[i + 1]]

        cv_train = c(train.y, tmp)

        model = dshw(cv_train,
                     period1 = s1,
                     period2 = s2,
                     alpha= par[[1]],
                     beta = par[[2]],
                     gamma = par[[3]],
                     omega = par[[4]],
                     phi = par[[5]])

        pred.ls = forecast(model, h = length(cv_test))
        cv_test.dist = measure_dist(pred.ls$mean, cv_test)
        cv_dist.vec = c(cv_dist.vec, cv_test.dist)
    }

    cv.mat = matrix(c(org.vec, cv_dist.vec), nrow = kfold, byrow = TRUE)
    return(apply(cv.mat, 2, mean))
}

