#' Select the optimal model
#'
#' @description double seasonal exponential smoothing method is implemented. If grid.search = TRUE, grid search is applied by searching parameters around the "first attempt" parameters. \code{search.length} and \code{length.out} are for grid search. The search is based on the minimum mean absolute percentage error with parallel computing. More details about parallel computing can be found in \code{doParallel}.
#'
#' @param train.y A numeric vector for training.
#' @param valid.y A numeric vector for testing. If \code{grid.search = NULL}, \code{valid.y = NULL}.
#' @param s1 Period of the shorter seasonal period.
#' @param s2 Period of the longer seasonal period.
#' @param h Number of periods for forecasting.
#' @param grid.search If TRUE, a grid search is applied.
#' @param search.length Grid search parameter. Only used if \code{grid.search = TRUE}. It is the proportion decreasing/increasing of the first attempt parameters. For example \code{search.length = c(-0.5, 0.5)}, if the first attempt parameter is 0.1, then the searching window is from \code{0.1 * (1 - 0.5)} to \code{0.1 * (1+ 0.5)}
#' @param length.out Grid search parameter. Only used if \code{grid.search = TRUE}. It is the desired length of search sequence.
#' @param CV If TRUE, k-fold cross validation applied
#' @param kfold The number of K-fold
#'
#'
#' @return If \code{grid.search = FALSE}, an object of class \code{forecast} is return. Otherwise, a list contains:
#' \itemize{
#' \item model. An object of class \code{forecast}. The model is built by combining training and testing;
#' \item cv. A data frame of searched parameters with respect to its distance measures (see \code{\link{measure_dist}})
#' }
#' @importFrom forecast dshw
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
#' select.ls = select_model(train.y, valid.y, grid.search = TRUE, length.out = 2)
select_model = function(train.y,
                        valid.y = NULL,
                        s1 = 7,
                        s2 = 7 * 52,
                        h = 2* max(s1, s2),
                        grid.search = FALSE,
                        search.length = c(-0.5, 0.5),
                        length.out = 5,
                        CV = FALSE,
                        kfold = NULL) {


    org.m = dshw(train.y, period1 = s1, period2 = s2, h = length(valid.y))

    alpha0 = org.m$model$alpha
    beta0  = org.m$model$beta
    gamma0 = org.m$model$gamma
    omega0 = org.m$model$omega
    phi0 = org.m$model$phi


    # don't want to do grid search
    if (grid.search) {
        no_cores = parallel::detectCores()
        cl = makeCluster(no_cores)
        registerDoParallel(cl)

        # model built on train.y is for reference

        # build grid
        alpha.vec = seq(alpha0 * (1 + search.length[1]), min(alpha0 * (1 + search.length[2]), 1), length.out = length.out)

        beta.vec = seq(beta0 * (1 + search.length[1]), min(beta0 * (1 + search.length[2]), 1), length.out = length.out)

        gamma.vec =  seq(gamma0 * (1 + search.length[1]), min(gamma0 * (1 + search.length[2]), 1), length.out = length.out)

        omega.vec = seq(omega0 * (1 + search.length[1]), min(omega0 * (1 + search.length[2]), 1), length.out = length.out)

        phi.vec = seq(phi0 * (1 + search.length[1]), min(phi0 * (1 + search.length[2]), 1), length.out = length.out)

        # GRID SEARCH BASED ON THE MINIMUM SSE WITH PARALLEL COMPUTING
        cv = foreach(alpha = alpha.vec, .combine = rbind, .packages=c("foreach", "forecast", "amplify")) %dopar% {
            foreach(beta = beta.vec, .combine = rbind) %dopar% {
                foreach(gamma = gamma.vec, .combine = rbind) %dopar% {
                    foreach(omega = omega.vec, .combine = rbind) %dopar% {
                        foreach(phi = phi.vec, .combine = rbind) %dopar% {
                            if (CV) {

                                # cross validation
                                test.dist = dstsCV(train.y, valid.y, s1, s2, kfold, par = list(alpha, beta, gamma, omega, phi))
                            } else {
                                # traditional validation
                                test.dist = dstsCV(train.y, valid.y, s1, s2, 1, par = list(alpha, beta, gamma, omega, phi))

                            }

                            data.frame(alpha = alpha,
                                       beta = beta,
                                       gamma = gamma,
                                       omega = omega,
                                       phi = phi,
                                       me = test.dist[1],
                                       rmse = test.dist[2],
                                       mpe = test.dist[3],
                                       mape = test.dist[4])
                        }
                    }
                }
            }
        }
        #' Double seasonal exponential time series cross validation

        colnames(cv) = c("alpha", "beta", "gamma", "omega", "phi", "me", "rmse", "mpe", "mape")
        # the one with least mape
        opt.para = cv[which.min(cv$mape),]

        model = dshw(c(train.y, valid.y),
                    period1 = s1,
                    period2 = s2,
                    h = h,
                    alpha= opt.para$alpha,
                    beta = opt.para$beta,
                    gamma = opt.para$gamma,
                    omega = opt.para$omega,
                    phi =opt.para$phi)

        list(model = model, cv = cv)
    } else {

        model = dshw(c(train.y, valid.y),
                     period1 = s1,
                     period2 = s2,
                     h = h,
                     alpha= alpha0,
                     beta = beta0,
                     gamma = gamma0,
                     omega = omega0,
                     phi =phi0)

        test.dist = measure_dist(forecast(org.m, h=length(valid.y))$mean, valid.y)

        cv = data.frame(alpha = alpha0,
                        beta = beta0,
                        gamma = gamma0,
                        omega = omega0,
                        phi = phi0,
                        me = test.dist[1],
                        rmse = test.dist[2],
                        mpe = test.dist[3],
                        mape = test.dist[4])
        return( list(model = model, cv = cv))
    }
}
