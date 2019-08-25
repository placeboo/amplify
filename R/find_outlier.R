#' Detect outliers
#'
#' @description Outliers are detected if it is larger than Q3 + lambda * (Q3-Q1) or smaller than Q1 - lambda * (Q3-Q1), where Q3 and Q1 are the third and first quantile of \code{x}.
#' @param x A numeric vector.
#' @param lambda=1.5, a number decribes the "distance" from the group \code{x}. The larger the lambda is, the number of outliers captured is less.
#' @return A list contains a character vector names \code{is_outlier} that indicates whether it is outier ("yes", "no"), and a numeric vector named "bounds" that stores the upper cutting line and lower cutting line.
#' @examples
#' x = rnorm(100, 0, 1)
#' idx = c(1, 5, 10, 21, 30)
#' x[idx] = x[idx] + 5
#' find_outlier(x, lambda = 1.5)
#' @export


find_outlier = function (x,
                         lambda = 1.5) {
    q3 =  quantile(x, 0.75)
    q1 = quantile(x, 0.25)
    iqr = q3 - q1

    lower_cut = q1 - lambda * iqr
    upper_cut =  q3 + lambda * iqr

    bounds = c(lower_cut, upper_cut)
    names(bounds) = c("lower", "upper")
    outlier = ifelse(x > upper_cut | x < lower_cut, "yes", "no")

    list("is_outlier" = outlier, "bounds" = bounds)
}
