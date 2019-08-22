#' Training and testing set split
#'
#' @param data A data frame to be splitted.
#' @param var The column name for dates.
#' @param trian.window A nummric vetor of training set starting time, and ending time
#' @param test.window A nummric vetor of testing set starting time, and ending time
#' @return A list of training set and testing set
#' @examples
#' data(tickets)
#' train_test_split(tickets, var = "date", train.window = c(20140701, 20180630), test.window = c(20180701, 20190630))
#' @importFrom lubridate is.Date
#' @export

train_test_split = function(data,
                            var = "date",
                            train.window,
                            test.window) {
    data$date = as.Date(pull(data, var))
    if (!is.Date(data$date)) {
        stop("Date must be date format.")
    }

    train.dat = data %>%
        filter(date >=min(ymd(train.window)),
               date <= max(ymd(train.window)))

    test.dat = data %>%
        filter(date >=min(ymd(test.window)),
               date <= max(ymd(test.window)))


    return(list(train.dat = train.dat,
                test.dat = test.dat))
}
