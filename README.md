I have leveraged more features to the Amplify Package. Now we release
the updated version 0.2.0. The latest one update functions
`modify_pred`, `smooth_ts`. `find_adj_date`, and `select_model`. For the
first three functions, I have added a mode called `weekday.sep = FALSE`.
If the `weekday.sep = TRUE`, one of the seasonal pattern is weekly and
time series is implemented respectively according to their weekdays. In
`select_model`, I have added cross validation option. See more details
below.

Our package is for double seasonal daily time seriers. Data features
include outliers, variation increasing/decreasing, and double
seasonality. We build multiplicative modeling for time series.

In this document, we explain how to use our R package.

To showing the steps, we forecast the some airline supporting tickets
for FY20 as an example.

Install R package
-----------------

Install the package from github

    devtools::install_github("placeboo/amplify", force = TRUE)

We start from loading our package `Amplify`.

    library(amplify)

We have support ticket from 2014-07-01 to 2018-06-10. A glimpse of the
data:

    ##         date ticket
    ## 1 2014-07-01   4540
    ## 2 2014-07-02   4547
    ## 3 2014-07-03   3888
    ## 4 2014-07-04   2172
    ## 5 2014-07-05    435
    ## 6 2014-07-06    486
    ## 7 2014-07-07   4411
    ## 8 2014-07-08   4475
    ## 9 2014-07-09   4183

In the package, we have a holiday data set. For forecasting, you can add
your own holiday information. It should include the date and the name of
the holidays. Have a look:

    data("holiday")
    head(holiday)

    ##         date                    name   is_holiday
    ## 1 2014-01-01                 NewYear          Yes
    ## 2 2014-01-02           AfterNewYear1 AfterHoliday
    ## 3 2014-01-03           AfterNewYear2 AfterHoliday
    ## 4 2014-01-20 Martin Luther King Day           Yes
    ## 5 2014-02-11 National Foundation Day          Yes
    ## 6 2014-03-01       Carnival Saturday          Yes

Smoothing
---------

The whole data set is our training set. We aim to predict the daily
tickets from 2019-07-01 to 2020-06-30. First we detect the outiers and
impute them with reasonable value.

    smooth.ls = smooth_ts(data = tickets,
                          vars = c("date", "ticket"),
                          par = list(lambda = 1.5, s = 7),
                          weekday.sep = TRUE)

    train.dat = cbind(tickets, smooth.ls$ts.dat)
    head(train.dat) 

    ##         date ticket is_outlier       ts
    ## 1 2014-07-01   4540         no 4540.000
    ## 2 2014-07-02   4547         no 4547.000
    ## 3 2014-07-03   3888         no 3888.000
    ## 4 2014-07-04   2172        yes 3437.725
    ## 5 2014-07-05    435         no  435.000
    ## 6 2014-07-06    486         no  486.000

Validation
----------

We split the data set into subtraining set and validation set to find
parameters. There are 5 parameters {*α*, *β*, *γ*, *ω*, *ϕ*} in the
model. We use grid search to find a set of optimal parameters. The
searching process has been optimized by parallel computing. We also
implement cross validation.

    train.start = 20140701
    train.end = 20190630

    subtrain.end = 20190430
    valid.start = 20190501

    valid.ls = train_test_split(train.dat,
                                train.window = c(train.start, subtrain.end),
                                test.window = c(valid.start, train.end))

    subtrain.dat = valid.ls$train.dat
    valid.dat = valid.ls$test.dat

    tic("grid search")
    select.ls = select_model(train.y = subtrain.dat$ts,
                             valid.y = valid.dat$ts,
                             grid.search = TRUE,
                             search.length = c(-0.5, 0.5), 
                             length.out = 5,
                             CV = TRUE,
                             kfold = 4)
    toc()

    ## grid search: 196.18 sec elapsed

The list `select.ls` contains the model which has the smallest MAPE for
validation set, and the validation results.

    cv = select.ls$cv
    head(cv) 

    ##        alpha        beta      gamma     omega       phi        me     rmse
    ## 1 0.06012052 0.001109511 0.07088587 0.1928071 0.1079121 -135.9885 404.3380
    ## 2 0.06012052 0.001109511 0.07088587 0.1928071 0.1618682 -134.5796 404.8921
    ## 3 0.06012052 0.001109511 0.07088587 0.1928071 0.2158243 -132.9768 405.4458
    ## 4 0.06012052 0.001109511 0.07088587 0.1928071 0.2697803 -131.1372 406.0008
    ## 5 0.06012052 0.001109511 0.07088587 0.1928071 0.3237364 -129.0040 406.5593
    ## 6 0.06012052 0.001109511 0.07088587 0.2892107 0.1079121 -140.2428 378.7406
    ##           mpe       mape
    ## 1 -0.01407710 0.04389997
    ## 2 -0.01403177 0.04408120
    ## 3 -0.01392302 0.04427219
    ## 4 -0.01372369 0.04448696
    ## 5 -0.01339904 0.04474509
    ## 6 -0.01190884 0.04081531

We pick the parameters which yield the lowest MAPE.

    cv[which.min(cv$mape), ]

    ##          alpha        beta     gamma     omega       phi        me     rmse
    ## 1966 0.1503013 0.001109511 0.1772147 0.4820178 0.1079121 -97.86261 356.5105
    ##               mpe       mape
    ## 1966 -0.006091832 0.03736505

Prediction
----------

We first build a future data frame

    pred.dat = build_date(20190701, 20200630)
    head(pred.dat) 

    ##         date
    ## 1 2019-07-01
    ## 2 2019-07-02
    ## 3 2019-07-03
    ## 4 2019-07-04
    ## 5 2019-07-05
    ## 6 2019-07-06

Within the data frame, we fill the predicted tickets.

    model = select.ls$model

    pred.ls = forecast(model, h = nrow(pred.dat))
    pred = as.numeric(pred.ls$mean)
    pred.dat = pred.dat %>%
      mutate(pred = pred)

    head(pred.dat) 

    ##         date     pred
    ## 1 2019-07-01 8757.255
    ## 2 2019-07-02 9495.442
    ## 3 2019-07-03 9709.499
    ## 4 2019-07-04 9317.271
    ## 5 2019-07-05 7726.349
    ## 6 2019-07-06 1170.135

Holiday Modification
--------------------

We define the distance between the actual and predictiona and then
adjust the holidays.

    tmp = cbind(train.dat, pred = as.numeric(fitted(model)))
    tmp = tmp %>%
      mutate(resid = (ticket - pred) / pred)

In order to capture floating holidays such as Easter, we add more
information about Holidays.

    modify.ls = modify_pred(tmp,
                            vars = c("date", "resid"),
                            time.window = c(20190701, 20200630),
                            par = list(lambda = 1.5, n = 3), 
                            holiday = holiday)

    h.hat =  modify.ls$h.mat$h.hat
    h.hat[h.hat > 0] = 0
    pred.dat = pred.dat %>%
      mutate(h.hat = h.hat,
             pred_modify = pred * (1 + h.hat))

    pred.dat %>%
      ggplot(aes(x = date, y = pred_modify)) +
      geom_point(size = 2, alpha = 0.6) + 
      geom_line() +
      scale_x_date(name = "Date",
                    date_breaks = "1 month") + 
      ylab("Tickets") + 
      theme(axis.text.x = element_text(angle = 30, hjust = 1))

![Prediction of FY20](README_files/figure-markdown_strict/plot-1.png)
