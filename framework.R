rm(list = ls())
library(tidyverse)
library(lubridate)
library(amplify)
library(forecast)

data(tickets)
data("holiday")
smooth.dat = smooth_ts(tickets, vars = c("date", "ticket"))
data = cbind(tickets, smooth.dat)

data.ls = train_test_split(data, var = "date", train.window = c(20140701, 20180630), test.window = c(20180701, 20190630))

train.dat = data.ls$train.dat
test.dat = data.ls$test.dat
# subtraining and validation
train.ls = train_test_split(train.dat, var = "date", train.window = c(20140701, 20180430), test.window = c(20180501, 20180630))

subtrain.dat = train.ls$train.dat
validation.dat = train.ls$test.dat


select.ls = select_model(subtrain.dat$ts, validation.dat$ts, gril.search = TRUE, length.out = 10)


pred.dat = cbind(validation.dat, fitted = as.numeric(forecast::forecast(select.ls[[1]], h = nrow(validation.dat))$mean))

measure_dist(pred.dat$ts, pred.dat$fitted)




test.dat %>%
    gather(key = "model", value = "ts", ts, fitted) %>%
    ggplot(aes(x = date, y = ts, color = model)) +
    geom_line() +
    geom_point()

# train on full train
test.m = dshw(train.dat$ts,
              alpha=select.ls[[1]]$model$alpha,
              beta = select.ls[[1]]$model$beta,
              gamma = select.ls[[1]]$model$gamma,
              omega = select.ls[[1]]$model$omega,
              phi = select.ls[[1]]$model$phi,
              period1 = 7,
              period2 = 7 * 52)

test.dat$fitted = forecast(test.m, h = 365)$mean

# holiday modification
train.dat$fitted = test.m$fitted
train.dat$h.hat = (train.dat$ticket - train.dat$fitted) /  train.dat$fitted

adjD.ls = find_adj_date(train.dat,
                        vars = c("date", "h.hat"),
                        lambda = 1.2)

hhat.ls = modify_pred(train.dat,
            vars = c("date", "h.hat"),
            time.window = c(20180701, 20190630))

h.hat = hhat.ls$h.mat
tmp = h.hat$h.hat
tmp[tmp > 0] = 0
test.dat$adj = test.dat$fitted * (1 +tmp)


measure_dist(test.dat$adj, test.dat$ticket)

test.dat %>%
    gather(key = "model", value = "ts", ticket, adj) %>%
    ggplot(aes(x = date, y = ts, color = model)) +
    geom_line() +
    geom_point()

# without validation
# no validation
select.ls = select_model(train.dat$ts, gril.search = FALSE)
test.dat = cbind(test.dat, fitted = as.numeric(forecast::forecast(select.ls, h = nrow(test.dat))$mean))

train.dat$fitted = select.ls$fitted
train.dat$h.hat = (train.dat$ticket - train.dat$fitted) /  train.dat$fitted


hhat.ls = modify_pred(train.dat,
                      vars = c("date", "h.hat"),
                      time.window = c(20180701, 20190630),
                      par = list(lambda = 1.5, n = 3))
h.hat = hhat.ls$h.mat

tmp = h.hat$h.hat
tmp[tmp > 0] = 0
test.dat$adj = test.dat$fitted * (1 +tmp)
measure_dist(test.dat$fitted, test.dat$ticket)
measure_dist(test.dat$adj, test.dat$ticket)

# fit model to h.hat
h.m = auto.arima(ts(train.dat$h.hat, frequency = 7))
checkresiduals(h.m)

h.pred = forecast(h.m, h = 365)

train.dat$h.fitted = fitted(h.m)

fix.date = hhat.ls$fix.date

train.dat = train.dat%>%
    left_join(fix.date, by = "date")%>%
    mutate(weekday = wday(date, label = TRUE))

train.dat$mday = replace_na(train.dat$mday, "00")



h.m = lm(data = train.dat, h.hat ~ mday + weekday)
summary(h.m)


tmp = test.dat %>%
    mutate(mday = format(date, "%m-%d"),
           weekday = wday(date, label = TRUE))
tmp$mday[!tmp$mday %in% fix.date.hat] = "00"

h.pred = predict(h.m, newdata = tmp)


measure_dist((test.dat$fitted * (1 + h.pred)), test.dat$ticket)
test.dat$adj2 = test.dat$fitted * (1 + h.pred)
