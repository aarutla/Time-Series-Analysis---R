library(readxl)
library(fpp3)

mydata <- read_excel("Desktop/exchangerate.xlsx")

exchangetimeseries <- mydata %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

test <- exchangetimeseries %>%
  filter_index("2000 Jan" ~ "2022 Dec") 

train <- exchangetimeseries %>%
  filter_index("2000 Jan" ~ "2021 Dec") 

train %>% autoplot(Canadian)
train %>% autoplot(Pesos)

train %>%
  gg_season +
  labs(
    y = "Exchange Rate",
    title = "Canadian Exchange Rate relative to US Dollar")

train %>%
  model(STL(Canadian ~ season(window = 13) + trend(window = 21), robust = TRUE)) %>%
  components() %>%
  autoplot() +
  labs(title = "Canadian Exchange Rate relative to US Dollar")

train %>%
  gg_season +
  labs(
    y = "Exchange Rate",
    title = "Canadian Exchange Rate relative to US Dollar")

dcmp <- train %>% model(stl = STL(Canadian))
comp <- components(dcmp)
seasonal <- comp %>% 
  as_tsibble(key = Month) %>% 
  dplyr::select(Month, season_adjust) %>%
  autoplot(Month, season_adjust)

dcmp <- train %>% model(stl = STL(Canadian))
comp <- components(dcmp) 

train |>
  features(difference(Canadian), unitroot_kpss)

train |>
  features(difference(Pesos), unitroot_kpss)

train %>%
  autoplot(Canadian)
train %>% gg_tsdisplay(difference(Canadian), c("partial"))

train %>%
  autoplot(Canadian)
train %>% gg_tsdisplay((Canadian), c("partial"))

train %>%
  autoplot(Pesos)
train %>% gg_tsdisplay(difference(Pesos), c("partial"))

fit1 <- train |>
  model(
    aicc = VAR(vars(Canadian,Pesos)),
  )
report(fit1)

tidy(fit1)
accuracy(fit1)
glance(fit1)

fit2 <- train |>
  model(
    aicc = VAR(vars(Canadian, Pesos)),ic = c("bic")))
report(fit2)

tidy(fit2)
accuracy(fit2)
glance(fit2)

fit1 |>
  augment() |>
  ACF(.innov) |>
  autoplot()

fit2 |>
  augment() |>
  ACF(.innov) |>
  autoplot()


augment(fit1) |>
  features(.innov, ljung_box, lag = 10, dof = 3)

fit_var <- fit1 |>
  select(aicc) |>
  forecast(h = 12) 

fit1 |>
  select(aicc) |>
  forecast(h = 12) |>
  autoplot(exchangetimeseries)

fit3 <- train |>
  model(arima013= ARIMA(Canadian ~ pdq(0, 1, 3)),
        stepwise = ARIMA(Canadian),
        search = ARIMA(Canadian, stepwise=FALSE))

fit3 |> pivot_longer(everything(), names_to = "Model name",
                     values_to = "Orders")

glance(fit3) |> arrange(AICc)

tidy(fit3)
accuracy(fit3)
glance(fit3)

augment(fit3) |>
  filter(.model == 'search')|>
  features(.innov, ljung_box, lag = 10, dof = 5)

arma <- fit3 |>
  select(model=search) |>
  forecast(h = 12) 

fit3 |>
  select(model=search) |>
  forecast(h = 12) |>
  autoplot(exchangetimeseries)

fits <- train %>%
  model(    
    ses = ETS(Canadian ~ error("A") + trend("N") + season("N")),
    holt = ETS(Canadian ~ error("A") + trend("A") + season("N")),
    damped = ETS(Canadian ~ error("A") + trend("Ad") + season("N")))
report(fits)

tidy(fits)
accuracy(fits)
glance(fits)

fits1 <- train %>%
  model(
    additive = ETS(Canadian ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Canadian ~ error("M") + trend("A") + season("M")),
    auto=ETS(Canadian)
  )

tidy(fits1)
accuracy(fits1)
glance(fits1)

fits <- train %>%
  model(ANN = ETS(Canadian))
report(fits)

fits |>
  forecast(h = 12) |>
  autoplot(exchangetimeseries)

fits1 |>
  forecast(h = 12) |>
  autoplot(exchangetimeseries)

sef <- fits |>
  forecast(h = 12)

accuracy(fit1)
accuracy(fit3)
accuracy(fits)

accuracy(fit_var, test)
accuracy(arma, test)
accuracy(sef, test)