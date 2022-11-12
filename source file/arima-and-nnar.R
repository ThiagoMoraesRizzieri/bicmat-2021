library(fpp3)
library(forecast)

# Re-index based on trading days
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)

google_2015 %>%
  autoplot(Close) +
  labs(title= "Google Stock - 2015", x = "Working days in the year", y = "$US")

tendencia <- google_2015 %>%
  model(stl = STL(Close))
components(tendencia)

components(tendencia) %>%
  as_tsibble() %>%
  autoplot(Close, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(title= "Trend of Google Stock - 2015", x = "Working days in the year", y = "$US")

# Time series decomposition
google_2015 %>%
  model(
    STL(Close ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot() +
  labs(
    y = "$US",
    x = "Working days in the year",
    title = "Google Stock - 2015"
  )

# ARIMA model forecast
google_2015 %>%
  model(ARIMA(Close)) %>%
  forecast(h =  19) %>%
  autoplot(google_2015) +
  labs(
    y = "$US",
    x = "Working days in the year",
    title = "Google Stock - 2015"
  )

# Graph and more information about ARIMA model residuals
google_2015 %>%
  model(ARIMA(Close)) %>%
  gg_tsresiduals()

# NNAR model forecast
google_2015 %>%
  model(NNETAR(Close)) %>%
  forecast(h = 19) %>%
  autoplot(google_2015) +
  labs(
    y = "$US",
    x = "Working days in the year",
    title = "Google Stock - 2015"
  )

# Graph and more information about NNAR model residuals
google_2015 %>%
  model(NNETAR(Close)) %>%
  gg_tsresiduals()

# Fit the models
google_fit <- google_2015 %>%
  model(
    NNAR = NNETAR(Close),
    ARIMA = ARIMA(Close)
  )

# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))

google_fc <- google_fit %>%
  forecast(new_data = google_jan_2016)

# Plot the forecasts
google_fc %>%
  autoplot(google_2015) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(
    y = "$US",
    x = "Working days in the year",
    title = "Google Stock",
    subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))

# Point forecast accuracy of models
accuracy(google_fc, google_jan_2016)

accuracy(google_fc, google_jan_2016, list(crps = CRPS))

accuracy(google_fc, google_jan_2016, list(qs=quantile_score), probs=0.025)

accuracy(google_fc, google_jan_2016, list(winkler = winkler_score), level = 95)

google_fc %>%
  accuracy(google_stock, list(crps = CRPS))


# More info about ARIMA model
google_fit_arima <- google_2015 %>%
  model(
    'ARIMA' = ARIMA(Close),
  )
report(google_fit_arima)

# More info about NNAR model
google_fit_nnar <- google_2015 %>%
  model(
    'NNAR' = NNETAR(Close),
  )
report(google_fit_nnar)

sim <- google_fit_nnar %>% generate(h = 19, times = 50, bootstrap = TRUE)

google_2015 %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = sim) +
  labs(title="Google daily closing stock price", y="$US" ) +
  guides(colour = "none")
