library("fpp3")

# We can do better than using moving averages to smooth out noisy data
algeria_economy <- global_economy %>%
  filter(Country == "Algeria")
algeria_economy %>%
  autoplot(Exports) +
  ylab("Exports (% of GDP)") + xlab("Year")

# We can use exponential smoothing
fit <- algeria_economy %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N"), opt_crit = "mse"))
fc <- fit %>%
  forecast(h = 5)
fc %>%
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(fit)) +
  ylab("Exports (% of GDP)") + xlab("Year")
tidy(fit)
#not a very good forecast, such a wide level of fit, red line is 1 step late

# We can exponentially smooth the level and the trend
# We can also dampen the trend
aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population / 1e6)

aus_economy %>%
  autoplot(Pop) +
  ylab("Austrailian Economy") + xlab("Year")

aus_economy %>%
  model(
    `Holt's method` = ETS(Pop ~ error("A") + trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") + trend("Ad") + season("N"))
  ) %>%
  forecast(h = 15) %>%
  autoplot(aus_economy, level = NULL) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Population of Australia (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

# What approach would work for this data?
www_usage <- as_tsibble(WWWusage)
www_usage %>% autoplot(value) +
  xlab("Minute") + ylab("Number of users")

www_usage %>%
  stretch_tsibble(.init = 10) %>%
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") + season("N"))
  ) %>%
  forecast(h = 1) %>%
  accuracy(www_usage)
#looking for low MAPE
#Dammped looks best

# Damped Holt's is the best method under both metrics
fit <- www_usage %>%
  model(Damped = ETS(value ~ error("A") + trend("Ad") + season("N")))
tidy(fit)
fit %>%
  forecast(h = 10) %>%
  autoplot(www_usage) +
  xlab("Minute") + ylab("Number of users")

# We can add additive or multiplicative seasonality
aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips))
fit <- aus_holidays %>%
  model(
    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M"))
  )
fc <- fit %>% forecast(h = "3 years")

fc %>%
  autoplot(aus_holidays, level = NULL) + xlab("Year") +
  ylab("Overnight trips (millions)") +
  scale_color_brewer(type = "qual", palette = "Dark2")

# This also works for daily data
sth_cross_ped <- pedestrian %>%
  filter(Sensor == "Southern Cross Station", yearmonth(Date) == yearmonth("2016 July")) %>%
  index_by(Date) %>%
  summarise(Count = sum(Count))
sth_cross_ped %>%
  model(hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))) %>%
  forecast(h = "2 weeks") %>%
  autoplot(sth_cross_ped)


##### Easy method ######
# The ETS function can do all these things
aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips))
fit <- aus_holidays %>%
  model(ETS(Trips))
report(fit)
#gamma is seasonality
#alpha is noise
#Beta is trend

# We can see the components of an ETS model like we did in decomposition
components(fit) %>%
  autoplot() +
  ggtitle("ETS(M,N,M) components")

fit %>%
  forecast(h = 8) %>%
  autoplot(aus_holidays) +
  ylab("Domestic holiday visitors in Australia (thousands)")

