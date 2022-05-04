credit <- read.csv('credit (1).csv')

credit$month <- 492:1
credit$month <- yearmonth(credit$month)

credit_ts <- tsibble(credit, index = month)

autoplot(credit_ts)
gg_season(credit_ts)


#Arima
fit <- credit_ts %>%
  model(arima1 = ARIMA(ï..credit_in_millions ~ pdq(2,1,0)),
        arima2 = ARIMA(ï..credit_in_millions ~pdq(0,1,3)),
        arima3 = ARIMA(ï..credit_in_millions ~pdq(2,0,2)),
        arima4 = ARIMA(ï..credit_in_millions ~pdq(1,2,1)),
        arimastepwise = ARIMA(ï..credit_in_millions),
        searching = ARIMA(ï..credit_in_millions, stepwise = FALSE))

glance(fit) %>% arrange(AICc)

fit <- credit_ts %>% model(ARIMA())
report(fit)

fit %>% forecast(h=12) %>% autoplot(credit_ts)
              

#Tslm
credit_ts %>%
  ggplot(aes(x = month, y = ï..credit_in_millions)) +
  labs(y = "Credit in millions",
       x = "Month") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)  

credit_ts %>%
  model(TSLM(ï..credit_in_millions ~ month)) %>%
  report()
#Naive
train <- credit_ts %>%
  filter_index("1970 Feb" ~ "2011 Jan")
# Fit the models
credit_fit <- TrainingCredit %>%
  model(
    Mean = MEAN(credit_in_millions),
    `Naïve` = NAIVE(credit_in_millions),
    `Seasonal naïve` = SNAIVE(credit_in_millions)
  )
# Generate forecasts
credit_fc <- credit_fit %>% forecast(h = 12)
# Plot forecasts against actual values

credit_fc %>%
  autoplot(TrainingCredit, level = NULL)



#Training
credit_ts <- credit_ts %>%filter(year(month) >= '1970 Feb')
TrainingCredit <- credit_ts %>% filter(year(month) <= '2004 Jan')



#ETS
fit <- credit_ts %>%
  model(ETS(credit_in_millions))
report(fit)

library(fpp3)
credits$month <- 492:1
credits$month<- yearmonth(credits$month, year = 3000L, month= 1L)
credit_ts <- as_tsibble(credits)
gg_season(credit_ts)

credit_ts %>%  
  model(
    classical_decomposition(�..credit_in_millions, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of credits")


