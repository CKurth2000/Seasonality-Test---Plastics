library(fpp2)
library(car)
library(seasonal)

autoplot(plastics)
ggAcf(plastics) # Not White-noise

seasonplot(plastics)
plastics <- (plastics)

plastics2 <- window(plastics, start=c(1), end=c(5.92))
h <- 36
fit.plastics <- tslm(plastics2 ~ trend + season)
summary(fit.plastics)

linearHypothesis(fit.plastics, c("trend = 0","season2 = 0"))

checkresiduals(fit.plastics)
autoplot((plastics2), series="Data") +
  autolayer(fitted(fit.plastics), series="Fitted") #Very good fit

fcast <- forecast(fit.plastics, h=36, lambda = 1)
autoplot(fcast) 
accuracy(fcast,plastics2)
