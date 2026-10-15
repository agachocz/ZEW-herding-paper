

# VAR MODEL USED FOR VARIABLE SELECTION

library(vars)
library(tseries)

full_data <- full_join(zew_stationary, macro_stationary, by = "Date") %>%
  left_join(rec_data, by = "Date") %>%
  rename(ZEW_Econ_Growth = Econ_Growth, ZEW_Inflation = Inflation, ZEW_Current = Current,
         ZEW_STOXX = STOXX50.x, ZEW_RATE = Interest_Rate, Inflation = infl_EA,
         Rate = rate_EA, Unemployment = un_EA, GDP = gdp, STOXX50 = STOXX50.y) #%>%
  #filter(Date < as.Date("2020-01-01")) # before covid
  
full_data <- full_data[complete.cases(full_data),]

adf.test(full_data[,11]) # stationary

VARselect(full_data[,-c(1,12)], lag.max = 12, type="const") # no definitive answer, 1 or 2 lags, I choose simpler model
model <- VAR(full_data[,-c(1,12)], p = 1, type = "const")
summary(model)

causality(model, cause = "ZEW_Current")


# diagnostics
roots(model) # stable
serial.test(model, type = "BG") # autocorrelation
arch.test(model) # heteroscedascity
normality.test(model) # not normal

# Newey-West method
library(sandwich)

hc <- NeweyWest(model, lag = 1, diagnostics = F, sandwich = T)
cftest <- coeftest(model, vcov = hc)

# rounded for the table in the paper
round(cftest, 3)
write.csv(cftest, "full VAR results.csv")

