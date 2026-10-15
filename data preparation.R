library(tidyverse)
library(vars)
library(tseries)
library(lmtest)

# reading the ZEW indices data
zew <- read.csv("data/ZEW_eurozone.csv", sep = ";", dec = ",", encoding = "UTF-8")
colnames(zew) <- c("Date", "Econ_Growth", "Inflation", "Current", "STOXX50", "Interest_Rate")

zew_new <- read.csv("data/ZEW_eurozone_new.csv", sep = ";", dec = ",", encoding = "UTF-8")
colnames(zew_new) <- c("Date", "Current", "Econ_Growth", "Inflation", "Interest_Rate", "STOXX50")
zew_new <- zew_new[,c(1, 3, 2, 4, 6, 5)]

zew <- rbind(zew, zew_new)
zew <- zew %>% mutate(Date = paste0("01",substr(Date, 3, 10))) %>%
  mutate(Date = as.Date(Date, format = "%d.%m.%Y")) %>% filter(!is.na(Date)) %>%
  arrange(Date)

# visualisation
zew %>% dplyr::select(Date, Econ_Growth, Current) %>%
  mutate(Econ_Growth = dplyr::lag(Econ_Growth, 6)) %>%
  pivot_longer(-Date, names_to = "index", values_to = "value") %>%
  ggplot(aes(x = Date, y = value, col = index)) + geom_line()

# stationarity testing
adf.test(zew$Econ_Growth) # stationary
adf.test(zew$Inflation) # stationary
adf.test(zew$Current) # stationary
adf.test(zew$STOXX50) # non-stationary
adf.test(zew$Interest_Rate) # stationary

adf.test(diff(zew$STOXX50)) # stationary


plot(zew_stationary$STOXX50, type = "l")


# transforming ZEW STOXX50 as a deviation from its trend

library(mFilter)

zew_stationary <- zew %>%
  mutate(STOXX50_trend = hpfilter(STOXX50, 129600)$trend) %>%
  mutate(STOXX50 = STOXX50-STOXX50_trend) %>%
  dplyr::select(-STOXX50_trend)

zew_stationary %>% ggplot(aes(x = Date, y = Inflation)) + geom_line()
zew_stationary %>% ggplot(aes(x = Date, y = Econ_Growth)) + geom_line()
zew %>% ggplot(aes(x = Date, y = STOXX50)) + geom_line()

adf.test(zew_stationary$Econ_Growth)


# Plot for data description in the paper
# macroeconomic variables from the "macro data.R" file

full_data <- full_join(zew, macro_data_raw, by = "Date") %>%
  left_join(rec_data, by = "Date") %>%
  rename(ZEW_Econ_Growth = Econ_Growth, ZEW_Inflation = Inflation, ZEW_Current = Current,
         ZEW_STOXX = STOXX50.x, ZEW_RATE = Interest_Rate, Inflation = infl_EA,
         Rate = rate_EA, Unemployment = un_EA, GDP = gdp, STOXX50 = STOXX50.y)

full_data <- full_data[complete.cases(full_data),]

plot_data <- full_data %>%
  pivot_longer(-c(Date, EUROREC), names_to = "index", values_to = "value") %>%
  mutate(type = if_else(str_detect(index, "ZEW"), "ZEW", "macro"))

names <- c(
  ZEW_Current="ZEW Current Conditions",
  GDP="GDP",
  ZEW_Econ_Growth="ZEW Economic Sentiment",
  Unemployment="Unemployment",
  ZEW_Inflation="ZEW Inflation",
  Inflation="Inflation",
  ZEW_RATE="ZEW Short Term Interest Rate",
  Rate="Short Term Interest Rate",
  ZEW_STOXX="ZEW STOXX50",
  STOXX50="STOXX50"
)

plot_data$index <- factor(plot_data$index, levels = c("ZEW_Current", "GDP", "ZEW_Econ_Growth", "Unemployment",
                   "ZEW_Inflation", "Inflation", "ZEW_RATE", "Rate", "ZEW_STOXX", "STOXX50"), labels = names)


plot_data %>% ggplot(aes(x = Date, y = value, col = EUROREC)) + geom_line() +
  facet_wrap(~index, ncol = 2, scales = "free_y") + 
  theme_bw() + theme(legend.position = "none")

ggsave("figures/new data.png", width = 2000, height = 2400, units = "px", dpi = "print")

# descriptive stats table


library(moments)

desc <- full_data %>% dplyr::select(-c(Date, EUROREC)) %>%
  pivot_longer(everything(), names_to = "index", values_to = "value") %>%
  group_by(index) %>% summarise(
    mean = mean(value),
    median = median(value),
    sd = sd(value),
    skew = skewness(value),
    kurt = kurtosis(value)
  ) %>% mutate(across(-index, ~round(.x, 3)))

desc

adf.test(full_data[,11])



# check volatility during recession
full_data %>% dplyr::select(EUROREC, Inflation, Rate, Unemployment, GDP, STOXX50,
                            Uncertainty, Vstoxx) %>%
  gather(-EUROREC, key = "variable", value = "value") %>% group_by(EUROREC, variable) %>%
  mutate(value = if_else(variable %in% c("Vstoxx", "Uncertainty"), value, value/100 )) %>%
  summarise(
    m = mean(value),
    sd = sd(value)
  ) # higher volatility during recession
