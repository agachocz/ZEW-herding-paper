
# Reading and transforming all macroeconomic variables

transform_date <- function(x){
  split <- as.numeric(str_split(x, "\\.")[[1]])
  date <- paste0(split[3], "-", split[1], "-01")
  return(date)
}

library(dplyr)

# inflation

cphi <- read.csv("data/inflation.csv") %>% 
  filter(geo %in% c("PL", "EA")) %>%
  mutate(Date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  pivot_wider(names_from = "geo", values_from = OBS_VALUE) %>% arrange(Date) %>%
  group_by(Date) %>% summarise(
    EA = mean(EA, na.rm = T),
    PL = mean(PL, na.rm = T)
  ) %>%
  mutate(infl_PL = (lag(PL)-lag(PL, 13))/lag(PL,13),
         infl_EA = (lag(EA)-lag(EA, 13))/lag(EA,13)) %>%
  dplyr::select(Date, infl_PL, infl_EA) %>%
  mutate(diff_infl = infl_PL - infl_EA)

cphi_new <- read.csv("data/inflation new.csv", sep = ";") %>%
  mutate(price_index = str_replace_all(price_index, ",", ".")) %>%
  mutate(price_index = as.numeric(price_index), Date = as.Date(paste0(TIME,"-01"))) %>%
  filter(!is.na(price_index)) %>%
  mutate(infl_EA = (lag(price_index)-lag(price_index, 13))/lag(price_index,13)) %>%
  filter(Date > as.Date("2023-09-01")) %>% dplyr::select(Date, infl_EA)

inflation <- cphi %>% dplyr::select(Date, infl_EA) %>% rbind(cphi_new)

inflation %>% ggplot(aes(x = Date, y = infl_EA)) + geom_line() # looks OK

# short term interest rate

rate_raw <- read.csv("data/interest rate.csv") %>% 
  filter(geo %in% c("PL", "EA")) %>%
  mutate(Date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  pivot_wider(names_from = "geo", values_from = OBS_VALUE) %>%
  mutate(diff_rate = PL - EA) %>%
  rename(rate_PL = PL, rate_EA = EA)

rate_raw_new <- read.csv("data/interest rate new.csv", sep = ";") %>%
  mutate(interest_rate = str_replace_all(interest_rate, ",", ".")) %>%
  mutate(interest_rate = as.numeric(interest_rate), Date = as.Date(paste0(TIME,"-01"))) %>%
  filter(!is.na(interest_rate), Date > as.Date("2023-09-01")) %>% dplyr::select(Date, rate_EA = interest_rate)
  
rate_raw <- rate_raw %>% dplyr::select(Date, rate_EA) %>% rbind(rate_raw_new)
rate_raw %>% ggplot(aes(x = Date, y = rate_EA)) + geom_line() # looks OK

rate <- rate_raw %>% arrange(Date) %>%
  mutate(rate_EA = (lag(rate_EA)-lag(rate_EA, 2)))


# unemployment

unemployment_raw <- read.csv("data/unemployment.csv") %>% 
  filter(geo %in% c("PL", "EA20")) %>%
  mutate(Date = as.Date(paste0(TIME_PERIOD,"-01"))) %>%
  pivot_wider(names_from = "geo", values_from = OBS_VALUE) %>%
  mutate(diff_un = PL - EA20) %>%
  rename(un_PL = PL, un_EA = EA20)

unemployment_raw_new <- read.csv("data/unemployment new.csv", sep = ";") %>%
  mutate(unemployment = str_replace_all(unemployment, ",", ".")) %>%
  mutate(unemployment = as.numeric(unemployment), Date = as.Date(paste0(TIME,"-01"))) %>%
  filter(!is.na(unemployment), Date > as.Date("2023-08-01")) %>% dplyr::select(Date, un_EA = unemployment)


unemployment_raw <- unemployment_raw %>% dplyr::select(Date, un_EA) %>% rbind(unemployment_raw_new)
unemployment <- unemployment_raw %>% arrange(Date) %>%
  mutate(un_EA = (lag(un_EA)-lag(un_EA, 2)))


# GDP

gdp_raw <- read.csv("data/gdp_EA.csv") %>%
  dplyr::select(TIME_PERIOD, gdp = OBS_VALUE) %>% 
  mutate(Year = substr(TIME_PERIOD, 1, 4)) %>%
  mutate(Q = as.numeric(substr(TIME_PERIOD, 7, 7))) %>%
  mutate(gdp_rate = ((gdp-lag(gdp))/lag(gdp))*100) %>%
  mutate(gdp_rate = log(gdp/lag(gdp)))
  
gdp_EA <- read.csv("data/gdp euro area new.csv") %>%
  dplyr::select(TIME_PERIOD, gdp = OBS_VALUE) %>% 
  mutate(Year = substr(TIME_PERIOD, 1, 4)) %>%
  mutate(Q = as.numeric(substr(TIME_PERIOD, 7, 7))) %>%
  mutate(gdp_rate = ((gdp-lag(gdp))/lag(gdp))*100) %>%
  mutate(gdp_rate = log(gdp/lag(gdp)))

# transform quarterly to monthly data

years <- c(rep(1995:2025, each = 12), rep(2026, 6))
months <- c(rep(1:12, times = 31), 1:6)
dates <- as.Date(paste0(years, "-", months, "-01"))

gdp_monthly <- data.frame(Date = c(dates),
               gdp = rep(gdp_EA$gdp_rate, each = 3))

# STOXX50

stoxx <- read.csv("data/STOXX50.csv", sep = ";")[,c(2,6)] # choose only date and closing price
colnames(stoxx) <- c("Date", "STOXX50")

transform_date <- function(x){
  split <- as.numeric(str_split(x, "\\.")[[1]])
  date <- paste0(split[3], "-", split[2], "-01")
  return(date)
}

dates <- sapply(stoxx$Date, transform_date)
stoxx$Date <- as.Date(dates, format = "%Y-%m-%d")



# join all variables

macro_data <- inflation %>% full_join(rate_raw, by = "Date") %>%
  full_join(unemployment_raw, by = "Date") %>% left_join(gdp_monthly, by = "Date") %>% 
  left_join(stoxx, by = "Date") %>% dplyr::select(Date, infl_EA, rate_EA, un_EA, gdp, STOXX50) %>%
  filter(Date > as.Date("2000-01-01")) %>%
  filter(if_all(everything(), complete.cases))

macro_data_raw <- inflation %>% full_join(rate_raw, by = "Date") %>%
  full_join(unemployment_raw, by = "Date") %>% left_join(gdp_monthly, by = "Date") %>% 
  left_join(stoxx, by = "Date") %>%
  filter(Date > as.Date("2000-01-01")) %>%
  dplyr::select(Date, infl_EA, rate_EA, un_EA, gdp, STOXX50) %>%
  filter(if_all(everything(), complete.cases))

adf.test(macro_data$STOXX50) # non-stationary

library(mFilter)
library(scales)
library(tseries)
l <- 14400

macro_stationary <- macro_data %>% 
  dplyr::select(Date, infl_EA, rate_EA, un_EA, gdp, STOXX50) %>%
  mutate(STOXX50 = STOXX50 - hpfilter(STOXX50, l)$trend) %>%
  mutate(un_EA = un_EA - hpfilter(un_EA, l)$trend) %>%
  mutate(rate_EA = rate_EA - hpfilter(rate_EA, l)$trend) %>%
  #mutate(STOXX50 = (STOXX50-STOXX50_trend)/STOXX50_trend) %>%
  mutate(across(infl_EA:STOXX50, rescale)) %>%
  mutate(across(infl_EA:STOXX50, ~.*100))

macro_stationary <- macro_stationary[complete.cases(macro_stationary),]

macro_stationary %>% ggplot(aes(x = Date, y = STOXX50)) + geom_line()
adf.test(macro_stationary$rate_EA)


# recessions

rec_data <- read.csv("data/EURO recession.csv") %>% rename(Date = observation_date) %>%
  mutate(Date = as.Date(Date))

Sys.setlocale("LC_TIME", "English") # for reading English month names

rec_data_new <- read.csv("data/Growth-cycle-indicator.csv") %>% 
  mutate(Date = as.Date(paste0("01/", Category), format = "%d/%h/%Y")) %>%
  mutate(EUROREC = round(Business.cycle.coincident.indicator)) %>%
  dplyr::select(Date, EUROREC) %>% filter(Date > as.Date("2022-08-01"))

rec_data <- rec_data %>% rbind(rec_data_new)
rec_data %>% ggplot(aes(x = Date, y = EUROREC)) + geom_line()




# uncertainty measures
library(scales)

vstoxx <- read.csv("data/vstoxx.csv", sep = ";") %>%
  mutate(Month = substr(Date, 4, 10)) %>%
  group_by(Month) %>% summarise(Indexvalue = mean(Indexvalue)) %>%
  mutate(Date = paste0("01.", Month)) %>% 
  mutate(Date = as.Date(Date, format = "%d.%m.%Y")) %>%
  dplyr::select(Date, Vstoxx = Indexvalue) %>% arrange(Date)

# newest data obtained manually from Investing.com
vstoxx_new <- data.frame(
  Date = c("2025-05-01", "2025-06-01", "2025-07-01", "2025-08-01", "2025-09-01", "2025-10-01", "2025-11-01", 
           "2025-12-01", "2026-01-01", "2026-02-01", "2026-03-01", "2026-04-01", "2026-05-01", "2026-06-01",
           "2026-07-01", "2026-08-01", "2026-09-01"),
  Vstoxx = c(20.2, 18.5, 17.85, 18.35, 17.75, 19, 17, 16, 19.55, 20.1, 28.3, 21.65, 19.7, 17.6, 18.5, 17.25, 18.85)
) %>% mutate(Date = as.Date(Date))

vstoxx <- rbind(vstoxx, vstoxx_new) %>%
  mutate(Vstoxx = rescale(Vstoxx, to = c(0,1)))

plot(vstoxx$Vstoxx, type = "l")

uncer <- read.csv("data/Europe_Policy_Uncertainty_Data.csv", sep = ";")
colnames(uncer) <- c("Year", "Month", "Uncertainty")
uncer <- uncer %>% mutate(Date = paste0(Year, "-", Month, "-01")) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Uncertainty = rescale(Uncertainty, to = c(0,1))) %>%
  dplyr::select(Date, Uncertainty)

plot(uncer$Uncertainty, type = "l")
max(vstoxx$Date)


