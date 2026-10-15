
full_data <- full_join(zew_stationary, macro_stationary, by = "Date") %>%
  left_join(rec_data, by = "Date") %>% left_join(vstoxx, by = "Date") %>%
  left_join(uncer, by = "Date") %>%
  rename(ZEW_Econ_Growth = Econ_Growth, ZEW_Inflation = Inflation, ZEW_Current = Current,
         ZEW_STOXX = STOXX50.x, ZEW_RATE = Interest_Rate, Inflation = infl_EA,
         Rate = rate_EA, Unemployment = un_EA, GDP = gdp, STOXX50 = STOXX50.y)

full_data <- full_data[complete.cases(full_data),]

adf.test(full_data$ZEW_STOXX)



# ZEW CURRENT

data <- full_data %>% #filter(Date < as.Date("2020-01-01")) %>% # before covid 
  rename(x = ZEW_Current, y1 = ZEW_Econ_Growth, y2 = STOXX50, rec = EUROREC) %>% 
  #mutate(rec = Vstoxx) %>% # changing the interaction term
  dplyr::select(x, y1, y2, rec) %>%
  mutate(across(-rec,  ~./100)) %>%
  mutate(across(everything(), lag, .names = "{.col}l")) %>%
  mutate(across(everything(), ~. - lag(.,1), .names = "d{.col}")) %>%
  mutate(pxl = 1+xl, mxl = 1-xl) %>%
  filter(!is.na(x)) %>%
  #mutate(rec = recl-1) %>% # for testing purposes
  mutate(xm = mean(x))

data <- data[complete.cases(data),]

herding_model_basic(data, 2, 0.1)
herding_model(data, 2, 0.5)
herding_model_noy(data)
herding_model_noy_rec(data)
model <- herding_model_return(data, 2, 0.5)
summary(model)

# test
vcov(model)

U = 7.190813 - 6.762008
V =  1.211005^2 + 1.578241^2 + 2*(-1.422987076)

t <- U/sqrt(V)

1-pt(abs(t), 316-8)



# ZEW ECONOMIC GROWTH

data <- full_data %>% #filter(Date < as.Date("2020-01-01")) %>% # before covid
  rename(x = ZEW_Econ_Growth, y1 = ZEW_Current, y2 = ZEW_Inflation, 
         y3 = Inflation, y4 = STOXX50, y5 = Unemployment, rec = EUROREC) %>% 
  #mutate(rec = Uncertainty) %>% # changing the interaction term
  dplyr::select(x, y1, y2, y3, y4, y5, rec) %>%
  mutate(rec = lag(rec)) %>% # lag the rec so it matches xl
  mutate(across(-rec,  ~./100)) %>%
  mutate(across(everything(), lag, .names = "{.col}l")) %>%
  mutate(across(everything(), ~. - lag(.,1), .names = "d{.col}")) %>%
  mutate(pxl = 1+xl, mxl = 1-xl) %>%
  filter(!is.na(x)) %>%
  #mutate(rec = recl-1) %>% # for testing purposes
  mutate(xm = mean(x))

data <- data[complete.cases(data),]

herding_model_noy(data)
herding_model_noy_rec(data)
herding_model_basic(data, 5, 0.5)
herding_model(data, 5, 0.5) # highly significant recession

model <- herding_model_return(data, 5, 0.5)
summary(model)

# test
vcov(model)

U = 0.58681 - 1.64909
V =  0.70932^2 + 0.96415^2 + 2*(-0.5630049007)

t <- U/sqrt(V)

1-pt(abs(t), 316-13)


# ZEW INFLATION

data <- full_data %>% #filter(Date < as.Date("2020-01-01")) %>% # before covid
  rename(x = ZEW_Inflation, y1 = Inflation, y2 = Rate, rec = EUROREC) %>% 
  #mutate(rec = Uncertainty) %>% # changing the interaction term
  dplyr::select(x, y1, y2, rec) %>%
  mutate(rec = lag(rec)) %>% # lag the rec so it matches xl
  mutate(across(-rec,  ~./100)) %>%
  mutate(across(everything(), lag, .names = "{.col}l")) %>%
  mutate(across(everything(), ~. - lag(.,1), .names = "d{.col}")) %>%
  mutate(pxl = 1+xl, mxl = 1-xl) %>%
  filter(!is.na(x)) %>%
  mutate(rec = recl-1) %>% # for testing purposes
  mutate(xm = mean(x))

data <- data[complete.cases(data),]

herding_model_basic(data, 2, 0.9)
herding_model(data, 2, 0.9)
herding_model_noy(data)
herding_model_noy_rec(data)

model <- herding_model_return(data, 2, 0.3)
summary(model)

cor(full_data$ZEW_Inflation, full_data$Rate)

plot(x = full_data$Date, full_data$Inflation, type = "l")

# test
vcov(model)

U = 2.46809 - 1.16071
V =  0.346563662 + 0.485876056 - 2*(-0.268910700)

t <- U/sqrt(V)

1-pt(abs(t), 231)




# ZEW Rate

data <- full_data %>% filter(Date < as.Date("2020-01-01")) %>% # before covid 
  rename(x = ZEW_RATE, y1 = Rate, y2 = STOXX50, rec = EUROREC) %>% 
  #mutate(rec = Uncertainty) %>% # changing the interaction term
  dplyr::select(x, y1, y2, rec) %>%
  mutate(across(-rec,  ~./100)) %>%
  mutate(across(everything(), lag, .names = "{.col}l")) %>%
  mutate(across(everything(), ~. - lag(.,1), .names = "d{.col}")) %>%
  mutate(pxl = 1+xl, mxl = 1-xl) %>%
  filter(!is.na(x)) %>%
  #mutate(rec = recl-1) %>% # for testing purposes
  mutate(xm = mean(x))

data <- data[complete.cases(data),]

herding_model_basic(data, 2, 0.3)
herding_model(data, 2, 0.5)
herding_model_noy(data)
herding_model_noy_rec(data)

model <- herding_model_return(data, 2, 0.3)
summary(model)

# test
vcov(model)

U = 4.07744 - 2.11605
V =  0.519763562 + 0.511033910 - 2*(-0.367393509)

t <- U/sqrt(V)

1-pt(abs(t), 231)


# ZEW STOXX

data <- full_data %>% #filter(Date < as.Date("2020-01-01")) %>% # before covid
  rename(x = ZEW_STOXX, y1 = GDP, rec = EUROREC) %>% 
  #mutate(rec = Uncertainty) %>% # changing the interaction term
  dplyr::select(x, y1, rec) %>%
  mutate(across(-rec,  ~./100)) %>%
  mutate(across(everything(), lag, .names = "{.col}l")) %>%
  mutate(across(xl, ~. - lag(.,1), .names = "d{.col}")) %>%
  mutate(across(y1l, ~. - lag(.,1), .names = "d{.col}")) %>%
  mutate(pxl = 1+xl, mxl = 1-xl) %>%
  filter(!is.na(x)) %>%
  mutate(rec = recl-1) %>% # for testing purposes
  mutate(xm = mean(x))

data <- data[complete.cases(data),]

herding_model_basic(data, 1, 0.6)
herding_model(data, 1, 0.5) # nothing's changed
herding_model_noy(data)
herding_model_noy_rec(data)

model <- herding_model_return(data, 1, 0.6)
summary(model)

# test
vcov(model)

U = 0.12455 - 0.13076
V =  0.566190944 + 1.322615479 - 2*(-0.548392341)

t <- U/sqrt(V)

1-pt(abs(t), 231)



