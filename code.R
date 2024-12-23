library(tidyverse)
library(broom)
library(glue)
library(ggplot2)
library(scales)


df_MSCI <- readxl::read_excel("adatokesemnyelemzs/MSCI International World Price Index USD Realtime Index.xlsx", sheet = 2)

manipulate_data <- function(df, win_start, est_win_length, event_win_start, event_win_length, name) {
  
# calculate log returns
df_company <- df %>% select(`Exchange Date`, "CompanyClose" = Close) %>% 
  arrange(`Exchange Date`) %>% 
  mutate(lnreturn =  log(CompanyClose /  lag(CompanyClose,1))) %>% 
  tail(-1)

df_MSCI_edit <- df_MSCI %>% select(`Exchange Date`,"IndexClose" = Close) %>% 
  arrange(`Exchange Date`) %>% 
  mutate(Indexlnreturn =  log(IndexClose /  lag(IndexClose,1))) %>% 
  tail(-1)

# make windows
joindata <-  left_join(df_company, df_MSCI_edit, by = "Exchange Date") %>% 
  mutate(
    est_win = if_else(
      row_number() >= which(`Exchange Date` == as.Date(win_start))[1] &
      row_number() < which(`Exchange Date` == as.Date(win_start))[1] + est_win_length,
      1,
      0
    ),
    event_win = if_else(
      row_number() >= which(`Exchange Date` == as.Date(event_win_start))[1] &
      row_number() <  which(`Exchange Date` == as.Date(event_win_start))[1] + event_win_length,
      1,
      0
    )
    
  )

# run regression  - in est_window
fit <- lm(formula = lnreturn ~ Indexlnreturn, data = filter(joindata, est_win == 1))
beta0 <- summary(fit)$coefficients[1]
beta1 <- summary(fit)$coefficients[2]

# predict 
joindata <- joindata %>% 
  mutate(
    beta0 = beta0,
    beta1 = beta1,
    predicted = beta0 + beta1 *Indexlnreturn,
    AR = lnreturn - predicted
  )

stenderd_deviation <- sd(filter(joindata, est_win == 1)$AR)

joindata <- joindata %>% 
  mutate(
    sd = ifelse(est_win == 1, stenderd_deviation, NA)
  )

CAR <- sum(filter(joindata, event_win == 1)$AR)
se_CAR <-  stenderd_deviation * sqrt(nrow(filter(joindata, event_win == 1)))
t_test <- CAR / se_CAR
p_value = 2* (1- pt(abs(t_test), nrow(filter(joindata, event_win == 1)) -1))

joindata <- joindata %>% 
    mutate(
    CAR = ifelse(event_win == 1, CAR, NA),
    se_CAR = ifelse(event_win == 1, se_CAR, NA),
    t_test = ifelse(event_win == 1, t_test, NA),
    p_value = ifelse(event_win == 1, p_value, NA)
    )
  
  
write.csv(joindata, glue("pred/{name}_{win_start}_{est_win_length}_{event_win_start}_{event_win_length}.csv"), row.names = FALSE)
#return(joindata)
}

df_Ap <- readxl::read_excel("adatokesemnyelemzs/AP Moeller - Maersk AS.xlsx", sheet = 2)
df_Cosco <- readxl::read_excel("adatokesemnyelemzs/COSCO Shipping Holdings Co Ltd.xlsx", sheet = 2)
df_Evergreen <- readxl::read_excel("adatokesemnyelemzs/Evergreen Marine Corp Taiwan Ltd.xlsx", sheet = 2)
df_Hapag <- readxl::read_excel("adatokesemnyelemzs/Hapag-Lloyd AG.xlsx", sheet = 2)
df_HMM <- readxl::read_excel("adatokesemnyelemzs/HMM Co Ltd.xlsx", sheet = 2)
df_Kawasaki <- readxl::read_excel("adatokesemnyelemzs/Kawasaki Kisen Kaisha Ltd.xlsx", sheet = 2)
df_Mitsui <- readxl::read_excel("adatokesemnyelemzs/Mitsui O.S.K. Lines Ltd.xlsx", sheet = 2)
df_Nippon <- readxl::read_excel("adatokesemnyelemzs/Nippon Yusen KK.xlsx", sheet = 2)
df_Orient <- readxl::read_excel("adatokesemnyelemzs/Orient Overseas (International) Ltd.xlsx", sheet = 2)
df_Yang <- readxl::read_excel("adatokesemnyelemzs/Yang Ming Marine Transport Corp.xlsx", sheet = 2)
  
estimation_window = "2020-09-03"
estwlength = 120
event_window = "2021-03-23"
ewlength = 21

manipulate_data(df_Ap, estimation_window, estwlength, event_window, ewlength, "AP")
manipulate_data(df_Cosco, estimation_window, estwlength, event_window, ewlength, "Cosco")
manipulate_data(df_Evergreen, estimation_window, estwlength, event_window, ewlength, "Evergreen")
manipulate_data(df_Hapag, estimation_window, estwlength, event_window, ewlength, "Hapag")
manipulate_data(df_HMM, estimation_window, estwlength, event_window, ewlength, "HMM")
manipulate_data(df_Kawasaki, estimation_window, estwlength, event_window, ewlength, "Kawasaki")
manipulate_data(df_Mitsui, estimation_window, estwlength, event_window, ewlength, "Mitsui")
manipulate_data(df_Nippon, estimation_window, estwlength, event_window, ewlength, "Nippon")
manipulate_data(df_Orient, estimation_window, estwlength, event_window, ewlength, "Orient")
manipulate_data(df_Yang, estimation_window, estwlength, event_window, ewlength, "Yang")
  
dfproba <- 
dfproba %>% 
  mutate(
    margin_of_error = qt(1-.05/2, nrow(dfproba)-1) * (sd(AR)/sqrt(nrow(dfproba))),
    CI_lower = mean(AR)-margin_of_error,
    CI_upper = mean(AR)+margin_of_error,
    )

# plot
target_row = which(as.Date(dfproba$`Exchange Date`) == event_window)
dfproba <- dfproba %>% 
  mutate(
    t = row_number() - target_row
  )
dfproba %>% 
  filter(t >= -10 & t <= 10) %>% 
  ggplot(aes(x = t, y = AR)) +
  geom_line() +
  geom_line() + 
  scale_y_continuous(labels = label_percent()) +
  #ylim(-.2,.2)+
  geom_hline(yintercept = 0, color = "grey")+
  theme_minimal()

dfproba %>% 
  filter(t>=10 & t<=10) %>% 
  ggplot(aes(t))+
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), fill = "grey")
  geom_line(aes(y = CI_lower), color = "black")+
  geom_line(aes(y = CI_upper), color = "black")
