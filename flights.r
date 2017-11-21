load("flight")
library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)

daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())
ggplot(daily, aes(date, n)) + 
  geom_line()
ggsave("date_n.jpg")

daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) + 
  geom_boxplot()
ggsave("wday_n.jpg")

mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4)
ggsave("wday_pred.jpg")

daily <- daily %>% 
  add_residuals(mod)
ggplot(daily, aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line()
ggsave("residual.jpg")

ggplot(daily, aes(date, resid, colour = wday)) + 
  geom_ref_line(h = 0) + 
  geom_line()
ggsave("wday_residual.jpg")

daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") + 
  geom_smooth(se = FALSE, span = 0.20)
ggsave("residual_trend.jpg")


daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) + 
  geom_point() + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")
ggsave("sat_flight.jpg")

term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

daily <- daily %>% 
  mutate(term = term(date)) 

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")
ggsave("sat_term_flight.jpg")

daily %>% 
  ggplot(aes(wday, n, colour = term)) +
  geom_boxplot()
ggsave("term_effect_on_flight.jpg")

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)
ggsave("residual_with_without_term_mode12.jpg")

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() + 
  geom_point(data = grid, colour = "red") + 
  facet_wrap(~ term)
ggsave("mod2_wday_term_flight.jpg")

mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()
ggsave("mod3__resid_flight.jpg")


library(splines)
mod4 <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod4) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
  geom_line() +
  geom_point()
ggsave("mod4_wday.jpg")

#homework 3

daily <- mutate(daily,  pred1=predict(mod1, daily))
ggplot(data = daily)+
  geom_line(mapping = aes(x = date, y = n))+
  geom_line(mapping = aes(x = date, y = pred1, color = "red"))
ggsave("modpred1_true.jpg")

daily <- mutate(daily,  pred2=predict(mod2, daily))
ggplot(data = daily)+
  geom_line(mapping = aes(x = date, y = n))+
  geom_line(mapping = aes(x = date, y = pred2, color = "red"))
ggsave("modpred2_true.jpg")

daily <- mutate(daily,  pred3=predict(mod3, daily))
ggplot(data = daily)+
  geom_line(mapping = aes(x = date, y = n))+
  geom_line(mapping = aes(x = date, y = pred3, color = "red"))
ggsave("modpred3_true.jpg")

daily <- mutate(daily,  pred4=predict(mod4, daily))
ggplot(data = daily)+
  geom_line(mapping = aes(x = date, y = n))+
  geom_line(mapping = aes(x = date, y = pred4, color = "red"))
ggsave("modpred4_true.jpg")


#24.3.5.3

daily <- daily %>%
  mutate(sat_terms = 
           case_when(wday == "Sat" & term == "summer" ~ "Sat-summer",
                     wday == "Sat" & term == "fall" ~ "Sat-fall",
                     wday == "Sat" & term == "spring" ~ "Sat-spring",
                     TRUE ~ as.character(wday)))
mod5 <- lm(n ~ sat_terms, data = daily)
daily %>% 
  gather_residuals(sat_term_resid = mod5, wday_terms_resid = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line()

ggsave("sat_term_comp.jpg")

#24.3.5.4

daily <- daily %>%
  mutate(wday_sat_term_holiday = 
           case_when(
             date %in% lubridate::ymd(c(20130101,
                                        20130121,
                                        20130527,
                                        20130704, 
                                        20130902, 
                                        20131128,
                                        20131224,
                                        20131225)) ~
               "holiday",
             wday == "Sat" & term == "summer" ~ "Sat-summer",
             wday == "Sat" & term == "fall" ~ "Sat-fall",
             wday == "Sat" & term == "spring" ~ "Sat-spring",
             TRUE ~ as.character(wday)))
mod6 <- lm(n ~ wday_sat_term_holiday, data = daily)
daily %>% 
  add_residuals(mod6, "mod6_resid") %>% 
  ggplot(aes(date, mod6_resid)) + 
  geom_line()

ggsave("holiday.jpg")


#24.3.5.5


daily <- daily %>% 
  mutate(month = month(date))

mod7 <- lm(n ~ wday*month, data = daily)

daily %>% 
  gather_residuals(wday_month_resid = mod7, wday_resid = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line()

ggsave("wday_month_vs_wday_term.jpg")


#24.3.5.6

mod8 <- lm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod8) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
  geom_line() +
  geom_point()


ggsave("mod8.jpg")

#24.3.5.7

flight_distance <-flights %>% 
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(mean_distance =  mean(distance))
  
  ggplot(flight_distance,aes(x = wday, y = mean_distance)) +
  geom_point()
ggsave("wday_mean_distance.jpg")

flight_time <-flights %>% 
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(mean_time =  mean(hour))
ggplot(flight_time,aes(x = wday, y = mean_time)) +
  geom_point()

ggsave("wday_mean_time.jpg")




save.image("flight")



