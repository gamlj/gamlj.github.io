library(lmerTest)
library(nlme)
library(tidyverse)


d<-read.csv("~/Downloads/Cod_daily_depth_data.csv") %>%
  mutate(date = lubridate::dmy(date)) %>%
  rename(temperature = Temperature_1m) %>%
  na.omit %>% 
  as_data_frame() %>%
  arrange(fish, date)
N<-length(table(d$fish))
summary(as.vector(table(d$fish)))
table(d$date,d$fish)
ggplot(d, aes(temperature, -log(depth_mean_day))) + 
  geom_point(alpha = 0.3) +
  facet_wrap(~fish)
d$fish<-factor(d$fish)
d <- d %>% mutate(temperature_centered = temperature - mean(temperature))
m1 <- lmer(log(depth_mean_day) ~ temperature_centered +
             (1 + temperature_centered | fish), data = d) # exercise
summary(m1)



library(broom.mixed)
aug <- broom.mixed::augment(m1)

ggplot(aug, aes(temperature_centered, `log(depth_mean_day)`)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~fish) +
  geom_line(aes(x = temperature_centered, y = .fitted), colour = "red")

ggplot(aug, aes(.fitted, .resid)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~fish) +
  geom_hline(yintercept = 0, colour = "red")

aug$date <- d$date
ggplot(aug, aes(date, .resid)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~fish) +
  geom_hline(yintercept = 0, colour = "red")

library(nlme)
m2 <- lme(
  log(depth_mean_day) ~ temperature_centered, random = list(~1 + temperature_centered | fish,~ 1 | date), data = d) # exercise
summary(m2)

m2 <- lme(
  log(depth_mean_day) ~ temperature_centered, random = ~ 1 + temperature_centered | fish, data = d, correlation = corCompSymm(~1|fish)) # exercise
summary(m2)

arm::display(m1)
summary(m2)
plot(m2, resid(., type = "normalized") ~ fitted(.) | fish, abline = 0)
plot(m2, resid(., type = "normalized") ~ fitted(.) | fish, abline = 0)
nlme:::residuals.lme
data<-lme4::fortify.merMod(m3)
data$.resid<-resid(m3, type = "normalized")
cluster<-"fish"
plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = .fitted, y = .resid)) 
plot <- plot + ggplot2::labs(x = "Predicted", y = "Residuals", color=cluster)
plot <- plot + ggplot2::geom_point(shape = 21)
plot <- plot + ggplot2::geom_hline(yintercept = 0, colour = "gray")
plot <- plot + ggtheme
plot <- plot + ggplot2::theme(legend.position="bottom")
plot + ggplot2::facet_wrap("fish")

table(data$fish)
plot(ACF(m2, resType = "normalized"))

m3 <- lme(log(depth_mean_day) ~ temperature_centered, 
          random = ~ 1 + temperature_centered | fish, data = d,
          correlation = 
            corAR1()# exercise
)
summary(m3)

plot(m3, resid(., type = "normalized") ~ lubridate::day(date) | fish, 
     abline = 0, cex = 0.3)
plot(m3, resid(., type = "normalized") ~ fitted(.) | fish, 
     abline = 0, cex = 0.3)


plot(ACF(m3, resType = "normalized"))

plot(m3, resid(.) ~ lubridate::day(date) | fish, abline = 0, cex = 0.3)
plot(m3, resid(.) ~ fitted(.) | fish, abline = 0, cex = 0.3)
