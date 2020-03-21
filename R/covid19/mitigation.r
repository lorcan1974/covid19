##################################################################################################
#
#
# What mitigation measures have been implemented and what are the impacts?
#
#
##################################################################################################

library(zoo)
library(broom)
library(gridExtra)
picture_path <- 'pictures'
days_onset2death <- 21

##################################################################################################
#
# First determine the countries and/or provinces that have the highest death toll so far.
#
##################################################################################################

#
# As of 2020-03-18 the top countries by number of deaths are:
# cv_with_pop[, .(.N, deaths = max(deaths)), by = 'code'][order(deaths, decreasing = T)][1:10]
#
# code    N deaths
# 1:  CHN 1703   3099
# 2:  ITA   46   2158
# 3:  ESP   45    342
# 4:  FRA   75    148
# 5:  KOR   55     75
# 6:  GBR   46     55

# By Province,
# cv_with_pop[, .(.N, deaths = max(deaths)), by = 'province'][order(deaths, decreasing = T)][1:10]
# 
# province   N deaths
# 1:           Hubei  55   3099
# 4:      Washington  17     48

high_countries <- c('ITA', 'KOR', 'FRA', 'UK', 'ESP')
high_provinces <- c('Hubei')

drate_fun <- function(deaths) {
  summary(lm(log(deaths)~seq_along(deaths)))$coefficients[2,1]
}

pval_fun <- function(deaths) {
  summary(lm(log(deaths)~seq_along(deaths)))$coefficients[2,4]
}

smooth_rate <- function(x, k = 7) {
  lm1 <- summary(lm(log(deaths)~obs, data = cv_deaths[region=='Hubei'][order(obs)][4:7]))
  drate <- rollapply(x, k, drate_fun, fill=NA)
  pval <- rollapply(x, k, pval_fun, fill=NA)
  ifelse(is.na(pval) | pval<0.05, drate, NA_real_)
}

cv_deaths <- rbind(
  cv_with_pop[code %in% high_countries, .(region=country, obs, deaths)],
  cv_with_pop[province %in% high_provinces, .(region=province, obs, deaths)]
  )[deaths>0][
    order(obs), death_roc:=smooth_rate(deaths, k=7), by = 'region'
    ][!is.na(death_roc)]

p_infection <- ggplot(cv_deaths, aes(x=obs-days(days_onset2death), death_roc, colour = region)) + geom_line() + geom_point() + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Likely infection date",
    y = "Rate of change of Infection rate",
    colour = "Region",
    title = "Rate of change of infection rate (estimated)"
  ) +
  theme(legend.position = 'none')

d_wuhan <- as.Date('2020-01-23')

wuhan <- list(
  geom_vline(xintercept = d_wuhan, colour = 'purple4', size=1),
  geom_label(aes(x = d_wuhan, y = .4, label = "Wuhan\nlockdown"), colour = 'black')
)


d_italy1 <- as.Date('2020-02-21')
d_italy2 <- as.Date('2020-03-08')
d_italy3 <- as.Date('2020-03-09')

italy <- list(
  geom_vline(xintercept = d_italy1, colour = 'blue'),
  geom_vline(xintercept = d_italy2, colour = 'blue'), 
  geom_vline(xintercept = d_italy3, colour = 'blue'),
  geom_label(aes(x = d_italy1, y = .4, label = "Lodi lockdown"), colour = 'black'),
  geom_label(aes(x = d_italy2, y = .4, label = "North\nquarantined"), colour = 'black'),
  geom_label(aes(x = d_italy3, y = .4, label = "Quarantine\nextended"), colour = 'black')
)

p1 <- p_infection + wuhan

p2 <- ggplot(cv_deaths, aes(obs, death_roc, colour = region)) + geom_line() + geom_point() + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Date",
    y = "Rate of change in mortality rate",
    colour = "Region",
    title = "Rate of change in mortality rate"
  ) +
  theme(legend.position = c(.3, .7))

g1 <- grid.arrange(p1, p2, nrow = 1, right="")

ggsave('death_rate_roc.png', 
       plot = g1, 
       path = picture_path,
       width = 13, height = 8.61)


ggplot(cv_deaths, aes(x=obs, deaths, colour = region)) + geom_line() + geom_point() + 
  labs(
    x = "Date",
    y = "Deaths (log scale)",
    colour = "Region",
    title = "Deaths by region"
  )

ggsave(file.path(picture_path, 'deaths_20200318.png'))


