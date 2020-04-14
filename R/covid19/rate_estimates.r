library(scales)
library(egg)

picture_path <- 'pictures'

hc_d <- cv_with_pop[deaths>10, .(.N, x = max(deaths)), by = 'code'][order(x, decreasing = T)][x>50 & N>10]
hc_c <- cv_with_pop[confirmed>10, .(.N, x = max(confirmed)), by = 'code'][order(x, decreasing = T)][x>300 & N>10]
hp <- cv_with_pop[, .(.N, x = max(deaths)), by = 'province'][order(x, decreasing = T)][1:10]

high_countries_c <- c('ITA', 'ESP', 'DEU', 'FRA', 'KOR', 'CHE', 'GBR', 'NLD', 'AUT', 'BEL', 'DNK', 'MYS', 'IRL')
high_countries_d <- c('ITA', 'ESP', 'FRA', 'KOR')

countries_by_province <- c('USA', 'CHN', 'AUS', 'CAN')

high_countries_c <- setdiff(hc_c$code, countries_by_province)
eu_countries <- c('AUT', 'BEL', 'CHE', 'DEU', 'DNK', 'ESP', 'FIN', 'FRA', 'GBR', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT')
high_countries_noneu <- setdiff(high_countries_c, eu_countries)
high_countries_eu <- intersect(high_countries_c, eu_countries)

high_countries_d <- setdiff(hc_d$code, c('USA', 'CHN', 'AUS'))
high_provinces <- c('Hubei', 'New York', 'Washington')

#
# Assuming y ~ Kexp(rt), then log|y| ~ log|K| + r*t
# So regression should give the rate r in the Beta_1 coefficient
#
rate_fun <- function(x) {
  summary(lm(log(x)~seq_along(x)))$coefficients[2,1]
}

pval_fun <- function(x) {
  summary(lm(log(x)~seq_along(x)))$coefficients[2,4]
}

smooth_rate <- function(x, k = 7) {
  drate <- rollapply(x, k, rate_fun, fill=NA, align = 'right')
  pval <- rollapply(x, k, pval_fun, fill=NA, align = 'right')
  ifelse(is.na(pval) | pval<0.05, drate, NA_real_)
}

rate_fun_lin <- function(x) {
  summary(lm((x)~seq_along(x)))$coefficients[2,1]
}

pval_fun_lin <- function(x) {
  summary(lm((x)~seq_along(x)))$coefficients[2,4]
}

smooth_rate_lin <- function(x, k = 7) {
  drate <- rollapply(x, k, rate_fun_lin, fill=NA, align = 'right')
  pval <- rollapply(x, k, pval_fun_lin, fill=NA, align = 'right')
  ifelse(is.na(pval) | pval<0.05, drate, NA_real_)
}

d_rates <- rbind(
  cv_with_pop[code %in% high_countries_d, .(region=country, obs, y = deaths)],
  cv_with_pop[province %in% high_provinces, .(region=province, obs, y = deaths)]
)[y>10][
  order(obs), y_rate:=smooth_rate(y, k=5) , by = 'region'
  ]

c_rates <- rbind(
  cv_with_pop[code %in% high_countries_c, .(region=code, obs, y = confirmed, pop)],
  cv_with_pop[province %in% high_provinces, .(region=province, obs, y = confirmed, pop)]
)[y>30][
  order(obs), c('y_rate', 'delta_y'):=list(smooth_rate(y, k=5), rollmean(c(NA, diff(y)), k=5, fill=NA)) , by = 'region'
  ]

c_rates <- c_rates[, c('y_pm', 'delta_y_pm'):=list(y/pop, delta_y/pop)]

p_test_v_burden <- ggplot(mapping = aes(obs, delta_y_pm, colour = region)) + geom_point() + geom_line() +
  scale_y_log10()

p_test_v_burden %+% c_rates[region %in% high_countries_eu & !is.na(delta_y) & region<='DNK']
p_test_v_burden %+% c_rates[region %in% high_countries_eu & !is.na(delta_y) & region>'DNK' & region<='GRC']
p_test_v_burden %+% c_rates[region %in% high_countries_eu & !is.na(delta_y) & region>'GRC']

p_test_v_burden %+% c_rates[(region %in% high_countries_noneu | region %in% high_provinces) & !is.na(delta_y)]

#############################################################################
#
# Rates all on the one plot
#
#############################################################################

p_rate <- ggplot(mapping = aes(obs, y_rate, colour = region)) + geom_point() + geom_line() + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0, NA)) +
  labs(
    x = "Date",
    y = "Case growth rate",
    colour = "Region"
  ) + geom_hline(yintercept = 0.05, colour = 'red', size = 1) 

  # geom_text(mapping = aes(x = as.Date('2020-03-13'), y = 0.08, label = "Losing"), colour = 'black') +
  # geom_text(mapping = aes(x = as.Date('2020-03-13'), y = 0.03, label = "Winning"), colour = 'black')

p_rate %+% c_rates[!is.na(y_rate) & region %in% c(high_countries_noneu, 'Hubei')]

r_oscilate <- c('MYS', 'ISR', 'AUT', 'BEL', 'CHE', 'DEU', 'ESP', 'FRA', 'GBR', 'GRC', 'IRL', 'NLD', 'PRT')
r_down <- c('DNK', 'FIN', 'KOR', 'JPN', 'Hubei', 'ITA')

r_earliest <- c_rates[region %in% r_oscilate, .(obs=min(obs)), by = 'region'][order(obs)]

p_rate %+% c_rates[!is.na(y_rate) & region %in% r_down & obs>='2020-02-15']
p_rate %+% 
  merge(c_rates[!is.na(y_rate) & region %in% r_oscilate], 
        r_earliest[obs<='2020-03-03', .(region)], 
        by = 'region')

p_rate %+% 
  merge(c_rates[!is.na(y_rate) & region %in% r_oscilate], 
        r_earliest[obs>'2020-03-03', .(region)], 
        by = 'region')


p_rate %+% c_rates[!is.na(y_rate) & region %in% high_countries_eu & region %in% c('GRC')]
p_rate %+% c_rates[!is.na(y_rate) & region %in% high_countries_eu & region<='DNK']
p_rate %+% c_rates[!is.na(y_rate) & region %in% high_countries_eu & region>'GRC' & region<='Z'] + labs(y = "Case growth rate")

p_rate %+% c_rates[!is.na(y_rate) & region %in% high_countries_eu] + labs(y = "Case growth rate")
p_rate %+% d_rates[!is.na(y_rate) & obs>='2020-03-02'] + labs(y = "Mortality growth rate")

#############################################################################
#
# Cases/Deaths per million by region
#
#############################################################################

p_pm <- ggplot(mapping = aes(obs, y/pop, colour = region)) + geom_point() + geom_line() + 
  # scale_y_continuous(trans=log_trans(), labels = comma_format()) +
  scale_y_log10() +
  # scale_y_continuous(labels = comma_format()) +
  labs(
    x = "Date",
    y = "Cases per million",
    colour = "Region"
  )

p_pm %+% c_rates[!is.na(y) & region %in% high_countries_eu]

p_pm %+% c_rates[!is.na(y_rate) & region %in% r_down & obs>='2020-02-15']
p_pm %+% 
  merge(c_rates[!is.na(y_rate) & region %in% r_oscilate], 
        r_earliest[obs<='2020-03-03', .(region)], 
        by = 'region')

p_pm %+% 
  merge(c_rates[!is.na(y_rate) & region %in% r_oscilate], 
        r_earliest[obs>'2020-03-03', .(region)], 
        by = 'region')

#############################################################################
#
# Split the rates out by region
#
#############################################################################

growth_plots <- function(location, cv_rates, picture_path, p1y, p2y, ptitle, fname_prefix) {
  stopifnot(location %in% cv_rates$region)
  cc <- cv_rates[region==location]
  p1 <- ggplot(cc[!is.na(y)], aes(obs, y)) + geom_point() + geom_line() + 
    # scale_y_continuous(trans=log_trans(), labels = comma_format()) +
    scale_y_continuous(labels = comma_format()) +
    coord_cartesian(xlim = range(cc$obs)) +
    labs(
      x = element_blank(),
      y = p1y
    ) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  p2 <- ggplot(cc[!is.na(y_rate)], aes(obs, y_rate)) + geom_point() + geom_line() + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0, NA)) +
    coord_cartesian(xlim = range(cc$obs))  +
    labs(
      x = "Date",
      y = p2y
    )
  
  g1 <- ggarrange(p1, p2, ncol = 1, top = sprintf(ptitle, location))
  ggsave(filename = sprintf('%s_%s.png', fname_prefix, location), 
         plot = g1, 
         path = picture_path,
         width = 13, height = 8.61)
}

####################################################################################
#
# Deaths and death rates
#
####################################################################################

p1y <- "Number of deaths"
p2y <- "Mortality growth rate"
ptitle <- "Death toll and mortality growth rate for %s"
fname_prefix <- 'mortality'
pic_dir <- 'mortality_rates'
d_path <- file.path(picture_path, pic_dir)

growth_plots('South Korea', d_rates, d_path, p1y, p2y, ptitle, fname_prefix)
map(unique(d_rates$region), growth_plots, d_rates, d_path, p1y, p2y, ptitle, fname_prefix)
  
####################################################################################
#
# Confirmed cases and case growth rates
#
####################################################################################

p1y <- "Number of cases"
p2y <- "Case growth rate"
ptitle <- "Number of cases and case growth rate for %s"
fname_prefix <- 'cases'
pic_dir <- 'case_rates'
c_path <- file.path(picture_path, pic_dir)

growth_plots('South Korea', c_rates, c_path, p1y, p2y, ptitle, fname_prefix)

map(unique(c_rates$region), growth_plots, c_rates, c_path, p1y, p2y, ptitle, fname_prefix)

