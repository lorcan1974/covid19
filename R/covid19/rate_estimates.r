library(scales)
library(egg)

picture_path <- 'pictures'

hc_d <- cv_with_pop[deaths>10, .(.N, x = max(deaths)), by = 'code'][order(x, decreasing = T)][x>50 & N>10]
hc_c <- cv_with_pop[confirmed>10, .(.N, x = max(confirmed)), by = 'code'][order(x, decreasing = T)][x>300 & N>10]
hp <- cv_with_pop[, .(.N, x = max(deaths)), by = 'province'][order(x, decreasing = T)][1:10]

high_countries_c <- c('ITA', 'ESP', 'DEU', 'FRA', 'KOR', 'CHE', 'GBR', 'NLD', 'AUT', 'BEL', 'DNK', 'MYS', 'IRL')
high_countries_d <- c('ITA', 'ESP', 'FRA', 'KOR')
high_provinces <- c('Hubei')

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

d_rates <- rbind(
  cv_with_pop[code %in% high_countries_d, .(region=country, obs, y = deaths)],
  cv_with_pop[province %in% high_provinces, .(region=province, obs, y = deaths)]
)[y>10][
  order(obs), y_rate:=smooth_rate(y, k=5) , by = 'region'
  ]

c_rates <- rbind(
  cv_with_pop[code %in% high_countries_c, .(region=country, obs, y = confirmed)],
  cv_with_pop[province %in% high_provinces, .(region=province, obs, y = confirmed)]
)[y>30][
  order(obs), y_rate:=smooth_rate(y, k=5) , by = 'region'
  ]

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

