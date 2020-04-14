# The reported case rates for all their flaws are the most up to date information we have
# This file is an effort to come up with some definitive way of looking at the daily figures.
# Trace of case reports 

picture_path <- 'pictures'

hc_c <- cv_with_pop[confirmed>50, .(.N, x = max(confirmed)), by = 'code'][order(x, decreasing = T)][x>50 & N>5]
hp_c <- cv_with_pop[confirmed>50, .(.N, x = max(confirmed)), by = 'province'][order(x, decreasing = T)][x>50 & N>5]

countries_by_province <- c('USA', 'CHN', 'AUS', 'CAN')

high_countries_c <- setdiff(hc_c$code, countries_by_province)
high_provinces_c <- c('Hubei', 'New York', 'Washington')

high_rates <- rbind(
  cv_with_pop[code %in% high_countries_c, .(region=code, obs, confirmed, deaths, pop)],
  cv_with_pop[province %in% high_provinces_c, .(region=province, obs, confirmed, deaths, pop)]
)

rates_pm <- high_rates[, c('confirmed_pm', 'deaths_pm'):=list(confirmed/pop, deaths/pop)]


c_rates <- rbind(
  cv_with_pop[code %in% high_countries_c, .(region=code, obs, confirmed, pop)],
  cv_with_pop[province %in% high_provinces, .(region=province, obs, confirmed, pop)]
)[
  order(obs), delta_confirmed:=rollmean(c(NA, diff(confirmed)), k=5, fill=NA) , by = 'region'
  ][, delta_confirmed_pm:=delta_confirmed/pop]


ggplot(c_rates[!is.na(delta_confirmed_pm) & region %in% c('GBR', 'IRL')], 
       aes(obs, confirmed, colour = region)) + geom_line() +
  #scale_y_log10() +
  labs(
    x = "Date",
    y = "Confirmed cases per day",
    title = "Daily confirmed cases",
    colour = "Region"
  )


last_dt <- rates_pm[, max(obs)]
latest_figures <- rates_pm[obs==last_dt]

ggplot(latest_figures, aes(x = factor(region, region[order(confirmed_pm)], ordered = T),
                           y = confirmed_pm)) + 
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.02), breaks = seq(0, 1.2e-3, by = 2e-4)) +
  labs(
    x = element_blank(),
    y = "Reported cases as a percentage of the population",
    title = sprintf("Reported cases percentage on %s", last_dt)
  ) +
  theme(plot.margin = unit(c(.5, 1, .5, .5), 'cm')) +
  coord_flip()

ggsave(filename = 'case_percentages.png', path = file.path(picture_path, 'case_rates'))

ggplot(latest_figures, aes(y, y_rate, label = region)) + geom_text() + scale_x_log10() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  geom_hline(yintercept = 0.05, colour = 'red', size = 1) + 
  labs(
    x = "Number of cases",
    y = "Latest case growth rate",
    title = "Cases vs case growth rate"
  )



reg_lin <- c('IRL', 'PRT', 'ISR', 'GBR', 'AUT', 'NLD', 'CHE', 'FRA', 'ESP', 'ITA')

summary(
  lm(log(confirmed)~obs, data = c_rates[region %in% c('IRL') & obs>=last_dt-days(5)])
)



ggplot(latest_figures, aes(x = factor(region, region[order(y_rate)], ordered = T),
                           y = y_rate)) + 
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  geom_hline(yintercept = 0.05, colour = 'red', size = 1) + 
  labs(
    x = "Region",
    y = "Daily growth rate in cases",
    title = "Latest Case growth rate"
  ) +
  coord_flip()


