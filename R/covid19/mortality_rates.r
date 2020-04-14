#
# Crude estimate of mortality rates by region: [number of deaths] / [number of confirmed cases]
# Column plot shows how this varies across territory.
#
# NOTES:
# Here I have fitted a line to the daily reports of mortalities vs confirmed cases. I’ve included only those countries/regions reporting a minimum of 30 deaths with at least 10 deaths reported on at least 5 days. From this you can work out a mortality rate per confirmed case, the ratio [number of deaths] / [number of confirmed cases] expressed as a percentage. 
# 
# There is a broad spread in these ratios. My initial interpretation of what I am seeing:
#   - The mortality rate can be higher than average because the health system is overwhelmed and unfortunately people who would normally survive do not.
# - The mortality rate can appear to be higher than average because the testing regime is inadequate: mild cases go unreported and/or cases are not being reported in a timely fashion.
# - The death rate will be playing catch-up with the confirmed case rates and so unfortunately all of these rates are likely to rise further.
# 
# Whichever way you look at it it looks as though Germany, New York, South Korea and Switzerland are out in front in terms of having a pro-active response. And it’s worth pointing to the fact that New York seems to be doing a much better job than Washington. Can anyone suggest why this might be? Thanks.

picture_path <- 'pictures'

hc_d <- cv_with_pop[deaths>10, .(.N, x = max(deaths)), by = 'code'][order(x, decreasing = T)][x>30 & N>5]
hp_d <- cv_with_pop[deaths>10, .(.N, x = max(deaths)), by = 'province'][order(x, decreasing = T)][x>30 & N>5]

countries_by_province <- c('USA', 'CHN', 'AUS', 'CAN')

high_countries_d <- setdiff(hc_d$code, countries_by_province)
high_provinces_d <- c('Hubei', 'New York', 'Washington')

high_rates <- rbind(
  cv_with_pop[code %in% high_countries_d, .(region=code, obs, confirmed, deaths, pop)],
  cv_with_pop[province %in% high_provinces_d, .(region=province, obs, confirmed, deaths, pop)]
)

rates_pm <- high_rates[, c('confirmed_pm', 'deaths_pm'):=list(confirmed/pop, deaths/pop)]

ggplot(rates_pm[order(obs)], aes(confirmed_pm, deaths_pm, colour = region)) + geom_line() +
  #scale_x_log10() + scale_y_log10() +
  labs(
    x = "Confirmed cases per million",
    y = "Deaths per million",
    title = "Deaths vs confirmed cases per million people",
    colour = "Region"
  )

lm1 <- lm(log_d~log_c, data = rates_pm[, .(log_c = (confirmed_pm), log_d = (deaths_pm))])
summary(lm1)

region_lst <- sort(unique(rates_pm$region))
xx <- map(region_lst, function(r) {broom::tidy(lm(deaths~confirmed, data = rates_pm[region==r]))})
names(xx) <- region_lst

mortality_tbl <- map_dfr(region_lst, function(r) {
  as.data.table(xx[[r]])[, .(term, estimate, pval = p.value)][, region:=r]
  }
)

ggplot(mortality_tbl[term=='confirmed'],
       aes(x = factor(region, levels = region[order(estimate)], ordered = T),
           y = estimate)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Region",
    y = "Mortality as a percentage of Confirmed cases"
  ) + coord_flip()

ggsave('mortality_tbl.png', path = file.path(picture_path, 'mortality_rates'))

