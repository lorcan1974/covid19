##################################################################################################
#
#
# https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30195-X/fulltext
# CI 95% (3.5-3.7) is the raw figure for China
# The Lancet re-estimated this at 5.6% (95% CI 5.4-5.8%) (But still a great degress of uncertainty)
#
# Wuhan lockdown data from Wikipedia:
# https://en.wikipedia.org/wiki/2020_Hubei_lockdowns
#
##################################################################################################

library(broom)

picture_path <- 'pictures'

##################################################################################################
#
# Extract the last 5 death counts for each country and each province.
#
##################################################################################################
cv_estimates <- cov_sum[deaths>0]

tail_by_country <- cv_estimates[province==''][order(obs), c(tail(.SD, n = 5)), by = 'country'][
  , c(.SD, data.table(day = .SD[, obs-min(obs)])), by = 'country']

tail_by_province <- cv_estimates[province!=''][order(obs), c(tail(.SD, n = 5)), by = 'province'][
  , c(.SD, data.table(day = .SD[, obs-min(obs)])), by = 'province']

lm_summary <- function(data_set) {
  broom::tidy(summary(lm(log2(deaths)~day, data = data_set)))
}

lm_summary_conf <- function(data_set) {
  broom::tidy(summary(lm(log2(confirmed)~day, data = data_set)))
}

##################################################################################################
#
# Stack province and country and fit an LM keeping all regions with a p value < 0.05.
# These are what I'm saying are in the 'exponential phase'.
#
##################################################################################################
tail_by_region <- rbind(tail_by_country[, .(region = country, obs, confirmed, deaths, day)],
                        tail_by_province[, .(region = province, obs, confirmed, deaths, day)]
                        )

lm_region_dt <- rbind(tail_by_region[, lm_summary(.SD), by = 'region'])[, .SD[all(p.value<0.05)], by = 'region']
lm_region_confirmed_dt <- rbind(tail_by_region[, lm_summary_conf(.SD), by = 'region'])[, .SD[all(p.value<0.05)], by = 'region']

#
# Look for variations in slope of confirmed cases vs death rates. Significant differences indicate the testing is not being carried out 
# properly. Though would have to look at lagged values I think.
#
merge(
  lm_region_dt[term=='day', .(region, d_est = estimate, d_se = std.error)], 
  lm_region_confirmed_dt[term=='day', .(region, c_est = estimate, c_se = std.error)], 
  by = c('region'))

exponential_regions <- unique(lm_region_dt$region)

##################################################################################################
#
# Rebuild these LMs, capturing them all in a named list.
#
##################################################################################################
lm_fit <- function(r) {
  lm(log2(deaths)~obs, data = tail_by_region[region==r][order(obs)])
}

lm_exp_lst <- map(exponential_regions, lm_fit)
names(lm_exp_lst) <- exponential_regions

##################################################################################################
#
# Line up the actual death tolls.
#
##################################################################################################

exp_regions <- rbind(cv_estimates[country %in% exponential_regions, .(obs, region = country, deaths)],
                     cv_estimates[province %in% exponential_regions, .(obs, region = province, deaths)]
                     )

##################################################################################################
#
# Fix the dates and make the region by region predictions using the list of Linear Models.
#
##################################################################################################

d1 <- max(exp_regions[, obs]) + days(1)
d2 <- d1 + days(5)
prediction_dates <- seq(d1, d2, by = 1)

predictions <- map_dfr(exponential_regions,
                          function(region) {
                            data.table(
                              obs = prediction_dates,
                              region = region,
                              deaths = 2^predict(lm_exp_lst[[region]], newdata = data.table(obs = prediction_dates))
                            )
                          })

##################################################################################################
#
# Stack the predictions onto the actuals.
#
##################################################################################################

exp_regions_with_preds <- rbind(cv_estimates[country %in% exponential_regions, .(obs, region = country, deaths)],
                                cv_estimates[province %in% exponential_regions, .(obs, region = province, deaths)],
                                predictions)

##################################################################################################
#
# There's too much for one plot - so split by high and low. I fiddled about with the threshold to achieve an even split.
#
##################################################################################################

batch_threshold <- exp_regions_with_preds[obs==max(obs), median(deaths)]
exp_regions_with_preds <- exp_regions_with_preds[, batch:=ifelse(max(deaths)>batch_threshold, 'High', 'Low'), by = 'region'][obs>d0]

p1 <- ggplot(mapping = aes(obs, deaths, colour=region)) + 
  geom_point() + geom_line() +
  scale_y_log10() +
  labs(
    x = "Date",
    y = "Death toll",
    title = "Death toll in regions with Coronavirus spreading",
    colour = "Region"
  ) +
  geom_vline(xintercept = d1, colour = 'purple4')

##################################################################################################
#
# High region plot.
#
##################################################################################################

spans <- exp_regions_with_preds[batch=='High', .(d0 = min(obs), ymin = min(deaths), ymax = max(deaths))]
high_notes <- list(
  annotate('rect', xmin=spans$d0, xmax=d1, ymin=spans$ymin, ymax=spans$ymax, alpha=0.2, fill = 'deepskyblue4'),
  annotate('rect', xmin=d1, xmax=d2, ymin=spans$ymin, ymax=spans$ymax, alpha=0.2, fill = 'orangered1'),
  geom_label(aes(x = as.Date('2020-02-27'), y = 1000, label = "Actual toll"), colour = 'black'),
  geom_label(aes(x = as.Date('2020-03-19'), y = 3, label = "Prediction"), colour = 'black')
)

p1 %+% exp_regions_with_preds[batch=='High'] + high_notes
ggsave(file.path(picture_path, 'exp_regions_high.png'))

##################################################################################################
#
# Low region plot.
#
##################################################################################################

spans <- exp_regions_with_preds[batch=='Low', .(d0 = min(obs), ymin = min(deaths), ymax = max(deaths))]
low_notes <- list(
  annotate('rect', xmin=spans$d0, xmax=d1, ymin=spans$ymin, ymax=spans$ymax, alpha=0.2, fill = 'deepskyblue4'),
  annotate('rect', xmin=d1, xmax=d2, ymin=spans$ymin, ymax=spans$ymax, alpha=0.2, fill = 'orangered1'),
  geom_label(aes(x = as.Date('2020-03-05'), y = 15, label = "Actual toll"), colour = 'black'),
  geom_label(aes(x = as.Date('2020-03-19'), y = 3, label = "Prediction"), colour = 'black')
)

p1 %+% exp_regions_with_preds[!(region %in% high_regions) & obs >= d0] + low_notes
ggsave(file.path(picture_path, 'exp_regions_low.png'))


##################################################################################################
#
#
# Next: use the computed exponential rate to estimate the actual number of cases in the population in Ireland.
#
#
##################################################################################################

##################################################################################################
#
# I'm interested specifically in Ireland - so will take the EU rate average.
#
##################################################################################################

exp_rates <- lm_region_dt[term=='day'][, doubling_time:=1/estimate]
exp_rates[order(doubling_time)][, .(region, 
                                    estimate, 
                                    doubling_time=round(doubling_time, digits = 1), 
                                    irl_proj=round(300*(1+estimate)^15))][, .(region, estimate, irl_proj, m=irl_proj*.035)]

exp_rates[order(doubling_time)][region %in% c('Spain', 'Switzerland', 'Germany', 'Netherlands', 'Italy'), 
                                .(region, estimate, doubling_time=round(doubling_time, digits = 1))]

eu_avg_rate <- exp_rates[region %in% c('Spain', 'Switzerland', 'Germany', 'Netherlands', 'Italy'), mean(estimate)]


mr_lo <- 0.015  # Lower bound on mortality rate
mr_med <- 0.056  # https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30195-X/fulltext
mr_hi <- 0.16   # Upper bound on mortality rate

mr <- 0.035
hubei_pop <- 58.5e6
italy_pop <- 60.5e6

days_onset2death <- 21

irl <- cv_with_pop[code=='IRL', .(obs, confirmed, deaths, pop)]

d1 <- as.Date('2020-03-02')
d2 <- as.Date('2020-03-06')
d3 <- irl[, max(obs)]

lm_early <- lm(log2(confirmed)~obs, data = irl[obs>=d1 & obs<=d2])
summary(lm_early)

lm_late <- lm(log2(confirmed)~obs, data = irl[obs>d2 & obs<=d3])
summary(lm_late)

irl_with_fit <- irl[, .(obs, confirmed, early=NA_real_, late=NA_real_)]
irl_with_fit[obs>=d1 & obs<=d2, early:=2^predict(lm_early)]
irl_with_fit[obs>=d2 & obs<=d3, late:=2^predict(lm_late)]

irl_tall <- irl_with_fit %>%
  melt(id.vars = 'obs', variable.name = 'key', value.name = 'test_count')

ggplot(irl_tall[test_count>0 & key=='confirmed'], aes(obs, test_count, colour = key)) + 
  geom_point() + geom_line() + geom_line(data = irl_tall[test_count>0 & key!='confirmed'], size=1) +
  scale_y_log10() +
  annotate('rect', xmin=d1, xmax=d2, ymin=1, ymax=max(irl_tall$test_count, na.rm=T), alpha=0.2, fill = 'deepskyblue4') +
  annotate('rect', xmin=d2, xmax=d3, ymin=1, ymax=max(irl_tall$test_count, na.rm=T), alpha=0.2, fill = 'orangered1') +
  geom_label(aes(x = as.Date('2020-03-04'), y = 30, label = "Outbreak -\nFirst wave"), colour = 'black') +
  geom_label(aes(x = as.Date('2020-03-12'), y = 10, label = "Outbreak is\noverwhelming\ntesting capacity"), colour = 'black') +
  labs(
    x = "Date",
    y = "Tests reported",
    colour = "Key",
    title = "HSE test counts show two distinct rates"
  )

ggsave(file.path(picture_path, 'hse_tests.png'))



# d0 <- as.Date('2020-03-14') - days(days_onset2death)
d0 <- as.Date('2020-02-28')
d1 <- irl[, max(obs)]
d2 <- d1 + days(7)
d3 <- d1 + days(14)

lm_irl <- lm(log2(confirmed)~obs, data = irl[obs>=as.Date('2020-03-02')]) 
summary(lm_irl)
2^(-9.208e3 + 5.026e-1 * as.numeric(d0))

d0_num_cases <- 2^predict(lm_irl, newdata = data.table(obs = d0))
d0_num_cases <- c(2)

irl_est <- data.table(obs = seq(d0, d1, by = 1))[, 
                                                 mult:=2^cumsum(c(0, rep(eu_avg_rate, times = .N-1)))
                                                 ][, 
                                                   c('estimate'):=lapply(d0_num_cases, `*`, mult)
                                                   ]

irl_with_est <- merge(irl[, .(obs, confirmed, deaths)], irl_est[, .(obs, estimate)], by = 'obs', all = T)

irl_stack <- irl_with_est %>% melt(id.vars = c('obs'), variable.name = 'key', value.name = 'count')

ggplot(irl_stack[count>0], aes(obs, count, colour = key)) + geom_line() + geom_point() +
  # scale_y_log10() +
  labs(
    x = "Date",
    y = "Count",
    title = "Estimated outbreak in Ireland",
    colour = "Key"
  )

ggsave(file.path(picture_path, 'irl_outbreak_estimate.png'))
  

irl_mortality <- irl_est[, c(list(obs=obs+days(days_onset2death), mortality=estimate*mr+max(irl$deaths)))]
irl_with_est <- merge(irl, irl_est[, mult:=NULL], by = 'obs', all = T) %>% merge(irl_mortality, by = 'obs', all = T)

#
# Take a quick look at another country...can piece these together tomorrow if I have something.
#
region_dt <- cv_country[code=='ITA', .(country, obs, outcome, count, pop)]
ggplot(region_dt[count>0], aes(obs, count, colour = outcome)) + scale_y_log10() + geom_point() + geom_line()

d0 <- as.Date('2020-02-24') - days(days_onset2death-7)
# d0 <- as.Date('2020-03-02')
d1 <- region_dt[, max(obs)]
d2 <- d1 + days(7)
d3 <- d1 + days(14)

region_dt <- cv_country[code=='ITA', .(country, obs, outcome, count, pop)] %>%
  dcast(...~outcome, value.var = 'count')
lm_region_dt <- lm(log2(confirmed)~obs, data = region_dt[obs>=as.Date('2020-03-01')]) 
summary(lm_region_dt)

d0_num_cases <- 2^predict(lm_region_dt, newdata = data.table(obs = d0))
region_dt_est <- data.table(obs = seq(d0, d3, by = 1))[, mult:=2^cumsum(c(0, rep(eu_avg_rate, times = .N-1)))][, estimated:=d0_num_cases*mult]
region_dt_with_est <- merge(region_dt, region_dt_est[, .(obs, estimated)], by = 'obs', all = T)

region_dt_stack <- region_dt_with_est %>%
  melt(id.vars = c('obs', 'country', 'pop'), measure.vars = c('confirmed', 'estimated', 'deaths'), variable.name = 'key', value.name = 'count')

ggplot(region_dt_stack[count>0], aes(obs, count, colour = key)) + geom_line() + geom_point() +
  scale_y_log10() +
  labs(
    x = "Date",
    y = "Count",
    title = "Estimated spread of Covid-19 in Ireland over next 7 days"
  )



##################################################################################################
#
# Finally check the population disease burden rate for Wuhan at the time of the measures.
#
##################################################################################################


cv_case_estimates <- cv_all[, .(region, 
                                obs=obs-days(days_onset2death), 
                                cases_hi = deaths/mr_lo,
                                cases_lo = deaths/mr_hi)]

cv_burden_est <- merge(cv_all[, .(region, obs, deaths)], cv_case_estimates, by = c('obs', 'region'), all = T)[, pop:=ifelse(region=='Hubei', hubei_pop, italy_pop)] %>%
  melt(id.vars = c('obs', 'region', 'pop'), 
       measure.vars = c('deaths', 'cases_hi', 'cases_lo'),
       variable.name = 'measure',
       value.name = 'count') %>%
  `[`(T, rate:=count/pop)

wuhan_rate_notes <- list(
  annotate('rect', xmin=d0, xmax=d1, ymin=1e-7, ymax=3e-4, alpha=0.2, fill = 'deepskyblue4'),
  annotate('rect', xmin=d1, xmax=d2, ymin=1e-7, ymax=3e-4, alpha=0.2, fill = 'orangered1'),
  annotate('rect', xmin=d2, xmax=d3, ymin=1e-7, ymax=3e-4, alpha=0.2, fill = 'darkslategray4'),
  annotate('rect', xmin=d3, xmax=d4, ymin=1e-7, ymax=3e-4, alpha=0.2, fill = 'wheat4'),
  geom_vline(xintercept = d1, colour = 'purple4'),
  geom_vline(xintercept = d2, colour = 'purple4'), 
  geom_vline(xintercept = d3, colour = 'purple4')
)

wuhan_rate_labels <- list(
  geom_label(aes(x = d1, y = 3e-5, label = "Wuhan lockdown"), colour = 'black'),
  geom_label(aes(x = d2, y = 1e-5, label = "Non-essential\nshutdown"), colour = 'black'),
  geom_label(aes(x = d3, y = 3e-7, label = "School\nshutdown"), colour = 'black'),
  geom_label(aes(x = dl1, y = 2e-4, label = '1'), colour = 'blue', fill = 'gray'),
  geom_label(aes(x = dl2, y = 2e-4, label = '2'), colour = 'blue', fill = 'gray'),
  geom_label(aes(x = dl3, y = 2e-4, label = '3'), colour = 'blue', fill = 'gray'),
  geom_label(aes(x = dl4, y = 2e-4, label = '4'), colour = 'blue', fill = 'gray')
)

ggplot(cv_burden_est[rate>0 & region=='Hubei'], aes(obs, rate, colour = measure)) + 
  geom_point() + geom_line() + scale_y_log10() +
  wuhan_rate_notes +
  wuhan_rate_labels +
  labs(
    x = "Date",
    y = "Population rate",
    colour = "Key",
    title = "Actual death rates and high/low estimates of all case onsets for Hubei"
  )

ggsave(filename = 'hubei_rates.png')

##################################################################################################
#
# Finally check the population disease burden rate for Italy at the time of the measures.
#
##################################################################################################

italy_rate_notes <- list(
  annotate('rect', xmin=d0, xmax=d1, ymin=1e-8, ymax=3e-4, alpha=0.2, fill = 'deepskyblue4'),
  annotate('rect', xmin=d1, xmax=d4, ymin=1e-7, ymax=3e-4, alpha=0.2, fill = 'orangered1'),
  geom_vline(xintercept = d1, colour = 'purple4'),
  geom_vline(xintercept = d2, colour = 'purple4'), 
  geom_vline(xintercept = d3, colour = 'purple4')
)

italy_rate_labels <- list(
  geom_label(aes(x = d1, y = 1e-6, label = "Lodi lockdown"), colour = 'black'),
  geom_label(aes(x = d2, y = 3e-5, label = "North\nquarantined"), colour = 'black'),
  geom_label(aes(x = d3, y = 1e-7, label = "Quarantine\nextended"), colour = 'black')
)

ggplot(cv_burden_est[rate>0 & region=='Italy'], aes(obs, rate, colour = measure)) + 
  geom_point() + geom_line() + scale_y_log10() +
  italy_rate_notes + italy_rate_labels +
  labs(
    x = "Date",
    y = "Population rate",
    colour = "Key",
    title = "Actual death rates and high/low estimates of all case onsets for Italy"
  )

ggsave(filename = 'italy_rates.png')


