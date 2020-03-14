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

mortality_rate <- 0.056  # https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30195-X/fulltext
hubei_pop <- 58.5e6
italy_pop <- 60.5e6
days_onset2death <- 21

cv_hubei <- cov_sum[, .(region = province, obs, deaths)]

##################################################################################################
#
# Finally check the population disease burden rate for Wuhan at the time of the measures.
#
##################################################################################################

mr_lo <- 0.015  # Lower bound on mortality rate
mr_hi <- 0.16   # Upper bound on mortality rate

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


