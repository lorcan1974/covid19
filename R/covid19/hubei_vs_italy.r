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

dd <- function(x) {rollmean(diff(c(0, x)), k=3, fill=0)}

cv_hubei <- cov_sum[province=='Hubei'][order(obs)][,  
                    .(
                      region = province,
                      obs=as.Date(obs), 
                      confirmed = dd(confirmed), 
                      deaths = dd(deaths)
                      )
                    ]

cv_italy <- cov_sum[country=='Italy'][order(obs)][,  
                    .(
                      region = country,
                      obs=as.Date(obs), 
                      confirmed = dd(confirmed), 
                      deaths = dd(deaths)
                    )
                    ]

cv_all <- rbindlist(list(cv_hubei, cv_italy))

cv_all_case <- cv_all[, .(region, obs=obs-days(days_onset2death), onsets = deaths)]

cv_all_merge <- merge(cv_all, cv_all_case, all=T, by = c('region', 'obs'))

cv_all_est <- cv_all_merge %>%
  melt(
    id.vars = c('region', 'obs'), 
    measure.vars = c('confirmed', 'deaths', 'onsets'), 
    variable.names = 'key', 
    value.name = 'count', 
    na.rm=T
    ) 

##################################################################################################
#
# Hubei Province
#
##################################################################################################

p1 <- ggplot(cv_all_est[count>=1 & region=='Hubei'], 
       aes(obs, count, colour = variable)) + 
  geom_point() + geom_line() + scale_y_log10() +
  labs(
    x = "Date",
    y = "Cases per day**",
    colour = "Key"
  )

d0 <- cv_all_est[count>=1 & region=='Hubei', min(obs)]
d1 <- as.Date('2020-01-23')
d2 <- as.Date('2020-02-13')
d3 <- as.Date('2020-02-20')
d4 <- cv_all_est[count>=1 & region=='Hubei', max(obs)]

dl1 <- as.Date('2020-01-10')
dl2 <- as.Date('2020-01-28')
dl3 <- as.Date('2020-02-18')
dl4 <- as.Date('2020-03-01')

wuhan_notes <- list(
  annotate('rect', xmin=d0, xmax=d1, ymin=6, ymax=110, alpha=0.2, fill = 'deepskyblue4'),
  annotate('rect', xmin=d1, xmax=d2, ymin=6, ymax=9000, alpha=0.2, fill = 'orangered1'),
  annotate('rect', xmin=d2, xmax=d3, ymin=6, ymax=9000, alpha=0.2, fill = 'darkslategray4'),
  annotate('rect', xmin=d3, xmax=d4, ymin=6, ymax=1000, alpha=0.2, fill = 'wheat4'),
  geom_vline(xintercept = d1, colour = 'purple4'),
  geom_vline(xintercept = d2, colour = 'purple4'), 
  geom_vline(xintercept = d3, colour = 'purple4'),
  geom_label(aes(x = d1, y = 8000, label = "Wuhan lockdown"), colour = 'black'),
  geom_label(aes(x = d2, y = 300, label = "Non-essential\nshutdown"), colour = 'black'),
  geom_label(aes(x = d3, y = 10, label = "School\nshutdown"), colour = 'black'),
  geom_label(aes(x = dl1, y = 3000, label = '1'), colour = 'blue', fill = 'gray'),
  geom_label(aes(x = dl2, y = 3000, label = '2'), colour = 'blue', fill = 'gray'),
  geom_label(aes(x = dl3, y = 3000, label = '3'), colour = 'blue', fill = 'gray'),
  geom_label(aes(x = dl4, y = 3000, label = '4'), colour = 'blue', fill = 'gray')
  )

p1 + wuhan_notes +
  labs(title = "Confirmed cases, deaths and estimate of fatal case onset for Hubei province")

ggsave(filename = 'hubei.png')

##################################################################################################
#
# Italy
#
##################################################################################################

d0 <- cv_all_est[count>=1 & region=='Italy', min(obs)]
d1 <- as.Date('2020-02-21')
d2 <- as.Date('2020-03-08')
d3 <- as.Date('2020-03-09')
d4 <- cv_all_est[count>=1 & region=='Italy', max(obs)]

dl1 <- as.Date('2020-01-10')
dl2 <- as.Date('2020-01-28')

p1 %+% cv_all_est[count>=1 & region=='Italy'] +
  annotate('rect', xmin=d0, xmax=d1, ymin=1, ymax=300, alpha=0.2, fill = 'deepskyblue4') +
  annotate('rect', xmin=d1, xmax=d4, ymin=1, ymax=1800, alpha=0.2, fill = 'orangered1') +
  geom_vline(xintercept = d1, colour = 'blue') +
  geom_vline(xintercept = d2, colour = 'blue') + 
  geom_vline(xintercept = d3, colour = 'blue') +
  geom_label(aes(x = d1, y = 600, label = "Lodi lockdown"), colour = 'black') +
  geom_label(aes(x = d2, y = 10, label = "North\nquarantined"), colour = 'black') +
  geom_label(aes(x = d3, y = 3, label = "Quarantine\nextended"), colour = 'black') +
  labs(title = "Confirmed cases, deaths and estimate of fatal case onset for Italy")

ggsave(filename = 'italy.png')

