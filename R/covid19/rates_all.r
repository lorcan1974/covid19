#
# Want a way to compare all regions irrespective of population.
# The case and death growth rates are important indicators.
#

source('setup.r')
source('read_jhu.r')

picture_path <- file.path('pictures', 'case_rates')

rate_fun <- function(x) {
  if(any(x==0)) {return(NA_real_)}
  sl <- summary(lm(log(x)~seq_along(x)))
  if(sl$coefficients[2,1]==0) {
    0
    } else {
      if(sl$coefficients[2,4]<0.05) {sl$coefficients[2,1]} else {NA_real_}
  }
}

roll_rate <- function(xx, k = 5) {
  r = rollapply(xx, 5, rate_fun, align='right', fill = NA)
  # Return only values after the last NA. The bit prior to this is not truly exponential
  if(any(is.na(r))){
    last_na <- which.max(is.na(r)*seq_along(r))
    r[1:last_na] <- NA_real_
  }
  r
}

last_dt <- jhu_gl[, max(obs)]
first_dt <- last_dt - days(45)

latest_country <- jhu_gl[province=='' & obs>=first_dt][, region:=country]
latest_province <- jhu_gl[province!='' & obs>=first_dt][, region:=province]

region_filt <- c('Austria', 'Belgium', 'Denmark', 'Finland', 'France', 'Germany', 'Greece', 'Iceland', 'India', 'Ireland', 'Israel', 'Italy', 'Japan', 'Korea South', 'Luxembourg', 'Malaysia', 'Netherlands', 'New South Wales', 'New Zealand', 'Northern Territory', 'Norway', 'Pakistan', 'Philippines', 'Poland', 'Portugal', 'Russia', 'Singapore', 'Spain', 'Sweden', 'Switzerland', 'Thailand', 'Turkey', 'Ukraine', 'United Kingdom', 'Western Australia')
region_filt2 <- c('Austria', 'Denmark', 'France', 'Germany', 'Iceland', 'India', 'Ireland', 'Italy', 'Japan', 'Korea South', 'Malaysia', 'Netherlands', 'New Zealand', 'Norway', 'Portugal', 'Singapore', 'Spain', 'Sweden', 'Switzerland', 'Thailand', 'United Kingdom')
region_filt3 <- c('Austria', 'Denmark', 'France', 'Germany', 'Iceland', 'India', 'Ireland', 'Italy', 'Japan', 'Korea South', 'Malaysia', 'Netherlands', 'New Zealand', 'Norway', 'Portugal', 'Singapore', 'Spain', 'Sweden', 'Switzerland', 'Thailand', 'United Kingdom')
region_filt4 <- c('France','Germany','Iceland','India','Ireland','Italy','Korea South','Malaysia','Norway','Singapore','Spain','Switzerland','United Kingdom')
region_filt4 <- c('France','Germany','Ireland','Italy','Malaysia','Norway','Spain','Switzerland','United Kingdom')
region_filt5 <- c('Korea, South', 'Italy', 'Germany', 'United Kingdom', 'Ireland', 'Sweden', 'Spain', 'Netherlands')

region_eu <- c('Austria', 'Belgium', 'Denmark', 'Finland', 'France', 'Germany', 'Greece', 
               'Ireland', 'Italy', 'Luxembourg', 
               'Netherlands', 'Norway', 'Poland', 'Portugal', 'Spain', 
               'Sweden', 'Switzerland', 'United Kingdom')

region_asia <- c('India', 'Japan', 'Malaysia', 'Korea South', 'Singapore', 'Thailand')

latest <- rbind(
  latest_country[, .(obs, region, confirmed, deaths)],
  latest_province[, .(obs, region, confirmed, deaths)]
)[region %in% region_eu & deaths>=50][, c('crate','drate'):=
                                            list(roll_rate(confirmed), roll_rate(deaths), k = 5),
                                            by = 'region']

# This sort of argument to show that should only consider >100 deaths to avoid that
# transient spike in the death rate:
# latest[, mean(deaths, na.rm = T), by = cut(drate, breaks = 6)][order(V1, decreasing = T)]
# cut          V1
# 1: (0.0295,0.126] 4265.108844
# 2:  (0.126,0.222] 1280.636364
# 3:  (0.222,0.318]  495.677419
# 4:  (0.318,0.415]   99.456522
# 5:  (0.415,0.511]   83.200000
# 6:  (0.511,0.607]   13.857143
# 7:             NA    6.685792

eu_codes <- data.table(
  region = region_eu,
  typ = c('DE', 'FR', 'SC', 'SC', 'FR', 'DE', 'IT', 'IE', 'IT', 'FR',
          'DE', 'SC', 'FR', 'IT', 'IT', 'SC', 'DE', 'GB'),
  cc = c('AT', 'BE', 'DK', 'FI', 'FR', 'DE', 'GR', 'IE', 'IT', 'LU',
         'NL', 'NO', 'PO', 'PT', 'ES', 'SW', 'CH', 'GB')
)

latest <- merge(latest, eu_codes[, .(region, cc)], by = 'region')

drate_wide <- dcast(latest[deaths>=50, .(obs, cc, drate)], 
                    obs~cc, 
                    value.var = 'drate')

drate_xc <- cor(drate_wide[obs>=last_dt-days(21), -1], use = 'pairwise.complete')
drate_gr <- graph_from_adjacency_matrix(abs(drate_xc)>0.98,
                                        mode = 'un',
                                        diag = F)

coords <- layout_with_fr(drate_gr)
plot(drate_gr, layout = coords)

drate_grps <- groups(components(drate_gr))

drate_cliques <- max_cliques(drate_gr, min = 2)

p1 <- ggplot(data=NULL, mapping = aes(obs, drate, colour = cc)) +
  geom_point() + geom_line()

p1 %+% latest[!is.na(drate) & obs>=last_dt-days(21) & cc %in% drate_grps[[1]]]

p1 %+% latest[!is.na(drate) & obs>=last_dt-days(21) & !(cc %in% drate_grps[[1]])]

tcor <- map_dbl(drate_wide[obs>=last_dt-days(14), -1],
                function(x) {cor(x, seq_along(x), use = 'pairwise.complete')})

tcor <- tcor[order(tcor)]

last_2weeks <- latest[!is.na(drate) & obs>=last_dt-days(14)]

growth_fit <- last_2weeks[, (broom::tidy(lm(drate ~ obs))), by = 'cc'][term=='obs']

ggplot(growth_fit[p.value<0.05], aes(x = factor(cc, 
                                                levels = cc[order(estimate)],
                                                ordered = T),
                                     y = -estimate)) +
  geom_bar(stat = 'identity', fill = 'blue', alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .5)) +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "Daily decline in mortality rate over past 2 weeks"
  )

ggsave(filename = 'daily_delta_mr.png', path = picture_path)

c95 <- data.table(cc = names(tcor)[tcor< -0.95])
c85 <- data.table(cc = names(tcor)[tcor>= -0.95 & tcor < -0.85])
cX <- data.table(cc = names(tcor)[tcor>= -0.85])

p_2wk <- ggplot(mapping = aes(obs, drate, group = cc, colour = cc)) +
  geom_point() + geom_line() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5), limits = c(0, NA)) +
  labs(
    colour = "Country",
    x = element_blank(),
    y = "Mortality growth rate"
  )

p_2wk %+% merge(last_2weeks, c95, by = 'cc') + labs(title = "Solid decline")
ggsave(filename = 'c95_2wk_mr.png', path = picture_path)

p_2wk %+% merge(last_2weeks, c85, by = 'cc') + labs(title = "Rates less reliable")
ggsave(filename = 'c85_2wk_mr.png', path = picture_path)

p_2wk %+% merge(last_2weeks, cX, by = 'cc') + labs(title = "Rates appear unreliable")
ggsave(filename = 'cX_2wk_mr.png', path = picture_path)


# [, .SD[all((deaths*confirmed)>0)], by = 'region']

irl_steps <- data.table(
  obs = as.Date(c('2020-03-12', '2020-03-24', '2020-03-27')),
  region = rep('Ireland', 3),
  txt = c('1', '2', '3')
  )



latest_mit <- merge(latest, eu_codes, by = c('region'), all.x = T)

latest_mit[!is.na(drate)] %>% 
  melt(measure.vars = c('crate', 'drate'), variable.name = 'key', value.name = 'rate') %>%
  `[`(!is.na(rate) & key=='drate') %>%
  {
    ggplot(., aes(obs, rate, group = region, colour = typ)) + 
      geom_line(size = 1) + geom_point() + 
      # geom_line(data = .[region=='Ireland'], size=1) +
      # geom_label(data = .[!is.na(txt)], mapping = aes(label = txt), colour = 'black', alpha = 0.5) +
      geom_text(data = .[obs==last_dt], 
                mapping = aes(x=obs+days(1), label = cc), 
                colour = 'black') + 
      scale_y_continuous(labels=scales::percent_format(5), limits = c(0, NA)) +
      scale_color_brewer(palette = 'Spectral') + 
      labs(
        x = "Date",
        y = "Growth rate",
        colour = "Region",
        linetype = "Rate",
        size = element_blank()
      )
}

# Google community data. Copied in by hand for March 29.
# The first reports, covering 131 countries and regions, include a graph that takes 
# Sunday, February 16, as the baseline for normal activity and tracks changes in 
# movements through to Sunday, March 29.

gcd_tr <- data.table(
  k = c('Retail', 'Grocery', 'Parks', 'Transit', 'Work', 'Home'),
  at = c(-0.87, -0.64, -.43, -.71, -.51, .14),
  be = c(-.84, -.53, -.66, -.76, -.52, .17),
  dk = c(-.37, -.22, .35, -.6, -.28, .09),
  fi = c(-.52, -.21, .48, -.59, -.25, .09),
  fr = c(-.88, -.72, -.82, -.87, -.56, .18),
  de = c(-.77, -.51, -.49, -.68, -.39, .11),
  gr = c(-.8, -.41, -.55, -.76, -.53, .2),
  ie = c(-.83, -.37, -.59, -.78, -.52, .19),
  it = c(-.94, -.85, -.9, -.87, -.63, .24),
  lu = c(-.9, -.7, -.66, -.78, -.6, .23),
  nl = c(-.65, -.29, -.3, -.68, -.35, .11),
  no = c(-.65, -.32, -.05, -.57, -.34, .11),
  pl = c(-.78, -.59, -.59, -.71, -.36, .13),
  pt = c(-.83, -.59, -.8, -.78, -.53, .22),
  es = c(-.94, -.76, -.89, -.88, -.64, .22),
  se = c(-.24, .1, .43, -.36, -.18, .05),
  ch = c(-.81, -.51, -.41, -.68, -.46, .15),
  gb = c(-.85, -.46, -.52, -.75, -.55, .15)
)

cc_map <- data.table(
  region = region_eu,
  cc = c('at', 'be', 'dk', 'fi', 'fr', 'de', 'gr', 'ie', 'it', 'lu', 'nl', 'no', 'pl',
         'pt', 'es', 'se', 'ch', 'gb')
)

eu_nordic <- c('se', 'fi', 'dk', 'de', 'no', 'nl', 'ch', 'at')

gcd <- gcd_tr %>%
  melt(id.vars = 'k', variable.name = 'cc') %>%
  dcast(cc ~ k) %>%
  merge(cc_map, by = 'cc') %>%
  merge(data.table(
    cc = eu_nordic,
    is_nordic = TRUE
  ), by = 'cc', all.x = T)

gcd <- gcd[is.na(is_nordic), is_nordic:=FALSE]

gcd_dr <- merge(gcd, 
                latest_mit[obs==last_dt, .(region, drate)], 
                by = 'region')

gcd_ln <- gcd_dr %>%
  melt(
    id.vars = c('region', 'cc', 'drate', 'is_nordic'),
    variable.name = 'key'
  )

gcd_non_nordic <- gcd_dr[!(cc %in% eu_nordic)]
gcd_nordic <- gcd_dr[(cc %in% eu_nordic)]

cor(gcd_dr[, .(Grocery, Parks, Retail, Transit, Work, Home)])
gcd_dr[, 
       lapply(.SD, function(c) {cor(c, drate)}),
       .SDcols = gcd_tr$k,
       by = is_nordic]

broom::tidy(lm(drate ~ Home, data = gcd_dr[is_nordic==F]))
broom::tidy(lm(drate ~ Home, data = gcd_dr[is_nordic==T]))

ggplot(gcd_ln[key=='Home'], aes(value, drate, colour = is_nordic)) + 
  geom_text(aes(label = cc)) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2), limits = c(0, .3)) +
  geom_abline(intercept = 0.273, slope = -0.893, colour = 'red', alpha = 0.5, linetype = 'dashed') +
  geom_abline(intercept = 0.142, slope = -0.396, colour = 'blue', alpha = 0.5, linetype = 'dashed') +
  scale_colour_manual(values = c('red', 'blue')) +
  labs(
    x = "Percentage change from Baseline for Community 'Home'",
    y = "Mortality growth rate",
    title = "Google community data vs Covid-19 mortality growth rate"
  ) +
  theme(legend.position = 'none')

lm_gcd_non_nordic <- lm(drate ~ Home, data = gcd_dr[is_nordic==F])
summary(lm_gcd_non_nordic)

lm_gcd_nordic <- lm(drate ~ Home, data = gcd_dr[is_nordic==T])
summary(lm_gcd_nordic)



gcd_ss$fit <- fitted.values(lm_gcd)

ggplot(merge(gcd_dr[, .(cc, Grocery, drate)], gcd_ss[, .(cc, fit)], by = 'cc', all.x = T)) + 
  geom_text(aes(Grocery, drate, label = toupper(cc)), data = gcd_dr[is.na(fit)], colour = 'black') +
  geom_text(aes(Grocery, drate, label = toupper(cc)), data = gcd_dr[!is.na(fit)], colour = 'blue') +
  geom_text(aes(Grocery, fit, label = toupper(cc)), colour = 'red') +
  # geom_abline(slope = 1, intercept = 0, colour = 'blue') + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 20), limits = c(-1, NA)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2), limits = c(0, NA)) +
  labs(
    x = "Grocery baseline",
    y = "Latest death rate",
    title = "Death rate fitted to Google Grocery baseline",
    subtitle = "Actual rate (black); Predicted (red)"
  )

gcd_cm <- merge(gcd_dr[, .(cc, Grocery, drate)], gcd_ss[, .(cc, fit)], by = 'cc', all.x = T)
ggplot() + 
  geom_text(aes(Grocery, drate, label = toupper(cc)), data = gcd_cm[is.na(fit)], colour = 'black') +
  geom_text(aes(Grocery, drate, label = toupper(cc)), data = gcd_ss, colour = 'blue') +
  geom_text(aes(Grocery, fit, label = toupper(cc)), data = gcd_ss, colour = 'red') +
  # geom_abline(slope = 1, intercept = 0, colour = 'blue') + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 20), limits = c(-1, NA)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2), limits = c(0, NA)) +
  labs(
    x = "Grocery baseline",
    y = "Latest death rate",
    title = "Death rate fitted to Google Grocery baseline",
    subtitle = "Actual rate (black); Predicted (red)"
  )

ggplot(gcd_dr, aes(Grocery, drate)) + 
  geom_text(aes(label = toupper(cc))) +
  # geom_abline(slope = 1, intercept = 0, colour = 'blue') + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 5), limits = c(-1, NA)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2), limits = c(0, NA)) +
  labs(
    x = "Grocery baseline",
    y = "Latest death rate"
  )



stop("Not below here")

latest_mit[deaths>10, deaths_scaled:=deaths*mean(confirmed)/mean(deaths), by = 'region'] %>%
  melt(measure.vars = c('confirmed', 'deaths_scaled'), variable.name = 'key', value.name = 'count') %>%
  `[`(!is.na(count) & count>10) %>% {
    ggplot(., aes(obs, count, colour = region, linetype = key)) + 
    geom_line() + geom_point() + 
    geom_line(data = .[region=='Ireland'], size=1) +
    geom_label(data = .[!is.na(txt)], mapping = aes(label = txt), colour = 'black', alpha = 0.5) +
    scale_y_log10() +
    labs(
      x = "Date",
      y = element_blank(),
      colour = "Region",
      linetype = "Count",
      size = element_blank()
    ) +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank())
  }

latest[obs>last_dt-days(7) & !is.na(drate) & region %in% region_filt4] %>% p1

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

latest_crate <- latest[, 
                      .(r = rate_fun(.SD[order(obs), confirmed]), 
                        pval = pval_fun(.SD[order(obs), confirmed])),
                      by = c('region')][, rate:=ifelse(pval<0.05, r, NA_real_)]

ggplot(latest_crate[!is.na(rate)], aes(rate)) + geom_histogram(binwidth = 0.01, boundary = 0)

latest_drate <- latest[, 
                       .(r = rate_fun(.SD[order(obs), deaths]), 
                         pval = pval_fun(.SD[order(obs), deaths])),
                       by = c('region')][, rate:=ifelse(pval<0.05, r, NA_real_)]

ggplot(latest_drate[!is.na(rate)], aes(rate)) + geom_histogram(binwidth = 0.01, boundary = 0)


p_rate <- ggplot(mapping = aes(x = factor(region, levels = region[order(rate)], ordered = T), y = rate)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(~cut(rate, breaks = quantile(rate), 
                  include.lowest = T, 
                  labels = c('LwrQ', 'IQ1', 'IQ2', 'UprQ')), 
             scales = 'free') +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  labs(x = element_blank()) +
  coord_flip()

p_rate %+% latest_crate[!is.na(rate)] + labs(y = "Rate of increase in confirmed cases")
p_rate %+% latest_drate[!is.na(rate)] + labs(y = "Rate of increase in mortalities")


