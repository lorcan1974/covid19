source('read_data.r')

cov_line[, .(`reporting date`, symptom_onset, hosp_visit_date, exposure_start, exposure_end)]
covid_ds <- map(list.files(data_path, full.names = T), fread)
cov_sum <- fread(
  file.path(data_path, 'covid_19_data.csv')
  )[, ObservationDate:=parse_date_time(ObservationDate, orders = c('m/d/y', 'm/d/Y'))]

names(cov_sum) <- c('id', 'obs', 'province', 'country', 'updated', 'confirmed', 'deaths', 'recovered')


countries <- c('Austria_AUT_P1', 'Australia_AUS_P2', 'Belgium_BEL_P1', 'Canada_CAN_P2', 'Denmark_DNK_P1', 
               'Finland_FIN_P1', 'France_FRA_P1', 'Germany_DEU_P1', 'Greece_GRC_P1', 'Hong Kong_HKG_P3', 
               'Ireland_IRL_P1', 'Israel_ISR_P1', 'Italy_ITA_P1', 'Japan_JPN_P2',
               'Mainland China_CHN_P3', 'Malaysia_MYS_P1', 'Netherlands_NLD_P1', 'Portugal_PRT_P1', 
               'Singapore_SGP_P3',  'South Korea_KOR_P1',
               'Spain_ESP_P1', 'Switzerland_CHE_P1', 'Thailand_THA_P3', 'UK_GBR_P1', 'US_USA_P1')

ggplot(cv_country[rate>0 & outcome=='confirmed' & code > 'MYS' & code <='USA'], 
       aes(obs, rate, colour = code)) + 
  geom_point() + geom_line() + scale_y_log10()

ccodes <- as.data.table(str_split_fixed(countries, '_', n = 3))
names(ccodes) <- c('country', 'code', 'policy')

country_pop <- fread(
  file.path(data_path, 'API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv'),
  select = c('Country Code', '2018'))
names(country_pop) <- c('code', 'pop')
country_pop$pop <- as.numeric(country_pop$pop)


cv_province <-  cov_sum[, 
                        lapply(.SD, sum), 
                        .SDcols = c('confirmed', 'deaths', 'recovered'),
                        by = c('obs', 'country', 'province')] %>%
  merge(ccodes, by = 'country')

ts_len <- cv_province[, 
                      c(.N, lapply(.SD, max)),
                      .SDcols = c('confirmed', 'deaths', 'recovered'),
                      by = c('code', 'province')][, mortality:=deaths/confirmed]

ggplot(ts_len, aes(N)) + geom_histogram( binwidth = 5)

cv_province_long <- cv_province %>%
  melt(id.vars = c('obs', 'country', 'code', 'province'), variable.name = 'outcome', value.name = 'count')

ggplot(cv_province_long[code=='CHN' & province=='Hubei' & count>0], aes(obs, count, colour = outcome)) + 
  geom_point() + geom_line() + scale_y_log10()

ggplot(cv_province_long[code=='USA' & province=='Washington' & count>0], 
       aes(obs, count, colour = outcome)) + 
  geom_point() + geom_line() + scale_y_log10()

cv_country <- cov_sum[, 
                      lapply(.SD, sum), 
                      .SDcols = c('confirmed', 'deaths', 'recovered'),
                      by = c('obs', 'country')] %>%
  melt(id.vars = c('obs', 'country'), variable.name = 'outcome', value.name = 'count') %>%
  merge(ccodes, by = 'country') %>%
  merge(country_pop, by = 'code') %>%
  `[`(T, rate:=count/pop)


ggplot(cv_country)

ggplot(cv_country[code=='CHN' & rate>0], aes(obs, count, colour = outcome)) + 
  geom_point() + geom_line() + scale_y_log10()

ggplot(cv_country[code=='USA' & rate >0], aes(obs, rate, colour = outcome)) + 
  geom_point() + geom_line() + scale_y_log10()

ggplot(cv_country[code=='ITA' & rate >0], aes(obs, rate, colour = outcome)) + 
  geom_point() + geom_line() + scale_y_log10()

ggplot(cv_country[code=='IRL' & rate >0], aes(obs, rate, colour = outcome)) + 
  geom_point() + geom_line() + scale_y_log10()

ggplot(cv_country[code=='NLD' & rate >0], aes(obs, rate, colour = outcome)) + 
  geom_point() + geom_line() + scale_y_log10()

ggplot(cv_country[code==c('CHN', 'KOR') & rate >0], aes(obs, rate, colour = outcome)) + 
  facet_wrap(~code) +
  geom_point() + geom_line() + scale_y_log10()

ggplot(cv_country[code==c('ITA', 'CHN', 'IRL', 'FRA', 'ESP', 'DEU') & rate >0], aes(obs, rate, colour = outcome)) + 
  facet_wrap(~code) +
  geom_point() + geom_line() + scale_y_log10()

ggplot(cv_country[(code %in% c('ITA', 'CHN', 'IRL', 'FRA', 'ESP', 'DEU', 'GBR', 'KOR', 'THA')) & 
                    rate>0 &
                    outcome=='confirmed'], 
       aes(obs, rate, colour = code)) + 
  geom_point() + geom_line() + scale_y_log10()

ggplot(cv_country[rate>0 & outcome=='confirmed' & 
                    (policy=='P3' | 
                       code %in% c('FRA', 'DEU', 'IRL', 'GBR', 'ITA'))], 
       aes(obs, rate, colour = code)) + 
  facet_wrap(~policy) +
  geom_point() + geom_line() + scale_y_log10()



