library(data.table)
library(purrr)
library(lubridate)
library(ggplot2)
library(stringr)

data_path <- 'datasets'
worldbank_data <- file.path(data_path, 'worldbank')
kaggle_data <- file.path(data_path, 'kaggle20200317_v35')

##########################################################################################
#
# Read in the summary table showing the C19 cases
# These data sourced from Kaggle: 
# https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset/data#covid_19_data.csv
#
##########################################################################################

cov_sum <- fread(
  file.path(kaggle_data, 'covid_19_data.csv'), drop=c('SNo', 'Last Update', 'Recovered')
)[, ObservationDate:=parse_date_time(ObservationDate, orders = c('m/d/y', 'm/d/Y'))]

names(cov_sum) <- c('obs', 'province', 'country', 'confirmed', 'deaths')
cov_sum <- cov_sum[, c('obs', 'confirmed', 'deaths'):=list(as.Date(obs), as.numeric(confirmed), as.numeric(deaths))]

# Tidy up some weirdness in the Province field:
cov_sum[province==country, province:='']
cov_sum[province=='' & country=='UK', province:='United Kingdom']

# Correction required for Ireland:
cov_sum[country=='Ireland' & obs=='2020-03-15', c('confirmed', 'deaths'):=list(169, 2)]

# Correction for Italy required in the kaggle20200314 dataset:
cov_sum[country=='Italy' & obs=='2020-03-12', c('confirmed', 'deaths'):=list(15113, 1016)]
# tail(cov_sum[country=='Italy'][order(obs)])
# tail(cov_sum[country=='Ireland'][order(obs)])

##########################################################################################
#
# Select a set of countries for a more detailed look. I chose these on the basis that they have had a 
# significant level of exposure and are likely to adhere to similar standards in data collection and 
# reporting and therefore results are likely to be comparable across sets.
#
# The countries are coded as: <Name>_<ISO Code>_<Policy label>.
# The ISO Code is used to join to a population table below.
# The Policy label was handcoded by examining the evolution of the growth in cases/million and separating
# into 3 groups by eye using a purely data-driven approach.
#
##########################################################################################
countries <- c('Austria_AUT_P1', 'Australia_AUS_P2', 'Belgium_BEL_P1', 'Canada_CAN_P2', 'Denmark_DNK_P1', 
               'Finland_FIN_P1', 'France_FRA_P1', 'Germany_DEU_P1', 'Greece_GRC_P1', 'Hong Kong_HKG_P3', 
               'Ireland_IRL_P1', 'Israel_ISR_P1', 'Italy_ITA_P1', 'Japan_JPN_P2',
               'Mainland China_CHN_P3', 'Malaysia_MYS_P1', 'Netherlands_NLD_P1', 'Portugal_PRT_P1', 
               'Singapore_SGP_P3',  'South Korea_KOR_P1',
               'Spain_ESP_P1', 'Switzerland_CHE_P1', 'Thailand_THA_P3', 'UK_GBR_P1', 'US_USA_P1')

ccodes <- as.data.table(str_split_fixed(countries, '_', n = 3))
names(ccodes) <- c('country', 'code', 'policy')

# Stub code for examining small batches of country growth rates, hence commented out. 
# Relies on a DT defined below, copied here for convenience.
#
# ggplot(cv_country[rate>0 & outcome=='confirmed' & code > 'MYS' & code <='USA'], 
#        aes(obs, rate, colour = code)) + 
#   geom_point() + geom_line() + scale_y_log10()

##########################################################################################
#
# Population data from: 
# https://data.world/worldbank/total-population-per-country#__sid=js0
#
##########################################################################################

country_pop <- fread(
  file.path(worldbank_data, 'API_SP.POP.TOTL_DS2_en_csv_v2_821007.csv'),
  select = c('Country Code', '2018'))
names(country_pop) <- c('code', 'pop')
country_pop$pop <- as.numeric(country_pop$pop)

##########################################################################################
#
# Bump all the data together, rollup by country.
# (I did some analysis of Province, mostly everything in China maps to Hubei, so I abandoned that idea.)
#
##########################################################################################

cv_with_pop <- cov_sum %>%
  merge(ccodes, by = 'country') %>%
  merge(country_pop, by = 'code')

##############################################################################
#
# Some errors parsing dates - am leaving these out for now:
#
##############################################################################

# cov_open <- fread(
#   file.path(kaggle_data, 'COVID19_open_line_list.csv'), 
#   select = c('age', 'sex', 'country', 'date_onset_symptoms', 'date_admission_hospital', 'date_confirmation', 'symptoms', 'date_death_or_discharge', 'outcome')
# )[, c('onset', 'admission', 'confirmed', 'death_or_discharge'):=lapply(.SD, parse_date_time, orders = c('d.m.Y', 'd.m.y')), 
#   .SDcols = c('date_onset_symptoms', 'date_admission_hospital', 'date_confirmation', 'date_death_or_discharge')][
#     , c('date_onset_symptoms', 'date_admission_hospital', 'date_confirmation', 'date_death_or_discharge'):=NULL
#     ][, c('tt_confirm', 'tt_resolve'):=
#         list(
#           as.numeric(difftime(confirmed, onset, units = 'days')), 
#           as.numeric(difftime(death_or_discharge, onset, units = 'days'))
#         )
#       ]
# 
# cov_line <- fread(
#   file.path(kaggle_data, 'COVID19_line_list_data.csv'),
#   select = c('summary', 'country', 'gender', 'age', 'death', 'recovered', 'reporting date', 'symptom_onset', 'hosp_visit_date', 'exposure_start', 'exposure_end')
# )[, c('reported', 'onset', 'hospitalised', 'exposure_start', 'exposure_end'):=lapply(.SD, parse_date_time, orders = c('m/d/Y', 'm/d/y')), 
#   .SDcols = c('reporting date', 'symptom_onset', 'hosp_visit_date', 'exposure_start', 'exposure_end')][
#     , c('reporting date', 'symptom_onset', 'hosp_visit_date', 'exposure_start', 'exposure_end'):=NULL
#     ][, c('tt_hosp', 'tt_report'):=
#         list(
#           as.numeric(difftime(hospitalised, onset, units = 'days')), 
#           as.numeric(difftime(reported, onset, units = 'days'))
#         )
#       ]

