##########################################################################################
#
# Kicked the tyres on this, but most of the useful looking columns are scantily populated or contain obviously noisy data.
# What I really wanted was time from onset to resolution, be it either recovery or death, but data here are very scant.
# From 12 data points I have 17.7 days to death; and around the same to a 'discharge' status. 
# Mean time to confirm is 6.43 days, but not sure how useful this is for any analysis.
# So almost certainly better to Google the best estimates for these.
#
##########################################################################################

cov_open <- fread(
  file.path(data_path, 'COVID19_open_line_list.csv'), 
  select = c('age', 'sex', 'country', 'date_onset_symptoms', 'date_admission_hospital', 'date_confirmation', 'symptoms', 'date_death_or_discharge', 'outcome')
)[, c('onset', 'admission', 'confirmed', 'death_or_discharge'):=lapply(.SD, parse_date_time, orders = c('d.m.Y', 'd.m.y')), 
  .SDcols = c('date_onset_symptoms', 'date_admission_hospital', 'date_confirmation', 'date_death_or_discharge')][
    , c('date_onset_symptoms', 'date_admission_hospital', 'date_confirmation', 'date_death_or_discharge'):=NULL
    ][, c('tt_confirm', 'tt_resolve'):=
        list(
          as.numeric(difftime(confirmed, onset, units = 'days')), 
          as.numeric(difftime(death_or_discharge, onset, units = 'days'))
        )
      ]

ggplot(cov_open[tt_resolve>=0 & outcome=='death'], aes(tt_resolve)) + geom_histogram(binwidth = 1, boundary = 1)
ggplot(cov_open[tt_confirm>=0], aes(tt_confirm)) + geom_histogram(binwidth = 1, boundary = 1)

cov_open[, mean(tt_confirm, na.rm = T)]

cov_open[outcome=='discharged', outcome:='discharge']
cov_open[outcome=='Discharged', outcome:='discharge']
cov_open[!is.na(tt_resolve), .(.N, tt_resolve = mean(tt_resolve, na.rm = T)), by = 'outcome']
ggplot(cov_open[!is.na(tt_resolve)], aes(tt_resolve)) + 
  geom_histogram(binwidth = 1, boundary = 1)

cov_open[outcome=='death' & !is.na(tt_resolve)]  

##########################################################################################
#
# This didn't have much obviously useful either.
#
##########################################################################################

cov_line <- fread(
  file.path(data_path, 'COVID19_line_list_data.csv'),
  select = c('summary', 'country', 'gender', 'age', 'death', 'recovered', 'reporting date', 'symptom_onset', 'hosp_visit_date', 'exposure_start', 'exposure_end')
)[, c('reported', 'onset', 'hospitalised', 'exposure_start', 'exposure_end'):=lapply(.SD, parse_date_time, orders = c('m/d/Y', 'm/d/y')), 
  .SDcols = c('reporting date', 'symptom_onset', 'hosp_visit_date', 'exposure_start', 'exposure_end')][
    , c('reporting date', 'symptom_onset', 'hosp_visit_date', 'exposure_start', 'exposure_end'):=NULL
    ][, c('tt_hosp', 'tt_report'):=
        list(
          as.numeric(difftime(hospitalised, onset, units = 'days')), 
          as.numeric(difftime(reported, onset, units = 'days'))
        )
      ]



ggplot(cov_line[tt_hosp>=0], aes(tt_hosp)) + geom_histogram(binwidth = 1, boundary = 1)
ggplot(cov_line[tt_report>=0], aes(tt_report)) + geom_histogram(binwidth = 1, boundary = 1)



