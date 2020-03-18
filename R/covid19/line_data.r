##########################################################################################
#
# Kicked the tyres on this, but most of the useful looking columns are scantily populated or contain obviously noisy data.
# What I really wanted was time from onset to resolution, be it either recovery or death, but data here are very scant.
# From 12 data points I have 17.7 days to death; and around the same to a 'discharge' status. 
# Mean time to confirm is 6.43 days, but not sure how useful this is for any analysis.
# So almost certainly better to Google the best estimates for these.
#
##########################################################################################

source('read_data.r')

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

ggplot(cov_line[tt_hosp>=0], aes(tt_hosp)) + geom_histogram(binwidth = 1, boundary = 1)
ggplot(cov_line[tt_report>=0], aes(tt_report)) + geom_histogram(binwidth = 1, boundary = 1)



