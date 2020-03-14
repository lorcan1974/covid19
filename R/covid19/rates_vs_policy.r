source('read_data.r')

##########################################################################################
#
# Now generate the plots that I posted to FB.
#
##########################################################################################

p1 <- ggplot(cv_country[rate>0 & 
                          outcome=='confirmed' & policy=='P1' &
                          code %in% 
                          c('FRA', 'DEU', 'IRL', 'GBR', 'ITA', 'CHE', 'USA', 'ISR', 'DNK')], 
             aes(obs, rate*1e6, colour = code)) + 
  geom_point() + geom_line() + scale_y_log10() +
  coord_cartesian(ylim = c(1e-2, 200)) +
  theme(axis.title=element_blank()) + 
  labs(colour = "Country")

p2 <- p1 %+% cv_country[rate>0 & outcome=='confirmed' & policy=='P2']
p3 <- p1 %+% cv_country[rate>0 & outcome=='confirmed' & policy=='P3']

g1 <- grid.arrange(p1, p2, p3, nrow = 1, 
                   top = "Confirmed case rates versus country policy",
                   bottom = "Observation Date",
                   left = "Cases per million people")

# ggsave('policy.png', g1, width = 100, height = 34, units = 'mm')

