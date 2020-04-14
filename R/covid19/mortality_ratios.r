#
# More simple than the mortality rates where I fitted a curve, here I just get the plain ratio
#

picture_path <- file.path('pictures', 'mortality_ratios')

latest_country <- cov_sum[province=='', .SD[obs==max(obs)], by = 'country'][, region:=country]
latest_province <- cov_sum[province!='', .SD[obs==max(obs)], by = 'province'][, region:=province]

latest <- rbind(
  latest_country[, .(region, confirmed, deaths)],
  latest_province[, .(region, confirmed, deaths)]
)[, ratio:=confirmed/deaths]

p1 <- ggplot(mapping = aes(x = factor(region, levels = region[order(ratio)], ordered = T),
                           y = ratio)) + 
  geom_bar(stat = 'identity') +
  labs(
    x = element_blank(),
    y = "Ratio of confirmed cases to reported deaths",
    fill = "Deaths"
  ) +
  theme(legend.position = c(0.8, 0.3)) +
  coord_flip()

p1 %+% latest[deaths>20] + aes(fill = cut(deaths, 
                                          breaks = c(20, 100, 1000, Inf),
                                          labels = c('20-100', '100-1000', '>1000'),
                                          right = F)) +
  labs(
    title = "Confirmed:Mortality ratios where there are more than 20 deaths", 
    fill = "Deaths"
    )

ggsave('high_deaths.png', path = picture_path)

p1 %+% latest[deaths<=20 & deaths>5] + aes(fill = cut(deaths, 
                                                      breaks = c(5, 10, 15, 20),
                                                      labels = c('5-10', '10-15', '15-20'),
                                                      right = T)) +
  labs(
    title = "Confirmed:Mortality ratios where there are 5-20 deaths", 
    fill = "Deaths")

ggsave('low_deaths.png', path = picture_path)
