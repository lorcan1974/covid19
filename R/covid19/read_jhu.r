
jhu_path <- '/Users/lorcan/Projects/GitHub/jhu/COVID-19/csse_covid_19_data/csse_covid_19_time_series'

fc_us <- 'time_series_covid19_confirmed_US.csv'
fc_gl <- 'time_series_covid19_confirmed_global.csv'
fd_us <- 'time_series_covid19_deaths_US.csv'
fd_gl <- 'time_series_covid19_deaths_global.csv'

c_us <- fread(file.path(jhu_path, fc_us))
d_us <- fread(file.path(jhu_path, fd_us))
c_gl <- fread(file.path(jhu_path, fc_gl))

read_jhu_global <- function(fname, jhu_path, count_name) {
  xx <- fread(file.path(jhu_path, fname))[, c('Lat', 'Long'):=NULL] %>%
  melt(id.vars = c('Province/State', 'Country/Region'),
       variable.name = 'obs',
       value.name = count_name) %>%
  `[`(T, obs:=as.Date(obs, format = '%m/%d/%y'))
  names(xx)[1:2] <- c('province', 'country')
  xx
}

read_jhu_us <- function(fname, jhu_path, count_name) {
  id_cols <- c('Province_State', 'Country_Region')
  xx <- fread(file.path(jhu_path, fname))
  cn <- names(xx)
  drop_cols <- setdiff(cn[is.na(parse_date_time(cn, orders = '%m/%d/%y', quiet = T))], 
                       c(id_cols, 'Admin2'))
  xx <- fread(file.path(jhu_path, fname), drop = drop_cols)[Province_State==Admin2] %>%
    `[`(T, Admin2:=NULL) %>%
    melt(id.vars = id_cols,
         variable.name = 'obs',
         value.name = count_name) %>%
    `[`(T, obs:=as.Date(obs, format = '%m/%d/%y'))
  names(xx)[1:2] <- c('province', 'country')
  xx
}

c_us <- read_jhu_us(fc_us, jhu_path, 'confirmed')
d_us <- read_jhu_us(fd_us, jhu_path, 'deaths')
jhu_us <- merge(c_us, d_us, all.x = T, by = c('province', 'country', 'obs'))

c_gl <- read_jhu_global(fc_gl, jhu_path, 'confirmed')
d_gl <- read_jhu_global(fd_gl, jhu_path, 'deaths')

jhu_gl <- merge(c_gl, d_gl, all.x = T, by = c('province', 'country', 'obs'))

rm(c_gl, d_gl)

