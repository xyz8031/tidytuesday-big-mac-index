library(countrycode)
library(dplyr)
library(ggplot2)
library(gganimate)

setwd('~/Documents/Github/tidytuesday-big-mac-index/')
theme_set(theme_minimal(base_family = 'Raleway', base_size = 14))

data = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv') %>% 
  dplyr::filter(name != 'Euro area') %>% 
  dplyr::select(date, name, dollar_price, gdp_dollar) %>% 
  dplyr::mutate(continent = countrycode(sourcevar = name,
                                        origin = "country.name",
                                        destination = "continent"))

data %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, name) %>% 
  tally() %>% 
  tidyr::spread(name, n, fill = 0) %>% 
  tidyr::gather(key = 'country', value = 'number', -year) %>% 
  ggplot() + 
  geom_tile(aes(x = year, y = country, fill = factor(number)), col = 'white') + 
  scale_x_continuous(breaks = seq(2000, 2020, 4),
                     labels = seq(2000, 2020, 4)) + 
  scale_fill_manual(values=c("#d53e4f","#fdae61","#abdda4")) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal') + 
  guides(fill = guide_legend(title="Number of Surveys")) + 
  labs(x = '', y = '')

country = data %>% 
  dplyr::group_by(name) %>% 
  tally() %>% 
  dplyr::filter(n >= 10)

data %>% 
  dplyr::filter(name %in% country$name) %>% 
  ggplot(aes(x = date, y = dollar_price, col = continent)) + 
  geom_line() +
  # stat_smooth(formula = y~x, method = 'lm', se = F, lwd = 0.75) + 
  facet_wrap(~name) + 
  scale_color_brewer(palette = 'Set1') + 
  scale_x_date(date_breaks = '10 year', date_labels = '%Y') + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal') + 
  theme_bw()


ggplot(data) + 
  geom_point(aes(x = gdp_dollar, y = dollar_price, col = continent)) +
  scale_color_brewer(palette = 'Set1') +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal') + 
  labs(title = 'Year: {frame_time}') +
  transition_time(date) + 
  ease_aes('linear')


