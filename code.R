library(countrycode)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

setwd('~/Documents/Github/tidytuesday-big-mac-index/')
theme_set(theme_minimal(base_family = 'Raleway', base_size = 10))

data = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv') %>% 
  dplyr::select(date, name, dollar_price, local_price, dollar_ex, gdp_dollar)

survey_num = data %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, name) %>% 
  tally() %>% 
  tidyr::spread(name, n, fill = 0) %>% 
  tidyr::gather(key = 'country', value = 'number', -year)

missing_country = survey_num %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(missing = sum(number == 0)) %>% 
  dplyr::filter(missing >= 10)
  
ggplot(survey_num, aes(x = year, y = country, fill = factor(number))) + 
  geom_tile( col = 'white', size = 0.5, alpha = 0.85) + 
  scale_x_continuous(breaks = seq(2000, 2020, 4),
                     labels = seq(2000, 2020, 4)) + 
  # scale_y_discrete(breaks = missing_country$country,
  #                  labels = missing_country$country,
  #                  expand=c(0,0)) + 
  scale_fill_manual(values=c("#d53e4f","#fdae61","#abdda4")) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y=element_text(vjust=0.2),
        axis.ticks=element_line(size=0.15)) + 
  guides(fill = guide_legend(title="Number of\nSurveys")) + 
  labs(x = '', y = '', title = 'The Number of Big Mac Index Surverys')


data = data %>% 
  dplyr::mutate(year = year(date)) %>% 
  dplyr::group_by(year, name) %>% 
  dplyr::summarise(dollar_price = mean(dollar_price),
                   local_price = mean(local_price),
                   dollar_ex = mean(dollar_ex),
                   gdp_dollar = mean(gdp_dollar)) %>%
  dplyr::mutate(continent = countrycode(sourcevar = name,
                                        origin = "country.name",
                                        destination = "continent")) %>%
  dplyr::mutate(continent = ifelse(name == 'Euro area','Europe', continent)) %>% 
  dplyr::select(year, name, continent, dollar_price:gdp_dollar)

data %>% 
  dplyr::filter(!(name %in% missing_country$country) & continent == 'Asia') %>% 
  # dplyr::mutate(name = forcats::fct_reorder(name, continent)) %>% 
  ggplot(aes(x = year, y = local_price, col = continent)) + 
  geom_line(lwd = 1.2) +
  # stat_smooth(formula = y~x, method = 'lm', se = F, lwd = 0.75) + 
  facet_wrap(~name, scales = 'free') + 
  labs(x = '', y = '', title = 'Price of Big Mac in Local Currency Across Asia') + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none') 

ggplot(data) + 
  geom_point(aes(x = gdp_dollar, y = dollar_price, col = continent)) +
  scale_color_brewer(palette = 'Set1') +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal') 


