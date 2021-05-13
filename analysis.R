library(countrycode)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(patchwork)
library(glue)

setwd('~/Documents/Github/tidytuesday-big-mac-index/')
theme_set(theme_minimal(base_family = 'Raleway', base_size = 10))

data = read.csv('clean_data.csv')

# Price change in local currency
#####################################################################################
i = 1
for (c in unique(data$continent)) {
  
  temp = data %>% 
    dplyr::filter(continent == c) %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(max = max(local_price),
                  min = min(local_price),
                  local_price = (local_price - min) / (max - min),
                  country2 = country) %>% 
    dplyr::select(-country)
  
  data %>% 
    dplyr::filter(continent == c) %>% 
    dplyr::group_by(country) %>% 
    # dplyr::mutate(max = max(local_price),
    #               min = min(local_price),
    #               local_price = (local_price - min) / (max - min)) %>% 
    ggplot(aes(x = year, y = local_price)) + 
    # geom_line(data = temp, aes(group=name2), col = 'lightgrey', lwd = 0.5) + 
    geom_line(col = ggthemes::gdocs_pal()(9)[i], lwd = 1.2) +
    facet_wrap(~country, scales = 'free') + 
    labs(x = '', y = '', title = glue('Price of Big Mac in Local Currency Across {c}')) + 
    theme(panel.grid.minor = element_blank(),
          legend.position = 'none') 
  
  # ggsave(glue('{c}_local_currency.png'), width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)
  
  i = i + 1
}

data %>% 
  dplyr::group_by(country) %>% 
  arrange(year) %>% 
  dplyr::mutate(local_price = local_price/sum((row_number() == 1)*local_price)) %>% 
  dplyr::filter(country != 'Argentina') %>%
  ggplot(aes(x = year, y = local_price, group = country, col = continent)) + 
  geom_line(show.legend = F) + 
  facet_wrap(~continent) +
  theme(panel.grid.minor = element_blank()) + 
  ggthemes::scale_color_gdocs() + 
  ylab("Gorwth Percentage") + xlab('') + 
  labs(title = 'Big Mac Price Surges over the Past 20 Years',
       subtitle = '2000 as Base Year')

# ggsave('price_change_normalize.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

temp = data %>% 
  dplyr::group_by(country) %>% 
  arrange(year) %>% 
  dplyr::mutate(local_price = local_price/sum((row_number() == 1)*local_price)) %>% 
  dplyr::group_by(continent, country) %>% 
  dplyr::summarise(max_growth = max(local_price)) 
temp$country = forcats::fct_reorder(temp$country, temp$max_growth)

temp %>% 
  dplyr::filter(country != 'Argentina') %>% 
  ggplot() + 
  geom_bar(aes(y = country, x = max_growth), stat = 'identity', fill = 'lightgrey') + 
  geom_vline(aes(xintercept = median(temp$max_growth)), col = 'red',linetype = 'dotted', lwd = 0.75) +
  scale_x_continuous(labels = scales::percent) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())
# facet_wrap(~continent, scales = 'free') + 
# ggsave('max_growth.png', height = 16, width = 9, units = 'in', dpi = 500, scale = 0.6)


temp = data %>% 
  dplyr::group_by(country) %>% 
  arrange(year) %>% 
  # dplyr::mutate(local_price = local_price/sum((row_number() == 1)*local_price)) %>% 
  dplyr::group_by(country) %>% 
  arrange(year) %>% 
  dplyr::filter(row_number() == 1 | row_number() == max(row_number())) %>% 
  arrange(country, year) %>% 
  dplyr::mutate(order = row_number(),
                order = ifelse(order == 1, 'start', 'end')) %>% 
  dplyr::select(country, local_price, order) %>% 
  tidyr::pivot_wider(id_col = country, names_from = order, values_from = local_price) %>% 
  dplyr::mutate(growth = (end/start)^(1/20) - 1) %>% 
  dplyr::mutate(continent = countrycode(sourcevar = country,
                                        origin = "country.name",
                                        destination = "continent")) %>%
  dplyr::mutate(continent = ifelse(country == 'Euro area','Europe', continent)) 
temp$name = forcats::fct_reorder(temp$country, temp$growth)

ggplot(temp) +
  geom_bar(aes(y = country, x = growth), stat = 'identity', fill = 'lightgrey') +
  # geom_bar(data = temp %>% dplyr::filter(continent == 'Asia'), aes(y = name, x = growth, fill = continent), stat = 'identity', alpha = 0.5) +
  # geom_vline(aes(xintercept = mean(temp$growth)), col = 'red', linetype = 'dashed', lwd = 0.85) +
  geom_vline(aes(xintercept = median(temp$growth)), col = 'red',linetype = 'dotted', lwd = 0.75) +
  scale_x_continuous(labels = scales::percent) +
  ggthemes::scale_fill_gdocs() + 
  # facet_wrap(~continent, scales = 'free_y') +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) 
# ggsave('price_growth.png', height = 16, width = 9, units = 'in', dpi = 500, scale = 0.6)


# Prince change in US dollar
#####################################################################################
i = 1
for (c in unique(data$continent)) {
  
  temp = data %>% 
    dplyr::filter(!(name %in% missing_country$country) & continent == c) %>% 
    dplyr::mutate(name2 = name)
  
  data %>% 
    dplyr::filter(!(name %in% missing_country$country) & continent == c) %>% 
    ggplot(aes(x = year, y = dollar_price)) + 
    geom_line(data = temp %>% dplyr::select(-name), aes(group=name2), col = 'lightgrey', lwd = 0.5) + 
    geom_line(lwd = 1.15, col = ggthemes::gdocs_pal()(9)[i]) +
    facet_wrap(~name, scales = 'free') + 
    labs(x = '', y = '', title = glue('Price of Big Mac in US dollar Across {c}')) + 
    theme(panel.grid.minor = element_blank(),
          legend.position = 'none') 
  
  # ggsave(glue('{c}_us_dollar.png'), width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)
  
  i = i + 1
}

#
ggplot(data, aes(x = gdp, y = dollar_price, col = continent)) + 
  geom_point(show.legend = F, col = 'grey') +
  stat_smooth(formula = y~x, method = 'lm', se = F, aes(group = name), show.legend = F) + 
  facet_wrap(~continent, scales = 'free')

#
ggplot(data, aes(x = year, y = dollar_price, group = name, col = continent)) +
  geom_point(col = 'grey') +
  stat_smooth(aes(group = continent), formula = y~x, se = F) + 
  theme_minimal() + 
  scale_color_brewer(palette = 'Set1') + 
  facet_wrap(~continent) + 
  ylim(0, 12) + 
  stat_regline_equation(aes(group = continent), label.y = 10, label.x = 2002) + 
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none')  

#
data %>% 
  dplyr::group_by(year, continent) %>% 
  dplyr::summarise(dollar_price = mean(dollar_price)) %>% 
  ggplot(aes(x = year, y = dollar_price, col = continent)) +
  geom_point(col = 'grey') +
  stat_smooth(aes(group = continent), formula = y~x, se = F) + 
  theme_minimal() + 
  scale_color_brewer(palette = 'Set1') + 
  facet_wrap(~continent) + 
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none')  

# Minimum Wage
#####################################################################################

wage = read.csv('minimum_wage.csv') %>% 
  dplyr::mutate(iso_a3 = countrycode(country, 
                                     destination = 'iso3c', origin = 'country.name'),
                country = countrycode(iso_a3, 
                                      origin = 'iso3c', destination = 'cldr.short.en')) %>% 
  dplyr::select(-iso_a3) %>% 
  tidyr::drop_na()


merge(data, wage, by = 'country') %>% 
  dplyr::filter(year == 2020) %>% 
  dplyr::mutate(hour = dollar_price / hourly_wage,
                continent = ifelse(continent %in% c('Africa', 'Oceania'), 'Africa & Oceania', continent)) %>% 
  dplyr::mutate(country = forcats::fct_reorder(country, hour)) %>%
  ggplot() + 
  geom_bar(aes(x = country, y = hour, fill = continent), stat = 'identity', show.legend = F) + 
  facet_wrap(~continent, scales = 'free_y') + 
  coord_flip() + 
  ggthemes::scale_fill_gdocs() +
  theme(panel.grid.minor = element_blank())



temp = merge(data, wage, by = 'country') %>% 
  dplyr::filter(year == 2020) %>% 
  dplyr::mutate(hour = dollar_price / hourly_wage,
                continent = ifelse(continent %in% c('Africa', 'Oceania'), 'Africa & Oceania', continent))
  
target = c('China', 'Hong Kong', 'India', 'Japan', 'South Korea', 'Taiwan', 'Thailand', 'UK', 'US')

ggplot(temp %>% dplyr::filter(!(country %in% target)), 
       aes(x = hourly_wage, y = dollar_price, label = country)) + 
  geom_point(aes(color = continent), shape = 21, fill = 'white', size = 5, stroke = 1.5) + 
  stat_smooth(formula = y~poly(x, 2), method = 'lm', se = F, col = 'grey', lwd = 1.5) +
  ggrepel::geom_label_repel(data = temp %>% dplyr::filter(country %in% target)) +
  ggthemes::scale_color_gdocs() +
  theme(legend.position = 'top')


