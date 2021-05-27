library(countrycode)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(patchwork)
library(glue)
library(ggrepel)

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


highlight = c('Ukraine', 'Egypt', 'Brazil', 'Uruguay', 'Sri Lanka', 'US',
              'Russia', 'Hungary', 'Turkey', 'Australia', 'Saudi Arabia',
              'Peru', 'Switzerland', 'Taiwan', 'South Africa', 'Argentina')

temp = data %>% 
  dplyr::group_by(country) %>% 
  arrange(year) %>% 
  dplyr::mutate(local_price = local_price/sum((row_number() == 1)*local_price),
                facet = ifelse(continent %in% c('Oceania', 'Africa'), 'Africa & Oceania', continent))  
  # dplyr::filter(country != 'Argentina') 
  
ggplot() + 
  geom_line(data = temp %>% dplyr::filter(!(country %in% highlight)), 
            aes(x = year, y = local_price, group = country), col = 'lightgrey', 
            lwd = 0.5, show.legend = F) + 
  geom_line(data = temp %>% dplyr::filter(country %in% highlight), 
            aes(x = year, y = local_price, group = country, col = continent), 
            lwd = 1.125, show.legend = F) + 
  geom_text(data =temp %>% dplyr::filter(country %in% highlight & year == 2020),
            aes(x = 2020.25, y = local_price, label = country),
            hjust = 0, check_overlap = T) + 
  facet_wrap(~facet) +
  ylim(NA, 8) + 
  scale_x_continuous(breaks = seq(2000, 2020, 5),
                     limits = c(2000, 2025)) + 
  theme(panel.grid.minor = element_blank()) + 
  ggthemes::scale_color_gdocs() + 
  ylab("Gorwth Percentage") + xlab('') + 
  labs(title = 'Big Mac Price Surges over the Past 20 Years',
       subtitle = '2000 as Base Period')

# ggsave('price_change_normalize.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

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
  dplyr::mutate(growth = (end/start)^(1/19) - 1) 

temp$country = forcats::fct_reorder(temp$country, temp$growth)
temp$continent = countrycode(temp$country, origin = "country.name", destination = "continent")
temp$continent = ifelse(temp$country == 'Euro Zone', 'Europe', temp$continent)

ggplot(temp) +
  geom_bar(aes(y = country, x = growth), stat = 'identity', fill = 'lightgrey') +
  # geom_bar(data = temp %>% dplyr::filter(continent == 'Asia'), aes(y = name, x = growth, fill = continent), stat = 'identity', alpha = 0.5) +
  # geom_vline(aes(xintercept = mean(temp$growth)), col = 'red', linetype = 'dashed', lwd = 0.85) +
  geom_vline(aes(xintercept = mean(temp$growth)), col = 'red',linetype = 'dotted', lwd = 0.75) +
  scale_x_continuous(breaks = seq(0, 0.25, 0.05),
                     labels = scales::percent) +
  ggthemes::scale_fill_gdocs() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) 
# ggsave('price_growth.png', height = 16, width = 9, units = 'in', dpi = 500, scale = 0.6)

data %>% 
  dplyr::group_by(country) %>% 
  arrange(year) %>% 
  dplyr::mutate(growth = (local_price / lag(local_price)) - 1) %>% 
  tidyr::drop_na(growth) %>% 
  # dplyr::filter(!(continent %in% c('Oceania','Africa'))) %>% 
  ggplot(aes(x = growth, y = year, group = year)) +
  ggridges::stat_density_ridges(quantile_lines = TRUE, scale = 1.5,
                                quantiles = 0.5, alpha = 0.5,
                                rel_min_height = 0.05) + 
  # ggridges::geom_density_ridges2(scale = 0.95) +
  # facet_wrap(~continent, ncol = 1) + 
  scale_x_continuous(limits = c(-.05, 0.3),
                     labels = scales::percent) + 
  scale_y_continuous(breaks = seq(2000, 2020, 4)) + 
  ggthemes::scale_color_gdocs() + 
  theme(legend.position = 'None',
        panel.grid.minor = element_blank())
# ggsave('price_growth_distribution.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

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
highlight = c('Australia', 'Egypt', 
              'US', 'Mexico', 'Brazil',
              'Israel', 'Singapore', 'Taiwan',
              'Switzerland', 'Russia', 'Euro Zone')

temp = data %>% 
  dplyr::mutate(region = ifelse(continent %in% c('Africa', 'Oceania'), 'Africa & Oceania', continent)) 

ggplot() +
  geom_line(data = temp %>% dplyr::filter(!(country %in% highlight)), 
            aes(x = year, y = dollar_price, group = country), 
            col = 'lightgrey', lwd = 0.5) + 
  geom_line(data = temp %>% dplyr::filter(country %in% highlight), 
            aes(x = year, y = dollar_price, group = country, col = continent),
            lwd = 1.125) + 
  geom_text(data = temp %>% dplyr::filter(country %in% highlight & year == 2020),
            aes(x = 2020.25, y = dollar_price, label = country),
            hjust = 0, check_overlap = T) + 
  facet_wrap(~region) + 
  ggthemes::scale_color_gdocs() +
  theme(legend.position = 'None',
        panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks = seq(2000, 2020, 5),
                     limits = c(2000, 2025)) + 
  ylim(0, NA)

# ggsave('price_change_usd.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)


#

temp = data %>% 
  dplyr::mutate(region = ifelse(continent %in% c('Africa', 'Oceania'), 'Africa & Oceania', continent)) 

highlight = c('South Africa',
              'Brazil', 'Canada', 'Chile',
              'Israel', 'Japan', 'Singapore', 'Turkey', 
              'Norway', 'Sweden', 'Hungary',
              'Australia', 'Thailand', 'Malaysia')

ggplot() + 
  geom_point(data = temp, aes(x = gdp, y = dollar_price), show.legend = F, size = 3, col = 'lightgrey', alpha = 0.35) +
  stat_smooth(data = temp %>% dplyr::filter(!(country %in% highlight)), 
              aes(x = gdp, y = dollar_price, group = country), col = 'grey',
              formula = y~x, method = 'lm', se = F, show.legend = F) + 
  stat_smooth(data = temp %>% dplyr::filter(country %in% highlight), 
              aes(x = gdp, y = dollar_price, group = country, col = continent), 
              formula = y~x, method = 'lm', se = F, show.legend = F) + 
  geom_label(data = temp %>% dplyr::filter(country %in% highlight) %>% dplyr::group_by(country) %>% dplyr::filter(dollar_price == max(dollar_price)),
            aes(x = gdp * 0.85, y = dollar_price * 0.95, label = country),
            label.size = 0.2) + 
  facet_wrap(~region, scales = 'free') + 
  ggthemes::scale_color_gdocs()  +
  theme(panel.grid.minor = element_blank())

# ggsave('dollar_price_vs_gdp.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

# Minimum Wage
#####################################################################################

wage = read.csv('minimum_wage.csv') %>% 
  dplyr::mutate(iso_a3 = countrycode(country, 
                                     destination = 'iso3c', origin = 'country.name'),
                country = countrycode(iso_a3, 
                                      origin = 'iso3c', destination = 'cldr.short.en')) %>% 
  dplyr::select(-iso_a3) %>% 
  tidyr::drop_na()

# hours to get big mac
merge(data, wage, by = 'country') %>% 
  dplyr::filter(year == 2020) %>% 
  dplyr::mutate(hour = dollar_price / hourly_wage,
                region = ifelse(continent %in% c('Africa', 'Oceania'), 'Africa & Oceania', continent)) %>% 
  dplyr::mutate(country = forcats::fct_reorder(country, hour)) %>%
  ggplot() + 
  geom_bar(aes(x = country, y = hour, fill = continent), stat = 'identity', show.legend = F) + 
  facet_wrap(~region, scales = 'free_y') + 
  coord_flip() + 
  ggthemes::scale_fill_gdocs() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

# ggsave('working_hour.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

# hourly wage vs dollar price
temp = merge(data, wage, by = 'country') %>% 
  dplyr::filter(year == 2020) %>% 
  dplyr::mutate(hour = dollar_price / hourly_wage,
                continent = ifelse(continent %in% c('Africa', 'Oceania'), 'Africa & Oceania', continent))
  
highlight = c('India', 'Japan', 'South Korea', 'Taiwan',
              'UK', 'US', 'Israel', 'Australia', 
              'Uruguay', 'Turkey', 'Brazil')

ggplot() + 
  geom_point(data = temp %>% dplyr::filter((!country %in% highlight)),
             aes(x = hourly_wage, y = dollar_price, col = continent), shape = 21, fill = 'white', size = 5, stroke = 1.5) + 
  stat_smooth(data = temp, 
              aes(x = hourly_wage, y = dollar_price, group = 1),
              formula = y~poly(x, 2), method = 'lm', se = F, col = 'black', lwd = 1.5, fullrange = T) +
  geom_label_repel(data = temp %>% dplyr::filter(country %in% highlight),
                   aes(x = hourly_wage, y = dollar_price, label = country, col = continent), show.legend = F) +
  ggthemes::scale_color_gdocs() +
  theme(legend.position = 'top',
        panel.grid.minor = element_blank())

# ggsave('hourly_wage_vs_dollar_price.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)


