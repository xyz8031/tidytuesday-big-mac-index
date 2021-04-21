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

# scales::show_col(scales::brewer_pal(palette = "Set1")(5))

data = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv') %>% 
  dplyr::select(date, name, dollar_price, local_price, dollar_ex, gdp_dollar) %>% 
  dplyr::mutate(name = plyr::mapvalues(name, 
                                       from = c('United States', 'United Arab Emirates'),
                                       to = c('USA', 'UAE')))

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

temp = survey_num %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  dplyr::mutate(country = forcats::fct_reorder(country, number))
  
temp = survey_num %>% 
  dplyr::filter(country != 'Euro area') %>% 
  dplyr::mutate(country = factor(country, levels = levels(temp$country))) %>% 
  dplyr::mutate(continent = countrycode(sourcevar = country,
                                     origin = "country.name",
                                     destination = "continent"),
                region = countrycode(sourcevar = country,
                                        origin = "country.name",
                                        destination = "region")) %>% 
  dplyr::mutate(continent = ifelse(continent == 'Asia', region, continent)) %>% 
  dplyr::mutate(continent = plyr::mapvalues(continent,
                                            from = c('Oceania', 'Africa', 'Europe & Central Asia', 'South Asia'),
                                            to = c('Other', 'Other', 'Other Asia', 'Other Asia')))
  # dplyr::mutate(continent = ifelse(country == 'Euro area','Europe', continent)) %>% 
  # dplyr::mutate(continent = plyr::mapvalues(continent,
  #                                           from = c('Africa', 'Oceania'),
  #                                           to = c('Other', 'Other'))) 

ggplot(temp, aes(x = year, y = country, fill = factor(number))) +
  geom_tile( col = 'white', size = 0.35, alpha = 0.85) +
  scale_x_continuous(name = NULL,
                     breaks = seq(2000, 2020, 4),
                     labels = seq(2000, 2020, 4),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL,
                   breaks = ,
                   guide = guide_axis(check.overlap = TRUE)) +
  scale_fill_manual(values=c("#d53e4f","#fdae61","#abdda4")) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        
        axis.ticks=element_line(size=0.15),
        axis.ticks.length.y = unit(0.25, 'cm'),
        
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18, margin=ggplot2::margin(5,0,5,0)),
        plot.caption = element_text(), 

        legend.position = c(0.85, 1.08),
        legend.direction = 'horizontal',
        legend.background = element_rect(fill = 'white', size = 0.3),
        legend.text = ggplot2::element_text(size=14, color="#222222")) +
  
  guides(fill = guide_legend(title="Number")) +
  labs(title = "The Number of Big Mac Index Surverys") +
  facet_wrap(~continent, scales = 'free', nrow = 2)

 # ggsave('survey_number.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

data = data %>% 
  dplyr::filter(!(name %in% missing_country$country)) %>% 
  dplyr::mutate(year = year(date)) %>% 
  dplyr::group_by(year, name) %>% 
  dplyr::summarise(dollar_price = mean(dollar_price, na.rm = T),
                   local_price = mean(local_price, na.rm = T),
                   dollar_ex = mean(dollar_ex, na.rm = T),
                   gdp_dollar = mean(gdp_dollar, na.rm = T)) %>%
  dplyr::mutate(continent = countrycode(sourcevar = name,
                                        origin = "country.name",
                                        destination = "continent")) %>%
  dplyr::mutate(continent = ifelse(name == 'Euro area','Europe', continent)) %>% 
  dplyr::select(year, name, continent, dollar_price:gdp_dollar)


# gdp
visdat::vis_miss(data)
# ggsave('missing_value_year.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

data %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(missing = sum(is.na(gdp_dollar)),
                   number = n()) %>% 
  dplyr::mutate(missing_pct = missing / number) %>% 
  ggplot() + 
  geom_bar(aes(x = year, y = missing_pct), stat = 'identity') + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 1)) + 
  xlab('') + 
  labs(title = 'GDP data Missing Percentage',
       subtitle = 'by Year') + 
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18, margin=ggplot2::margin(5,0,5,0)))
# ggsave('missing_value_country.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

temp = data %>% 
  dplyr::group_by(name, continent) %>% 
  dplyr::summarise(missing = sum(is.na(gdp_dollar)),
                   number = n()) %>% 
  dplyr::mutate(missing_pct = missing / number,
                continent = ifelse(continent %in% c('Africa', 'Oceania'), 'Africa & Oceania', continent)) 
temp$name = forcats::fct_reorder(temp$name, temp$missing_pct)
  
ggplot(temp, aes(x = name, y = missing_pct, fill = continent)) + 
  geom_bar(show.legend = F, stat = 'identity') + 
  coord_flip() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(breaks = c(0, 0.5, 1)) + 
  facet_wrap(~continent, scales = 'free_y')  +
  labs(title = 'GDP data Missing Percentage by Country',
       subtitle = 'by Country') + 
  xlab('') + 
  ggthemes::scale_fill_gdocs() +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18, margin=ggplot2::margin(5,0,5,0)))
# ggsave('plot.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

# add missing gdp data
gdp = read.csv('gdp_data.csv', header = T)
colnames(gdp)[-1] = seq(1960, 2019)

gdp = gdp %>% 
  tidyr::gather(key = 'year', value = 'gdp', -name) %>% 
  dplyr::filter(name %in% data$name)

merge(data, gdp, by = c('name','year')) %>%
  dplyr::filter(year >= 2011) %>% 
  ggplot(aes(x = gdp, y = gdp_dollar)) + 
  geom_point(col = 'grey', fill = 'lightgrey', size = 5, alpha =0.5) +
  stat_smooth(formula = y~x, method = 'lm', se= F, col = 'red', lwd = 1.2) + 
  facet_wrap(~year) + 
  labs(title = 'GDP Data Comparison',
       subtitle = "The Economist vs World Bank") + 
  xlab('GDP in Current US$') + 
  ylab('GDP in Historical US$') + 
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18, margin=ggplot2::margin(5,0,5,0)),
        plot.caption = element_text(), 
        
        legend.position = c(0.85, 1.15),
        legend.direction = 'horizontal',
        legend.background = element_rect(fill = 'white', size = 0.3),
        legend.text = ggplot2::element_text(size=14, color="#222222")) 

# ggsave('gdp_compare.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)


data = merge(data, gdp, by = c('name','year'), all.x = T) %>% 
  dplyr::mutate(gdp = ifelse(is.na(gdp), gdp_dollar, gdp)) %>% 
  dplyr::select(-gdp_dollar)

# 
p1 = data %>% dplyr::filter(name == 'Turkey') %>% 
  ggplot(aes(x = year, y = local_price)) +
  geom_line(lwd = 1.5)  +
  labs(title = 'Before') + 
  theme(panel.grid.minor = element_blank()) + 
  ylab('')

data$local_price[data$name == 'Turkey' & data$year < 2005] = data$local_price[data$name == 'Turkey' & data$year < 2005] / 1000000

p2 = data %>% dplyr::filter(name == 'Turkey') %>% 
  ggplot(aes(x = year, y = local_price)) + 
  geom_line(lwd = 1.5)  +
  labs(title = 'After') + 
  theme(panel.grid.minor = element_blank()) + 
  xlab('')

p1 + p2 + plot_annotation(title = 'The Turkish Lira')
# ggsave('turkey_currency.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)


# Price change in local currency
i = 1
for (c in unique(data$continent)) {
  
  temp = data %>% 
    dplyr::filter(!(name %in% missing_country$country) & continent == c) %>% 
    dplyr::group_by(name) %>% 
    dplyr::mutate(max = max(local_price),
                  min = min(local_price),
                  local_price = (local_price - min) / (max - min),
                  name2 = name) %>% 
    dplyr::select(-name)
  
  data %>% 
    dplyr::filter(!(name %in% missing_country$country) & continent == c) %>% 
    dplyr::group_by(name) %>% 
    dplyr::mutate(max = max(local_price),
                  min = min(local_price),
                  local_price = (local_price - min) / (max - min)) %>% 
    ggplot(aes(x = year, y = local_price)) + 
    geom_line(data = temp, aes(group=name2), col = 'lightgrey', lwd = 0.5) + 
    geom_line(col = ggthemes::gdocs_pal()(9)[i], lwd = 1.2) +
    facet_wrap(~name, scales = 'free') + 
    labs(x = '', y = '', title = 'Price of Big Mac in Local Currency Across Asia') + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          legend.position = 'none') 
  
  ggsave(glue('{c}_local_currency.png'), width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)
  
  i = i + 1
}

# Prince change in us dollar
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
    labs(x = '', y = '', title = 'Price of Big Mac in US dollar Across Asia') + 
    theme(panel.grid.minor = element_blank(),
          legend.position = 'none') 
  
  ggsave(glue('{c}_us_dollar.png'), width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)
  
  i = i + 1
}

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
  facet_wrap(~continent, scales = 'free') + 
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none')  
