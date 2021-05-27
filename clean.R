library(countrycode)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(patchwork)
library(glue)
library(forcats)

setwd('~/Documents/Github/tidytuesday-big-mac-index/')
theme_set(theme_minimal(base_family = 'Raleway', base_size = 10))

# scales::show_col(scales::brewer_pal(palette = "Set1")(5))

data = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')
str(data)

data = data %>% 
  dplyr::select(date, name, iso_a3, dollar_price, local_price, gdp_dollar)
  # dplyr::mutate(name = plyr::mapvalues(name, 
  #                                      from = c('United States', 'United Arab Emirates'),
  #                                      to = c('USA', 'UAE')))

head(data, 5) %>% knitr::kable()
skimr::skim(data)

# 國家、州名稱
data = data %>% 
  # dplyr::filter(iso_a3 != 'EUZ') %>% 
  dplyr::mutate(country = countrycode(iso_a3, origin = 'iso3c', destination = 'cldr.short.en'),
                continent = countrycode(iso_a3, origin = "iso3c", destination = "continent")) %>% 
  dplyr::mutate(country = ifelse(iso_a3 == 'EUZ', 'Euro Zone', country),
                continent = ifelse(iso_a3 == 'EUZ', 'Europe', continent)) %>% 
  dplyr::select(-name, -iso_a3)

# 調查數量
survey_num = data %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, country) %>% 
  tally() %>% 
  tidyr::spread(country, n, fill = 0) %>% 
  tidyr::gather(key = 'country', value = 'number', -year)

missing_country = survey_num %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(missing = sum(number == 0)) %>% 
  dplyr::filter(missing >= 12)

temp = survey_num %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(number = sum(number)) %>% 
  dplyr::mutate(country = forcats::fct_reorder(country, number))

temp = survey_num %>% 
  dplyr::mutate(country = factor(country, levels = levels(temp$country)),
                continent = countrycode(sourcevar = country,
                                        origin = "country.name",
                                        destination = "continent"),
                region = countrycode(sourcevar = country,
                                     origin = "country.name",
                                     destination = "region")) %>% 
  dplyr::mutate(continent = ifelse(continent == 'Asia', region, continent),
                continent = ifelse(country == 'Euro Zone', 'Europe', continent)) %>% 
  dplyr::mutate(continent = plyr::mapvalues(continent,
                                            from = c('Oceania', 'Africa', 'Europe & Central Asia', 'South Asia', 'East Asia & Pacific'),
                                            to = c('Other', 'Other', 'Other East', 'Other East', 'Far East')))

temp %>% 
  dplyr::mutate(country = forcats::fct_reorder(country, number)) %>% 
  ggplot(aes(x = year, y = country, fill = factor(number))) +
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
  dplyr::filter(!(country %in% missing_country$country)) %>% 
  dplyr::mutate(year = year(date)) %>% 
  dplyr::group_by(year, continent, country) %>% 
  dplyr::summarise(dollar_price = mean(dollar_price, na.rm = T),
                   local_price = mean(local_price, na.rm = T),
                   dollar_ex = mean(dollar_ex, na.rm = T),
                   gdp_dollar = mean(gdp_dollar, na.rm = T)) %>% 
  dplyr::select(year, continent, country, dollar_price:gdp_dollar)


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
# ggsave('missing_value_year.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

temp = data %>% 
  dplyr::group_by(country, continent) %>% 
  dplyr::summarise(missing = sum(is.na(gdp_dollar)),
                   number = n()) %>% 
  dplyr::mutate(missing_pct = missing / number,
                continent = ifelse(continent %in% c('Africa', 'Oceania'), 'Africa & Oceania', continent)) 
temp$country = forcats::fct_reorder(temp$country, temp$missing_pct)
  
ggplot(temp, aes(x = country, y = missing_pct, fill = continent)) + 
  geom_bar(show.legend = F, stat = 'identity') + 
  coord_flip() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(breaks = c(0, 0.5, 1)) + 
  facet_wrap(~continent, scales = 'free_y')  +
  labs(title = 'GDP data Missing Percentage',
       subtitle = 'by Country') + 
  xlab('') + 
  ggthemes::scale_fill_gdocs() +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18, margin=ggplot2::margin(5,0,5,0)))
# ggsave('missing_value_country.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

# add missing gdp data
gdp = read.csv('gdp_data.csv', header = T)
colnames(gdp)[-1] = seq(1960, 2019)

gdp = gdp %>% 
  tidyr::gather(key = 'year', value = 'gdp', -name) %>% 
  dplyr::filter(name %in% data$country)

merge(data, gdp, by.x = c('country','year'), by.y = c('name', 'year')) %>%
  dplyr::filter(year >= 2011) %>% 
  ggplot(aes(x = gdp_dollar, y = gdp)) + 
  geom_point(col = 'grey', fill = 'lightgrey', size = 5, alpha =0.5) +
  stat_smooth(formula = y~x, method = 'lm', se= F, col = 'red', lwd = 1.2, fullrange = T) + 
  facet_wrap(~year) + 
  labs(title = 'GDP Data Comparison',
       subtitle = "The Economist vs World Bank") + 
  xlab('GDP in Historical US$') + 
  ylab('GDP in Current US$') + 
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

merge(data, gdp, by.x = c('country','year'), by.y = c('name', 'year'), all.x = T)
naniar::vis_miss(temp)
# ggsave('missing_value_gdp.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

temp = merge(data, gdp, by.x = c('country','year'), by.y = c('name', 'year')) %>%
  dplyr::filter(year >= 2011)
model = lm(gdp~ gdp_dollar + year, data = temp)

data = merge(data, gdp, by.x = c('country','year'), by.y = c('name', 'year'), all.x = T) 
data$gdp[is.na(data$gdp)] = predict(model, data[is.na(data$gdp), ])
data = data %>% dplyr::select(-gdp_dollar)

naniar::vis_miss(data)
# ggsave('missing_value_gdp.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

# Turkish Lira
data %>% 
  dplyr::filter(continent == 'Asia') %>% 
  ggplot() + 
  geom_line(aes(x = year, y = local_price)) + 
  facet_wrap(~country, scales = 'free_y') +
  theme(panel.grid.minor = element_blank())
# ggsave('pre_analysis.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

p1 = data %>% dplyr::filter(country == 'Turkey') %>% 
  ggplot(aes(x = year, y = local_price)) +
  geom_line(lwd = 1.5)  +
  labs(title = 'Before') + 
  theme(panel.grid.minor = element_blank()) + 
  ylab('')

data$local_price[data$country == 'Turkey' & data$year < 2005] = data$local_price[data$country == 'Turkey' & data$year < 2005] / 1000000

p2 = data %>% dplyr::filter(country == 'Turkey') %>% 
  ggplot(aes(x = year, y = local_price)) + 
  geom_line(lwd = 1.5)  +
  labs(title = 'After') + 
  theme(panel.grid.minor = element_blank()) + 
  xlab('')

p1 + p2 + plot_annotation(title = 'The Turkish Lira')
# ggsave('turkey_currency.png', width = 16, height = 9, units = 'in', dpi = 500, scale = 0.6)

data = data %>% 
  dplyr::select(continent, country, year, dollar_price, local_price, gdp)

knitr::kable(head(data, 5))
str(data)

write.csv(data, 'clean_data.csv', row.names = F)


