highlight = c('China', 'Taiwan', 'Turkey', 'Israel', 'Thailand')

temp = data %>% 
  arrange(desc(dollar_price)) %>% 
  dplyr::filter((year %% 5) == 0) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(rank = row_number()) %>% 
  dplyr::select(year, continent, country, rank) %>% 
  dplyr::filter(continent == 'Asia') 
  
ggplot() + 
  geom_point(data = temp %>% dplyr::filter(!(country %in% highlight)) %>% dplyr::arrange(year) %>% dplyr::group_by(country) %>% dplyr::filter(row_number() == 1 | row_number() == max(row_number())), 
             aes(x = year, y = rank, group = country), col = 'lightgrey',
             alpha = 1, size = 4) + 
  geom_line(data = temp %>% dplyr::filter(!(country %in% highlight)), aes(x = year, y = rank, group = country), col = 'lightgrey',
            alpha = 0.5, size = 2) + 
  geom_point(data = temp %>% dplyr::filter(country %in% highlight) %>% dplyr::arrange(year) %>% dplyr::group_by(country) %>% dplyr::filter(row_number() == 1 | row_number() == max(row_number())), 
             aes(x = year, y = rank, group = country, col = continent),
             alpha = 1, size = 4) + 
  geom_line(data = temp %>% dplyr::filter(country %in% highlight), aes(x = year, y = rank, group = country, col = continent),
            alpha = 0.5, size = 2) + 
  # geom_text_repel(data = temp %>% dplyr::arrange(year) %>% dplyr::group_by(country) %>% dplyr::filter(row_number() == 1) %>% dplyr::filter(year == 2000),
  #           aes(x = 2000, y = rank, label = country), 
  #           hjust = 'left', direction = "y", nudge_x = -3, box.padding =.5) + 
  geom_text_repel(data = temp %>% dplyr::filter(country %in% highlight) %>% dplyr::arrange(desc(year)) %>% dplyr::group_by(country) %>% dplyr::filter(row_number() == 1),
            aes(x = 2020, y = rank, label = country), 
            hjust = 'right', direction = "y", nudge_x = 2, box.padding =.5) + 
  # facet_wrap(~continent) + 
  theme(panel.grid = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal') + 
  ggthemes::scale_color_gdocs() + 
  scale_y_reverse() + 
  scale_x_continuous(limits = c(1995, 2025))
  