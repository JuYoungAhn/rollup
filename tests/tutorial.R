library(dplyr)
library(rollup)

data("web_service_data")
web_service_data %>% head

# avg_pv_cnt group by (gender, age, (gender, age))
web_service_data %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age) %>% grouping_sets('gender', 'age', c('gender','age')) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt))

web_service_data %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age) %>% grouping_sets('gender', 'age', c('gender','age'))

# avg_pv_cnt group by ((gender, age, product_view_cnt_cat), product_view_cnt_cat)
web_service_data %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age, product_view_cnt_cat) %>% grouping_sets('product_view_cnt_cat', c('product_view_cnt_cat', 'gender', 'age')) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt)) %>% pivot_wider(names_from = product_view_cnt_cat, values_from = avg_pv_cnt)

# avg_pv_cnt group by (gender, age, (gender, age))
web_service_data %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age) %>% with_rollup() %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt))
