library(dplyr)
library(rollup)
library(sparklyr)
source("../rollup/R/rollup.R")

data("web_service_data")
web_service_data %>% head

cat("\n========== Regular DataFrame Tests ==========\n\n")

# avg_pv_cnt group by (gender, age, (gender, age))
cat("Test 1: grouping_sets with total_label and total_on_top\n")
web_service_data %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age) %>% grouping_sets('gender', 'age', c('gender','age'), NA, total_label = "Total", total_on_top = TRUE) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt))

cat("\nTest 2: with_cube\n")
web_service_data %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age) %>% with_cube(total_label='Total', total_on_top = TRUE) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt))

cat("\nTest 3: grouping_sets with pivot_wider\n")
# avg_pv_cnt group by ((gender, age, product_view_cnt_cat), product_view_cnt_cat)
web_service_data %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age, product_view_cnt_cat) %>% grouping_sets('product_view_cnt_cat', c('product_view_cnt_cat', 'gender', 'age'), total_label='Total', total_on_top = TRUE) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt)) %>% pivot_wider(names_from = product_view_cnt_cat, values_from = avg_pv_cnt)

cat("\nTest 4: with_rollup\n")
# avg_pv_cnt group by (gender, age, (gender, age))
web_service_data %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age) %>% with_rollup() %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt))

cat("\n\n========== Sparklyr DataFrame Tests ==========\n\n")

# Start Spark connection
sc <- spark_connect(master = "local", app_name = "rollup_test")

# Copy data to Spark
web_service_spark <- copy_to(sc, web_service_data, "web_service_data", overwrite = TRUE)

cat("Spark DataFrame created. Row count:", sdf_nrow(web_service_spark), "\n\n")

cat("Test 1 (Spark): grouping_sets with total_label and total_on_top\n")
result1 <- web_service_spark %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age) %>% grouping_sets('gender', 'age', c('gender','age'), NA, total_label = "Total", total_on_top = TRUE) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt, na.rm=TRUE))
print(result1)

cat("\nTest 2 (Spark): with_cube\n")
result2 <- web_service_spark %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age) %>% with_cube(total_label='Total', total_on_top = TRUE) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt, na.rm=TRUE))
print(result2)

cat("\nTest 3 (Spark): grouping_sets with pivot_wider\n")
result3 <- web_service_spark %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age, product_view_cnt_cat) %>% 
  grouping_sets('product_view_cnt_cat', c('product_view_cnt_cat', 'gender', 'age'), total_label='Total', total_on_top = TRUE) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt, na.rm=TRUE)) %>% 
  pivot_wider(names_from = product_view_cnt_cat, values_from = avg_pv_cnt)
print(result3)

cat("\nTest 4 (Spark): with_rollup\n")
result4 <- web_service_spark %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age) %>% with_rollup(total_label = "Total", total_on_top = TRUE) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt, na.rm=TRUE))
print(result4)

cat("\nTest 5 (Spark): with_rollup without total options\n")
result5 <- web_service_spark %>% filter(date_id == '2024-06-30') %>% 
  group_by(gender, age) %>% with_rollup() %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt, na.rm=TRUE))
print(result5)

# Cleanup
spark_disconnect(sc)

cat("\n\n✓ All tests completed successfully!\n")
