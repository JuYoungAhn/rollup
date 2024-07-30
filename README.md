

# rollup

Grouping Sets, With Rollup and With Cube implementation for R dataframe

## Install


```r
library(devtools)
devtools::install_github("JuYoungAhn/rollup")
```

## Usage


```r
library(rollup)
data("web_service_data")
web_service_data %>% head
#> # A tibble: 6 × 6
#>   date_id       id gender age   page_view_cnt product_view_cnt_cat
#>   <chr>      <dbl> <chr>  <fct>         <dbl> <fct>               
#> 1 2024-06-24    19 M      40                0 60%                 
#> 2 2024-06-24    34 M      40                5 70%                 
#> 3 2024-06-24    44 F      50               12 100%                
#> 4 2024-06-24    57 M      60               87 20%                 
#> 5 2024-06-24    65 F      50                1 100%                
#> 6 2024-06-24    86 F      40                3 90%
```

### grouping_sets

-   group by multiple columns in one line


```r
# avg_pv_cnt group by (gender, age, (gender, age))
web_service_data %>% filter(date_id == '2024-06-30' & gender != "N") %>% 
  group_by(gender, age) %>% grouping_sets('gender', 'age', c('gender','age')) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt))
#> `summarise()` has grouped output by 'gender'. You can override using the `.groups` argument.
#> # A tibble: 20 × 3
#>    gender age   avg_pv_cnt
#>    <chr>  <fct>      <dbl>
#>  1 F      <NA>       2.28 
#>  2 M      <NA>       1.92 
#>  3 <NA>   10         1.61 
#>  4 <NA>   20         3.01 
#>  5 <NA>   30         2.23 
#>  6 <NA>   40         1.77 
#>  7 <NA>   50         1.44 
#>  8 <NA>   60         2.30 
#>  9 F      10         2.33 
#> 10 F      20         2.86 
#> 11 F      30         2.67 
#> 12 F      40         2.33 
#> 13 F      50         2.24 
#> 14 F      60         1.48 
#> 15 M      10         0.92 
#> 16 M      20         3.19 
#> 17 M      30         1.91 
#> 18 M      40         1.31 
#> 19 M      50         0.907
#> 20 M      60         2.99

# avg_pv_cnt group by ((gender, age, product_view_cnt_cat), product_view_cnt_cat)
web_service_data %>% filter(date_id == '2024-06-30' & gender != "N") %>% 
  group_by(gender, age, product_view_cnt_cat) %>% grouping_sets('product_view_cnt_cat', c('product_view_cnt_cat', 'gender','age')) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt)) %>% pivot_wider(names_from = product_view_cnt_cat, values_from = avg_pv_cnt)
#> `summarise()` has grouped output by 'product_view_cnt_cat', 'gender'. You can override using the `.groups` argument.
#> # A tibble: 13 × 11
#>    gender age       X `20%` `40%` `50%` `60%` `70%` `80%` `90%` `100%`
#>    <chr>  <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
#>  1 <NA>   <NA>  1.46  1.84   2.02 2.31   2.72  2.89  2.8   3.79   2.82
#>  2 F      10    1.4   2      1.4  2.67   4    NA    NA     4     NA   
#>  3 F      20    0     3.5    2.08 2.29   3.83  2.57  3.45  4.83   2.25
#>  4 F      30    0.833 2.5    4.5  2.88   3     1.75  3.5   3      3.17
#>  5 F      40    1.33  1.9    2.7  2.2    1.22  3     3.38  4      2   
#>  6 F      50    0.462 1.5    2    2.5    1.2   4     2.5   5.33   3.5 
#>  7 F      60    1.19  1.71   1    1.33   3     3     1.5   2      3   
#>  8 M      10    0.375 0.833  1.14 3      1     0    NA    NA     NA   
#>  9 M      20    1.14  3.17   3.16 3.55   4.5   3    NA     3.5    7   
#> 10 M      30    0.824 1.62   1.31 2.7    3.38  2.5   1.86  3.5   NA   
#> 11 M      40    0.889 0.933  2.06 0.833  1.88  3.25  1.6   1.67  NA   
#> 12 M      50    0.562 1.07   1.06 2.6    2     0     0.5   0     NA   
#> 13 M      60    3.06  2.69   4    3.5    0     8     2     1     NA
```

### with_cube

-   with_cube add all possible combinations of grouping variables
-   easily add row sum and column sum in cross table


```r
web_service_data %>% filter(date_id == '2024-06-30' & gender != "N") %>% 
  group_by(gender, age) %>% with_cube() %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt)) %>% pivot_wider(names_from = age, values_from = avg_pv_cnt)
#> `summarise()` has grouped output by 'gender'. You can override using the `.groups` argument.
#> # A tibble: 3 × 8
#>   gender  `NA`  `10`  `20`  `30`  `40`  `50`  `60`
#>   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 F       2.28  2.33  2.86  2.67  2.33 2.24   1.48
#> 2 M       1.92  0.92  3.19  1.91  1.31 0.907  2.99
#> 3 <NA>    2.08  1.61  3.01  2.23  1.77 1.44   2.30

# with_cube equals to grouping_sets with all possible combinations
# web_service_data %>% filter(date_id == '2024-06-30' & gender != "N") %>% 
#   group_by(gender, age) %>% grouping_sets("gender","age",c("gender","age"), NA) %>% 
#   summarize(avg_pv_cnt = mean(page_view_cnt)) %>% pivot_wider(names_from = age, values_from = avg_pv_cnt)
```

### with_rollup

-   easily add all possible combinations of grouping variables in descending order
-   calculate group sum and total sum (DAU and MAU in this example)


```r
web_service_data %>% 
  group_by(date_id) %>% with_rollup() %>% 
  summarize(user_cnt = n_distinct(if_else(page_view_cnt > 0, id, NA)))
#> # A tibble: 31 × 2
#>    date_id    user_cnt
#>    <chr>         <int>
#>  1 2024-06-01      644
#>  2 2024-06-02      615
#>  3 2024-06-03      700
#>  4 2024-06-04      710
#>  5 2024-06-05      706
#>  6 2024-06-06      637
#>  7 2024-06-07      694
#>  8 2024-06-08      642
#>  9 2024-06-09      622
#> 10 2024-06-10      706
#> # ℹ 21 more rows

# with_rollup equals to grouping_sets with all possible combinations in descending order
# web_service_data %>%
#   group_by(date_id) %>% grouping_sets("date_id", NA) %>%
#   summarize(user_cnt = n_distinct(if_else(page_view_cnt > 0, id, NA)))
```

### sparklyr

-   All functions are also available in sparklyr (spark dataframe)


```r
# example usage with Spark DataFrame
library(sparklyr)
sc <- spark_connect(master = "local")
#> Re-using existing Spark connection to local
sdf <- copy_to(sc, web_service_data, "web_service_data", overwrite = TRUE)

# grouping_sets on spark dataframe
sdf %>%
  group_by(gender, age) %>%
  with_rollup() %>%
  summarize(avg_pv_cnt = mean(page_view_cnt)) %>% collect()
#> `summarise()` has grouped output by "gender". You can override using the `.groups` argument.
#> `summarise()` has grouped output by "gender". You can override using the `.groups` argument.
#> # A tibble: 17 × 3
#>    gender age   avg_pv_cnt
#>    <chr>  <chr>      <dbl>
#>  1 M      40          2.32
#>  2 M      20          3.52
#>  3 F      20          3.83
#>  4 M      60          2.80
#>  5 M      30          4.16
#>  6 N      10          1.41
#>  7 F      30          3.23
#>  8 F      10          2.59
#>  9 F      50          2.79
#> 10 F      40          2.76
#> 11 M      50          1.46
#> 12 M      10          1.12
#> 13 F      60          1.75
#> 14 N      <NA>        1.41
#> 15 M      <NA>        2.65
#> 16 F      <NA>        2.80
#> 17 <NA>   <NA>        2.61
```
