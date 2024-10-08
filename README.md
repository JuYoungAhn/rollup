

# rollup

`rollup`: A Tidy implementation of `grouping sets`, `rollup` and `cube`, which are extensions of the `group_by` clause that compute multiple `group_by` clauses in a single statement. 

## Install

```r
# From CRAN
install.packages("rollup")

# From Github
devtools::install_github("JuYoungAhn/rollup")
```
[![CRAN\_Download\_Badge](https://cranlogs.r-pkg.org/badges/rollup)](https://CRAN.R-project.org/package=rollup)


## In a Nutshell
- If you want to learn more about `rollup`, please refer to the [Tutorial](https://juyoungahn.github.io/rollup/articles/tutorial.html).
```r
mtcars %>% group_by(vs, am) %>% grouping_sets("vs","am",c("vs","am"),NA) %>% 
  summarize(n=n(), avg_mpg=mean(mpg))

mtcars %>% group_by(vs, am) %>% with_rollup() %>% 
  summarize(n=n(), avg_mpg=mean(mpg))

mtcars %>% group_by(vs, am) %>% with_cube() %>% 
  summarize(n=n(), avg_mpg=mean(mpg))

```
