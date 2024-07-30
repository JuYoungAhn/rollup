library(dplyr)

#' grouped_df_list class definition
#' 
#' A class to represent a list of grouped data frames.
#' 
#' @export
setClass("grouped_df_list", representation(df_list = "list"))

#' Generic `summarize` function
#' 
#' @param object Object to be summarized.
#' @param ... Additional arguments.
#' @export
setGeneric("summarize", function(object, ...) {
  standardGeneric("summarize")
})

#' Method for `summarize` on data.frame
#' 
#' @param object A data.frame object.
#' @param ... Additional arguments.
#' @export
setMethod("summarize", signature(object = "data.frame"), function(object, ...) {
  dplyr::summarize(object, ...)
})

#' Method for `summarize` on grouped_df_list
#' 
#' @param object A grouped_df_list object.
#' @param ... Additional arguments.
#' @export
setMethod("summarize", signature(object = "grouped_df_list"), function(object, ...) {
  summarize_rollup(object@df_list, ...)
})

#' Generic `summarise` function
#' 
#' @param object Object to be summarized.
#' @param ... Additional arguments.
#' @export
setGeneric("summarise", function(object, ...) {
  standardGeneric("summarise")
})

#' Method for `summarise` on data.frame
#' 
#' @param object A data.frame object.
#' @param ... Additional arguments.
#' @export
setMethod("summarise", signature(object = "data.frame"), function(object, ...) {
  dplyr::summarise(object, ...)
})

#' Method for `summarise` on grouped_df_list
#' 
#' @param object A grouped_df_list object.
#' @param ... Additional arguments.
#' @export
setMethod("summarise", signature(object = "grouped_df_list"), function(object, ...) {
  summarize_rollup(object@df_list, ...)
})



#' Grouping Sets for R dataframe
#'
#' Compute total amounts at different group levels, producing multiple subtotals. This mirrors the GROUPING SETS operations in SQL.
#'
#' @usage grouping_sets(df, ...)
#' @param df dataframe or grouped df
#' @param ... grouping variables 
#' @return list of dataframes grouped by grouping variables
#' @examples
#' mtcars %>% group_by(vs, am) %>% grouping_sets("vs","am",c("vs","am")) 
#' mtcars %>% group_by(vs, am) %>% with_rollup() 
#' mtcars %>% group_by(vs, am) %>% with_cube() 
#' @export 
grouping_sets <- function(df, ...) {
  var_list <- list(...)
  df <- df %>% ungroup()
  dfs <- lapply(var_list, function(x) {
    if (all(is.na(x))) {
      df_grouped <- df
    } else {
      df_grouped <- df %>%
        dplyr::group_by_at(vars(x))
    }
    return(df_grouped) # list of sdf
  })
  new("grouped_df_list", df_list = dfs)
}


#' With Cube for R dataframe
#'
#' Compute total amounts at different group levels, producing multiple subtotals. This mirrors the GROUPING SETS operations in SQL.
#'
#' @param grouped_df 'grouped_df' class
#'
#' @return list of dataframes
#' @examples
#' mtcars %>% group_by(vs, am) %>% grouping_sets("vs","am",c("vs","am"))
#' mtcars %>% group_by(vs, am) %>% with_rollup() 
#' mtcars %>% group_by(vs, am) %>% with_cube() 
#' @export 
with_cube <- function(grouped_df) {
  grouping_vars <- grouped_df %>% group_vars()
  combinations <- unlist(lapply(seq_along(grouping_vars), function(x) combn(grouping_vars, x, simplify = FALSE)), recursive = FALSE)
  grouped_var_list <- c(combinations, NA)
  do.call(grouping_sets, c(list(grouped_df), grouped_var_list))
}

#' With Rollup for R dataframes
#'
#' Compute total amounts at different group levels, producing multiple subtotals. This mirrors the GROUPING SETS operations in SQL.
#'
#' @param grouped_df 'grouped_df' class
#' 
#' @return list of dataframes
#' @examples
#' mtcars %>% group_by(vs, am) %>% grouping_sets("vs","am",c("vs","am")) 
#' mtcars %>% group_by(vs, am) %>% with_rollup()
#' mtcars %>% group_by(vs, am) %>% with_cube() 
#' @export
with_rollup <- function(grouped_df) {
  grouping_vars <- grouped_df %>% group_vars()
  grouped_var_list <- list()
  
  for (i in seq_along(grouping_vars)) {
    grouped_var_list[[i]] <- grouping_vars[1:(length(grouping_vars) - i + 1)]
  }
  
  grouped_var_list[[length(grouping_vars) + 1]] <- NA
  do.call(grouping_sets, c(list(grouped_df), grouped_var_list))
} 

#' Summarize returns of Grouping Sets
#'
#' S4 method of 'summarize' for class 'grouped_df_list'
#'
#' @param df_list list of 'grouped_df' class
#' @param ... functions for 'summarize'
#'
#' @return summarized dataframe
#' @export 
summarize_rollup <- function(df_list, ...) {  
  funcs <- rlang::quos(...)  

  result_tmp <- lapply(df_list, function(x) {
    summarized_df <- dplyr::summarize(x, !!!funcs)
  })
  
  common_cols <- Reduce(intersect, lapply(result_tmp, colnames))
  
  if(inherits(df_list[[1]], "tbl_spark")) {
    binded_result <- sdf_bind_rows(result_tmp)
  }
  else {
    binded_result <- dplyr::bind_rows(result_tmp)
  }

  cols <- colnames(binded_result)
  exclude_common_cols <- cols[!cols %in% common_cols]
  binded_result <- binded_result %>% select(all_of(c(exclude_common_cols, common_cols)))
  return(binded_result)
} 



