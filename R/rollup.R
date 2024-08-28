
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
#' @return An object of the same class as `.data`. One grouping level will be dropped.
#' @export
setGeneric("summarize", function(object, ...) {
  standardGeneric("summarize")
})

#' Default method for `summarize`
#'
#' @param object An object.
#' @param ... Additional arguments.
#' @return An object of the same class as `.data`. One grouping level will be dropped.
#' @export
setMethod("summarize", "ANY", function(object, ...) {
  dplyr::summarize(object, ...)
})


#' Method for `summarize` on grouped_df_list
#'
#' @param object A grouped_df_list object.
#' @param ... Additional arguments.
#' @return An object of the same class as `.data`. One grouping level will be dropped.
#' @export
setMethod("summarize", signature(object = "grouped_df_list"), function(object, ...) {
  summarize_rollup(object@df_list, ...)
})

#' Generic `summarise` function
#'
#' @param object Object to be summarized.
#' @param ... Additional arguments.
#' @return An object of the same class as `.data`. One grouping level will be dropped.
#' @export
setGeneric("summarise", function(object, ...) {
  standardGeneric("summarise")
})

#' Default method for `summarise`
#'
#' @param object An object
#' @param ... Additional arguments.
#' @return An object of the same class as `.data`. One grouping level will be dropped.
#' @export
setMethod("summarise", "ANY", function(object, ...) {
  dplyr::summarise(object, ...)
})

#' Method for `summarise` on grouped_df_list
#'
#' @param object A grouped_df_list object.
#' @param ... Additional arguments.
#' @return An object of the same class as `.data.` One grouping level will be dropped.
#' @export
setMethod("summarise", signature(object = "grouped_df_list"), function(object, ...) {
  summarize_rollup(object@df_list, ...)
})

#' grouping_sets
#'
#' Compute total amounts at different group levels, producing multiple subtotals. With the 'grouping_sets' clause following 'group_by', you can aggregate multiple grouping variables in one operation. This reflects the 'GROUPING SETS' operations in 'SQL'.
#'
#' @importFrom magrittr %>%
#' @importFrom methods new
#' @importFrom utils combn
#' @import tidyr
#' @import dplyr
#' @usage grouping_sets(df, ...)
#' @param df dataframe or grouped df
#' @param ... grouping variables
#' @return A list of 'grouped_df' class. each 'grouped_df' object has a different grouping level.
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


#' with_cube
#'
#' Compute total amounts at different group levels, producing multiple subtotals. With the 'with_cube' clause following 'group_by', you can aggregate multiple grouping variables in one operation. This reflects the 'WITH CUBE' operations in 'SQL'.
#'
#' @import dplyr
#' @param grouped_df 'grouped_df' class
#'
#' @return A list of 'grouped_df' class. each 'grouped_df' object has a different grouping level.
#' @examples
#' mtcars %>% group_by(vs, am) %>% grouping_sets("vs","am",c("vs","am"))
#' mtcars %>% group_by(vs, am) %>% with_rollup()
#' mtcars %>% group_by(vs, am) %>% with_cube()
#' @export
with_cube <- function(grouped_df) {
  grouping_vars <- grouped_df %>% dplyr::group_vars()
  combinations <- unlist(lapply(seq_along(grouping_vars), function(x) combn(grouping_vars, x, simplify = FALSE)), recursive = FALSE)
  grouped_var_list <- c(combinations, NA)
  do.call(grouping_sets, c(list(grouped_df), grouped_var_list))
}

#' with_rollup
#'
#' Compute total amounts at different group levels, producing multiple subtotals. With the 'with_rollup' clause following 'group_by', you can aggregate multiple grouping variables in one operation. This reflects the 'WITH ROLLUP' operations in 'SQL'.
#'
#' @import dplyr
#' @param grouped_df 'grouped_df' class
#' @return A list of 'grouped_df' class. each 'grouped_df' object has a different grouping level.
#' @examples
#' mtcars %>% group_by(vs, am) %>% grouping_sets("vs","am",c("vs","am"))
#' mtcars %>% group_by(vs, am) %>% with_rollup()
#' mtcars %>% group_by(vs, am) %>% with_cube()
#' @export
with_rollup <- function(grouped_df) {
  grouping_vars <- grouped_df %>% dplyr::group_vars()
  grouped_var_list <- list()

  for (i in seq_along(grouping_vars)) {
    grouped_var_list[[i]] <- grouping_vars[1:(length(grouping_vars) - i + 1)]
  }

  grouped_var_list[[length(grouping_vars) + 1]] <- NA
  do.call(grouping_sets, c(list(grouped_df), grouped_var_list))
}

#' summarize_rollup
#'
#' 'summarize_rollup' aggregates each 'grouped_df' in the 'grouped_df_list' class and return the unioned aggregated results.
#'
#' @import dplyr
#' @importFrom rlang quos
#' @importFrom sparklyr sdf_bind_rows
#' @param df_list 'grouped_df_list' class
#' @param ... functions for 'summarize'
#'
#' @return An object of the same class as `.data`. The unioned aggregated result of multiple grouping levels will be dropped.
#' @export
summarize_rollup <- function(df_list, ...) {
  funcs <- rlang::quos(...)

  result_tmp <- lapply(df_list, function(x) {
    summarized_df <- dplyr::summarize(x, !!!funcs)
  })

  common_cols <- Reduce(intersect, lapply(result_tmp, colnames))

  if(inherits(df_list[[1]], "tbl_spark")) {
    if(requireNamespace("sparklyr", quietly = TRUE)) {
      binded_result <- sparklyr::sdf_bind_rows(result_tmp)
    }
    else {
      stop("sparklyr package is required for operating on Spark DataFrames.")
    }
  }
  else {
    binded_result <- dplyr::bind_rows(result_tmp)
  }

  cols <- colnames(binded_result)
  exclude_common_cols <- cols[!cols %in% common_cols]
  binded_result <- binded_result %>% select(all_of(c(exclude_common_cols, common_cols)))
  return(binded_result)
}



