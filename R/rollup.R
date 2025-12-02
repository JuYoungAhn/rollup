
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
  summarize_rollup(object, ...)
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
  summarize_rollup(object, ...)
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
#' @usage grouping_sets(df, ..., total_label = NULL, total_on_top = FALSE)
#' @param df dataframe or grouped df
#' @param ... grouping variables
#' @param total_label Character string to use for total rows instead of NA (default: NULL keeps NA)
#' @param total_on_top Logical. If TRUE, total rows appear at the top of the result (default: FALSE)
#' @return A list of 'grouped_df' class. each 'grouped_df' object has a different grouping level.
#' @examples
#' mtcars %>% group_by(vs, am) %>% grouping_sets("vs","am",c("vs","am"))
#' mtcars %>% group_by(vs, am) %>% with_rollup()
#' mtcars %>% group_by(vs, am) %>% with_cube()
#' mtcars %>% group_by(vs, am) %>% with_rollup(total_label = "Total", total_on_top = TRUE)
#' @export
grouping_sets <- function(df, ..., total_label = NULL, total_on_top = FALSE) {
  # Get only unnamed arguments (grouping variables)
  dots <- rlang::enquos(...)
  named_args <- names(dots)
  
  # If dots has names, it means named arguments were passed incorrectly
  # Filter to keep only unnamed or properly named grouping sets
  var_list <- lapply(dots, function(x) {
    val <- rlang::eval_tidy(x)
    if(is.character(val) || is.null(val) || (length(val) == 1 && is.na(val))) {
      return(val)
    }
    return(val)
  })
  
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
  obj <- new("grouped_df_list", df_list = dfs)
  attr(obj, "total_label") <- total_label
  attr(obj, "total_on_top") <- total_on_top
  return(obj)
}


#' with_cube
#'
#' Compute total amounts at different group levels, producing multiple subtotals. With the 'with_cube' clause following 'group_by', you can aggregate multiple grouping variables in one operation. This reflects the 'WITH CUBE' operations in 'SQL'.
#'
#' @import dplyr
#' @param grouped_df 'grouped_df' class
#' @param total_label Character string to use for total rows instead of NA (default: NULL keeps NA)
#' @param total_on_top Logical. If TRUE, total rows appear at the top of the result (default: FALSE)
#'
#' @return A list of 'grouped_df' class. each 'grouped_df' object has a different grouping level.
#' @examples
#' mtcars %>% group_by(vs, am) %>% grouping_sets("vs","am",c("vs","am"))
#' mtcars %>% group_by(vs, am) %>% with_rollup()
#' mtcars %>% group_by(vs, am) %>% with_cube()
#' mtcars %>% group_by(vs, am) %>% with_cube(total_label = "Total", total_on_top = TRUE)
#' @export
with_cube <- function(grouped_df, total_label = NULL, total_on_top = FALSE) {
  grouping_vars <- grouped_df %>% dplyr::group_vars()
  combinations <- unlist(lapply(seq_along(grouping_vars), function(x) combn(grouping_vars, x, simplify = FALSE)), recursive = FALSE)
  grouped_var_list <- c(combinations, NA)
  do.call(grouping_sets, c(list(grouped_df), grouped_var_list, 
                            list(total_label = total_label, total_on_top = total_on_top)))
}

#' with_rollup
#'
#' Compute total amounts at different group levels, producing multiple subtotals. With the 'with_rollup' clause following 'group_by', you can aggregate multiple grouping variables in one operation. This reflects the 'WITH ROLLUP' operations in 'SQL'.
#'
#' @import dplyr
#' @param grouped_df 'grouped_df' class
#' @param total_label Character string to use for total rows instead of NA (default: NULL keeps NA)
#' @param total_on_top Logical. If TRUE, total rows appear at the top of the result (default: FALSE)
#' @return A list of 'grouped_df' class. each 'grouped_df' object has a different grouping level.
#' @examples
#' mtcars %>% group_by(vs, am) %>% grouping_sets("vs","am",c("vs","am"))
#' mtcars %>% group_by(vs, am) %>% with_rollup()
#' mtcars %>% group_by(vs, am) %>% with_cube()
#' mtcars %>% group_by(vs, am) %>% with_rollup(total_label = "Total", total_on_top = TRUE)
#' @export
with_rollup <- function(grouped_df, total_label = NULL, total_on_top = FALSE) {
  grouping_vars <- grouped_df %>% dplyr::group_vars()
  grouped_var_list <- list()

  for (i in seq_along(grouping_vars)) {
    grouped_var_list[[i]] <- grouping_vars[1:(length(grouping_vars) - i + 1)]
  }

  grouped_var_list[[length(grouping_vars) + 1]] <- NA
  do.call(grouping_sets, c(list(grouped_df), grouped_var_list, 
                            list(total_label = total_label, total_on_top = total_on_top)))
}

#' summarize_rollup
#'
#' 'summarize_rollup' aggregates each 'grouped_df' in the 'grouped_df_list' class and return the unioned aggregated results.
#'
#' @import dplyr
#' @importFrom rlang quos
#' @importFrom sparklyr sdf_bind_rows
#' @param object 'grouped_df_list' class or df_list
#' @param ... functions for 'summarize'
#'
#' @return An object of the same class as `.data`. The unioned aggregated result of multiple grouping levels will be dropped.
#' @export
summarize_rollup <- function(object, ...) {
  funcs <- rlang::quos(...)
  
  # Handle both grouped_df_list object and plain list
  if(inherits(object, "grouped_df_list")) {
    total_label <- attr(object, "total_label")
    total_on_top <- attr(object, "total_on_top")
    df_list <- object@df_list
  } else {
    # Backwards compatibility - object is df_list
    df_list <- object
    total_label <- NULL
    total_on_top <- FALSE
  }
  
  if(is.null(total_on_top)) total_on_top <- FALSE

  result_tmp <- lapply(df_list, function(x) {
    summarized_df <- dplyr::summarize(x, !!!funcs)
  })

  common_cols <- Reduce(intersect, lapply(result_tmp, colnames))
  exclude_common_cols_list <- lapply(result_tmp, function(x) {
    cols <- colnames(x)
    cols[!cols %in% common_cols]
  })
  all_exclude_cols <- unique(unlist(exclude_common_cols_list))

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
  binded_result <- binded_result %>% ungroup() %>% select(all_of(c(exclude_common_cols, common_cols)))
  
  # Apply total_label if specified - AFTER binding (bind_rows adds NA for missing columns)
  if(!is.null(total_label)) {
    for(col in all_exclude_cols) {
      if(col %in% colnames(binded_result)) {
        # Store original NA positions before converting to character
        na_positions <- is.na(binded_result[[col]])
        # Convert to character
        binded_result[[col]] <- as.character(binded_result[[col]])
        # Replace only the original NA positions with total_label
        binded_result[[col]][na_positions] <- total_label
      }
    }
  }
  
  # Move total rows to top if total_on_top is TRUE
  if(total_on_top && !is.null(total_label) && length(exclude_common_cols) > 0) {
    # Identify total rows (rows where all grouping columns equal total_label)
    checks <- sapply(exclude_common_cols, function(col) {
      binded_result[[col]] == total_label
    })
    if(is.matrix(checks)) {
      is_total_row <- rowSums(checks, na.rm = TRUE) == length(exclude_common_cols)
    } else {
      is_total_row <- checks
    }
    
    if(sum(is_total_row, na.rm = TRUE) > 0) {
      total_rows <- binded_result[which(is_total_row), , drop = FALSE]
      other_rows <- binded_result[which(!is_total_row), , drop = FALSE]
      binded_result <- bind_rows(total_rows, other_rows)
    }
  } else if(total_on_top && length(exclude_common_cols) > 0) {
    # If total_on_top but no total_label, move NA rows to top
    checks <- sapply(exclude_common_cols, function(col) {
      is.na(binded_result[[col]])
    })
    if(is.matrix(checks)) {
      is_total_row <- rowSums(checks, na.rm = TRUE) == length(exclude_common_cols)
    } else {
      is_total_row <- checks
    }
    
    if(sum(is_total_row, na.rm = TRUE) > 0) {
      total_rows <- binded_result[which(is_total_row), , drop = FALSE]
      other_rows <- binded_result[which(!is_total_row), , drop = FALSE]
      binded_result <- bind_rows(total_rows, other_rows)
    }
  }
  
  return(binded_result)
}



