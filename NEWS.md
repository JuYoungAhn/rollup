# rollup 0.1.0

* Added a `pkgdown` website for package description

# rollup 0.2.0 (Development)

## New Features

* Added `total_label` parameter to `grouping_sets()`, `with_rollup()`, and `with_cube()`
  - Allows customizing the label for total/subtotal rows (default: `NULL` keeps `NA`)
  - Example: `with_rollup(total_label = "Total")`
  
* Added `total_on_top` parameter to `grouping_sets()`, `with_rollup()`, and `with_cube()`
  - When `TRUE`, moves total/subtotal rows to the top of the result (default: `FALSE`)
  - Example: `with_cube(total_label = "Grand Total", total_on_top = TRUE)`

## Improvements

* Enhanced `summarize_rollup()` to handle custom total labels and positioning
* Improved compatibility with both numeric and character grouping variables
* Maintained backward compatibility - existing code continues to work without changes
