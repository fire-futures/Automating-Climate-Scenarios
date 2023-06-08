#' Find all grid cells
#'
#' This function searches for all user input grid cell data (ui_df_names) in the global environment and returns a list of their names.
#'
#' @return A character vector containing the names of all user input grid cell data.
#' @export
#' @examples
#' find_all_grid_cells()
find_all_grid_cells <- function() {
  pattern <- "^grid_cell_.*"
  all_names <- ls(pattern = pattern, envir = globalenv())
  ui_df_names <<- setdiff(all_names, grep("_output$", all_names, value = TRUE))
  return(ui_df_names)
}