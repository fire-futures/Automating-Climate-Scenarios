find_all_grid_cells <- function() {
  pattern <- "^grid_cell_.*"
  all_names <- ls(pattern = pattern, envir = globalenv())
  ui_df_names <<- setdiff(all_names, grep("_output$", all_names, value = TRUE))
  return(ui_df_names)
}