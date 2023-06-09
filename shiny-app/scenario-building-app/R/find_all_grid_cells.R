# find_all_grid_cells <- function() {
# 
#   pattern <- "^grid_cell_.*"
#   ui_df_names <<- ls(pattern = pattern, envir = globalenv())
# 
#   #my_list <- mget(df_names)
#   return(ui_df_names)
# }



find_all_grid_cells <- function() {
  pattern <- "^grid_cell_.*"
  all_names <- ls(pattern = pattern, envir = globalenv())
  ui_df_names <<- setdiff(all_names, grep("_output$", all_names, value = TRUE))
  return(ui_df_names)
}