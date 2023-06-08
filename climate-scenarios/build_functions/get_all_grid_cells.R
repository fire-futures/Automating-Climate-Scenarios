#' Get all grid cells
#'
#' This function takes in a list of all grid cell data dataframe names and loops through the "cut_stitch_ts" function for each grid cell.
#'
#' @param df_names A vector of grid cell names, an output from the find_all_grid_cells() function
#' @return None, calls the cut_stitch_ts() function to loop through with all the df_names
#' @examples
#' ui_grid_cells <- c(1,2)
#' get_all_grid_cells(all_grid_cells = ui_grid_cells)

get_all_grid_cells <- function(df_names,
                               ui_gcm = ui_gcm,
                               ui_rcp = ui_rcp) {
  
  # creating the root output folder that the cut_stitch_ts() function and get_metadata() function will add things into
  root_output_folder_name <<- paste0(ui_gcm, "_", ui_rcp, "_all_grid_cell_output")
  dir.create(root_output_folder_name, showWarnings = FALSE)
  
  for(i in seq_along(df_names)) {
    
    # loop's through the cut/stitch / time series function 
    cut_stitch_ts(df_names = df_names[i], root_folder = root_output_folder_name)
    
  }
  
}

