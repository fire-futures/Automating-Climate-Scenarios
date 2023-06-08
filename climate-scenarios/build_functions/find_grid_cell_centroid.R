#' Find centroid of grid cells
#'
#' This function maps grid cells to their corresponding centroids based on the provided grid cell IDs.
#'
#' @param ui_grid_cells A character vector containing the IDs of the grid cells to find centroids for.
#'
#' @return A data frame containing the grid cell IDs, latitude, and longitude of the centroids.
#' @export
#' @examples
#' grid_cells <- c("grid_cell_1", "grid_cell_2", "grid_cell_3")
#' find_grid_cell_centroid(grid_cells)
find_grid_cell_centroid <- function(ui_grid_cells) {
  
  caladaptpolygons <- ca_locagrid_geom()

# find the centroid in point geometry form
centroids_of_interest <- caladaptpolygons %>%
  rename(grid_cell_id = id) %>% 
  filter(grid_cell_id %in% ui_grid_cells) %>% 
  st_centroid()

# extract coordinates from the point geometry form (if we use st_coordinates in a pipe, we lose the grid_cell_id)
centroids_of_interest_coords <- st_coordinates(centroids_of_interest)

# add in the coordinates and drop the point geometry column
centroids_of_interest <<- centroids_of_interest %>% 
  mutate(lat = centroids_of_interest_coords[, "Y"]) %>% 
  mutate(lon = centroids_of_interest_coords[, "X"]) %>% 
  as.data.frame() %>% 
  select(grid_cell_id, lat, lon)
}
