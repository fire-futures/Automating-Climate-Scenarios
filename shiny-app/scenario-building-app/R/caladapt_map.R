#' Generate a map with Cal Adapt polygons and grid cell IDs
#'
#' This function generates a map using the leaflet package, displaying Cal Adapt polygons and their corresponding grid cell IDs.
#'
#' @return A leaflet map object.
#' @import leaflet
#' @importFrom leaflet addProviderTiles addPolygons addLayersControl addControl setView
#' @export
caladapt_map <- function() {
  
  # cal adapt polygons and grid cell ids
  caladaptpolygons <- ca_locagrid_geom()
  
  # map criteria
  map <- leaflet() %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite", layerId = "Satelite") %>%
    addProviderTiles("CartoDB.Positron", group = "Basemap", layerId = "Default") %>%
    addPolygons(data = caladaptpolygons,
                fillColor = "blue",
                fillOpacity = 0.2,
                color = "white",
                weight = 1,
                popup = paste("ID: ", caladaptpolygons$id),
                group = "Grid Cell IDs") %>%
    addLayersControl(position = "topleft",
                     baseGroups = c("Default", "Satellite"),
                     overlayGroups = "Grid Cell IDs",
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    addControl(
      html = "Click a Grid Cell to grab its ID",
      position = "topright"
    ) %>%
    setView(lng = -119.6823, lat = 34.4208, zoom = 10)
  
  # display the map
  return(map)
}
