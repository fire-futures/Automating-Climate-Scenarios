get_metadata_shiny <- function(criteria_list,
                               gcm, 
                               rcp, 
                               sample_cell, 
                               spatial_extent, 
                               start_date, 
                               segment_type, 
                               duration, 
                               sample_window, 
                               number_of_matches 
) {
  
  metadata <- criteria_list %>% 
    mutate(gcm = gcm) %>%
    mutate(rcp = rcp) %>%
    mutate(sample_cell = sample_cell) %>%
    mutate(spatial_extent = paste(spatial_extent, collapse = ",")) %>%
    mutate(start_date = start_date) %>%
    mutate(segment_type = segment_type) %>%
    mutate(duration = duration) %>%
    mutate(sample_window = sample_window) %>%
    mutate(number_of_matches = unlist(number_of_matches))
  
  return(metadata)
}
