library(fresh)

create_theme(
  
  adminlte_color(
    light_blue = "#0a1d3d"
  ),
  
  adminlte_global(
    content_bg = "#E7B5B5"
  ),
  
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#57F",
    dark_hover_bg = "darkorchid",
    dark_color = "goldenrod"
  ),
  
  output_file = "shiny-dashboard/www/fresh_theme.css"
  
)
