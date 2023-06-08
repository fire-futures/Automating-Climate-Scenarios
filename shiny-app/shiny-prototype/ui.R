# dashboard header ----
header <- dashboardHeader(
  
  title = "Creating Climate Scenarios and RHESSys Inputs",
  titleWidth = 400
  
) # END dashboardHeader



# dashboard sidebar ----
sidebar <- dashboardSidebar(
  
  #sidebar menu showing different tabs
  sidebarMenu(
    
    menuItem(text = "Welcome", tabName = "Welcome", icon = icon("star")),
    menuItem(text = "Build", tabName = "Custom_Scenario_Builder", icon = icon("temperature-arrow-up"))
    
  ) #END sidebarMenu
  
) #END dashboardSidebar 



# dashboard body ----
body <- dashboardBody(
  
  #set theme ----
  fresh::use_theme("fresh_theme.css"),
  
  #tabItems ----
  tabItems(
    
    #welcome tabItem ----
    tabItem(tabName = "Welcome",
            
            fluidRow(
            
                column(width = 6,
                       
                       #INTRO TEXT BOX
                       box(width = NULL,
                       title = tagList(icon("fire"),
                                       tags$strong("Constructing Climate Scenarios for RHESSys")),
                       includeMarkdown("text/intro.md")
                     
                       ), #END INTRO TEXT BOX
                       
                       
                       # DISCLAIMER BOX
                       box(width = NULL,
                           includeMarkdown("text/disclaimer.md")
                       
                           ) #END DISCLAIMER BOX
                    
                     
              ), #END LEFT COLUMN
              
              column(width = 6,
                     
                     fluidRow(
                       
                       # MAP OF CAL ADAPT BOX
                       box(width = NULL,
                           title = tags$strong("Map of Cal-Adapt Grid Cells. Interact with the map to find your grid cells of interest"),
                           leafletOutput(outputId = "cal_adapt_map") %>%
                             withSpinner(type = 1, color = "steelblue")
                      
                            ), #END CAL ADAPT MAP BOX
                     
                       #CITATION BOX   
                       box(width = NULL,
                           includeMarkdown("text/citation.md")
                       ) #END CITATION BOX
                    
                     ) #END FLUID ROW
                     
              ) #END RIGHT COLUMN
            
              ) #END FLUID ROW
            
    ), #END TAB ITEM
    
    
    #custom scenario builder tabItem ----
    tabItem(tabName = "Custom_Scenario_Builder",
            
            # fluidRow ----
            fluidRow(
              
              # #left column with pickers and sliders
              # column(width = 5,
                     
                     # Model + RCP Input Box ----
                     box(#title of the box
                       title = tags$strong("Choose Your Data"),
                       width = 6,
                       height = 425,
                       
                       # Model Picker Input
                       bsTooltip(id = "model_single_select", 
                                 title = "Four priority Global Climate Models deemed as most suitable for California", 
                                 placement = "right", 
                                 trigger = "hover"),
                       selectInput(inputId = "model_single_select",
                                   label = "Select Climate Model:",
                                   choices = models,
                                   selected = NULL,
                                   multiple = FALSE
                       ), # END model picker input
                       
                       # RCP Picker Input
                       bsTooltip(id = "RCP_input", 
                                 title = "Representative Concentration Pathway (RCP): greenhouse gas concentration trajectory", 
                                 placement = "right", 
                                 trigger = "hover"),
                       selectInput(
                         inputId = "RCP_input",
                         label = "Select RCP Assumption:",
                         choices = RCP_assumptions,
                         selected = NULL,
                         multiple = FALSE
                       ), # END RCP picker input
                       
                       
                       # spatial extent input
                       bsTooltip(id = "spatial_extent_input", 
                                 title = "The first cell in your list will be the sampling grid cell. The random samples will depend on the climate criteria from this cell.", 
                                 placement = "right", 
                                 trigger = "hover"),
                       selectInput(
                         inputId = "spatial_extent_input",
                         label = "Select grid cells (the first cell in your list will be the sampling grid cell):",
                         choices = grid_list,
                         selected = NULL,
                         multiple = TRUE
                       ), # END spatial extent input
                       
                     ), #END model Input box
                     

                     # Start date + Duration of Segment + Sampling Window Box
                     box(
                       title = tags$strong("Choose Scenario Building Specifications"),
                       width = 6,
                       height = 425,
                       
                       # Season vs Year picker input
                       bsTooltip(id = "season_vs_year_input", 
                                 title = "Wet season is defined as October-April and dry season is May-September", 
                                 placement = "left", 
                                 trigger = "hover"),
                       selectInput(
                         inputId = "season_vs_year_input",
                         label = "Select segment type:",
                         choices = segment_type,
                         selected = "year",
                         multiple = FALSE
                       ), # END season vs year picker input
                       
                       # Duration of scenario picker ----
                       bsTooltip(id = "scenario_duration_slider_input", 
                                 title = "Scenario duration in number of segments", 
                                 placement = "left", 
                                 trigger = "hover"),
                       sliderInput(inputId = "scenario_duration_slider_input",
                                   label = "Select scenario duration:",
                                   min = 2,        
                                   max = 100,    
                                   value = 10), #END duration of scenario slider
                       
                       # Start year picker ----
                       bsTooltip(id = "start_year_input", 
                                 title = "All scenarios start at the beginning of the water year (October 1st)", 
                                 placement = "left", 
                                 trigger = "hover"),
                       numericInput(inputId = "start_year_input",
                                    label = "Select start year:",
                                    min = 2000,
                                    max = 2080,
                                    value = 2023),  #END start year picker
                       
                       # Sampling window picker ----
                       bsTooltip(id = "sampling_window_slider_input", 
                                 title = "Sample Window: “window” of climate segments before and after the segment of interest that are available for sampling", 
                                 placement = "left", 
                                 trigger = "hover"),
                       sliderInput(inputId = "sampling_window_slider_input",
                                   label = "Select sampling window:",
                                   min = 10,        
                                   max = 70,    
                                   value = 30), #END sampling window slider
                       
                     ), # END start date, duration, sampling window box ----
                     
            ), #END DATA AND SCENARIO INPUT ROW
            
            #CLIMATE SLIDER AND HISTOGRAMS ROW
            fluidRow(
                     
                     #Box for the histograms
                     box(title = tags$strong("Distribution of Sample Grid Cell Data"),
                         width = 12,
                         height = 500,
                         
                         #plotting the climate histograms
                         plotOutput(outputId = "climate_histograms") %>%
                           withSpinner(type = 1, color = "steelblue")
                         
                     ), #END box for histograms 
              
              ),  #END ROW FOR CLIMATE SLIDER AND HISTOGRAMS ROW
            
            
            #ROW FOR CLIMATE CRITERIA TABLE
            fluidRow(#Box for climate criteria table
              box(title = tags$strong("Select climate criteria for each segment by clicking into this table. To apply changes across rows or columns, drag values."),
                  footer = tags$strong("If building by season, all odd numbered rows will represent wet season (October–April) and all even rows will represent dry season (May-September)."),
                  width = 10,
                  
                  #plotting the climate variables time series 
                  rHandsontableOutput("myTable") %>%
                    withSpinner(type = 1, color = "steelblue")
                  
              ), # END box for climate criteria table)
              
              #display number of matches
              box(title = tags$strong("Number of available matches in each segment"),
                  width = 2,
                  
                  #showing the table with number of matches
                  tableOutput("number_matches_table") %>%
                    withSpinner(type = 1, color = "steelblue")
                  
                  ) #END number matches box
              
            ), #END ROW FOR CLIMATE CRTIERIA TABLE
            
            fluidRow(
              
              #Box for climate scenario time series
              box(title = tags$strong("How your scenario looks for each climate variable"),
                  width = 12,
                  height = 500,
                  
                  #plotting the climate variables time series 
                  plotOutput(outputId = "climate_variables_time_series") %>%
                    withSpinner(type = 1, color = "steelblue")
                  
              ), # END box for climate scenario time series
              
              
            ), #END FLUID ROW FOR CLIMATE SERIES AND HEAD OF TIME SERIES 
            
            
            tags$head(
              tags$style(HTML("
    /* Increase the size of the download button */
    .btn-download {
      font-size: 18px;
      padding: 10px 20px;
    }
    
    /* Center the download button */
    .btn-container {
      text-align: center;
      margin-top: 20px;
    }
  "))
            ),
  
  
  # Download button row
  fluidRow(
    column(
      width = 12,
      align = "center",
      
      # Download button
      div(class = "btn-container",
          downloadButton(outputId = "download_button", label = "Download Scenario", class = "btn-download")
      )
    )
    )
                     
            
    ) # END dashboard tabItem
    
  ) # END tabItem  
  
) #END dashboardBody

#combine all ----
dashboardPage(header, sidebar, body)