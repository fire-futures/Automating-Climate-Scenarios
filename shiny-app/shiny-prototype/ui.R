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
    menuItem(text = "Prebuilt_Scenario", tabName= "Prebuilt_Scenario", icon = icon("sun")),
    menuItem(text = "Custom_Scenario_Builder", tabName = "Custom_Scenario_Builder", icon = icon("temperature-arrow-up"))
    
  ) #END sidebarMenu
  
) #END dashboardSidebar 



# dashboard body ----
body <- dashboardBody(
  
  #set theme ----
  fresh::use_theme("fresh_theme.css"), #don't need full file path 
  
  #tabItems ----
  tabItems(
    
    #welcome tabItem ----
    tabItem(tabName = "Welcome",
            
            # left-hand column 
            column(width = 6,
                   
                   box(width = NULL,
                       
                       title = tagList(icon("fire"), 
                                       tags$strong("Constructing Climate Scenarios for RHESSys")),
                       includeMarkdown("text/intro.md"),
                       
                       # IF WE WANT TO ADD AN IMAGE, WE CAN DO SO HERE
                       # tags$img(src = "map.jpeg", #don't need to type "www/map.jpeg" shiny knows
                       #          alt = "here is some alt text",
                       #          style = "max-width: 100%;"),
                       # tags$h6(tags$em("Map Source:",
                       #                 tags$a(href = "http://www.fishcreekwatershed.org",
                       #                        "FCWO")),
                       #         style = "text-align: center;")
                       
                   ) # END left-box
                   
            ), # END left-column
            
            
            # right-hand column 
            column(width = 6,
                   
                   # top fluid row
                   fluidRow(
                     
                     box(width = NULL,
                         
                         includeMarkdown("text/citation.md")
                         
                     ) # END box
                     
                   ), # END top fluid row
                   
                   # bottom fluid row
                   fluidRow(
                     
                     box(width = NULL,
                         
                         includeMarkdown("text/disclaimer.md")
                         
                     ) # END box
                     
                   ) # END bottom fluid row
                   
            ) # END right column
            
    ), # END welcome tabItem
    
    #pre-built scenarios tabItem ----
    tabItem(tabName = "Prebuilt_Scenario",
            
            # fluidRow ----
            fluidRow(
              
              # Scenario Input Box ----
              box(width = 4,
                  
                  title = tags$strong("Choose Prebuilt Scenario"),
                  
                  # sliderInput
                  selectInput(inputId = "scenario_single_select",
                              label = "Choose Your Scenario",
                              choices = scenarios,
                              selected = NULL,
                              multiple = FALSE
                  )
                  
              ), #END Scenario box
              
            ) # END fluidRow
            
    ), # END prebuilt scenario tabItem
    
    
    #custom scenario builder tabItem ----
    tabItem(tabName = "Custom_Scenario_Builder",
            
            # fluidRow ----
            fluidRow(
              
              #left column with pickers and sliders
              column(width = 5,
              
                       
              # Model + RCP Input Box ----
              box(#title of the box
                  title = tags$strong("Choose Your GCM and RCP Assumption"),
                  width = NULL,
                  
                  # Model Picker Input
                  selectInput(inputId = "model_single_select",
                              label = "Choose Your GCM",
                              choices = models,
                              selected = NULL,
                              multiple = FALSE
                  ), # END model picker input
                  
                  # RCP Picker Input
                  selectInput(
                    inputId = "RCP_input",
                    label = "Select RCP Assumption:",
                    choices = RCP_assumptions,
                    selected = NULL,
                    multiple = FALSE
                  ), # END RCP picker input
                  
                  # Season vs Year picker input
                  selectInput(
                    inputId = "season_vs_year_input",
                    label = "Building by season or year?",
                    choices = run_type,
                    selected = "year",
                    multiple = FALSE
                  ), # END season vs year picker input
                  
                  # sampling grid cell input
                  selectInput(
                    inputId = "sampling_grid_cell_input",
                    label = "Select your sampling grid cell",
                    choices = paste0("grid_cell_", grid_list),
                    selected = NULL,
                    multiple = FALSE
                  ), # END sampling grid input
                  
                  # spatial extent input
                  selectInput(
                    inputId = "spatial_extent_input",
                    label = "Select your spatial extent (all grid cells)",
                    choices = grid_list,
                    selected = NULL,
                    multiple = TRUE
                  ), # END spatial extent input
                
              ), #END model Input box
              
              
              # Start date + Duration of Run + Sampling Window Box
              box(
                title = tags$strong("Choose your start data, length of scenario, and sampling window"),
                width = NULL,
                
                # # Start data slider ----
                dateInput(inputId = "start_date_input",
                          label = "Choose the start data for your scenario",
                          value = NULL,
                          min = NULL,
                          max = NULL,
                          format = "yyyy-mm-dd"), #END start date slider
                
                
                # Duration of scenario picker ----
                sliderInput(inputId = "scenario_duration_slider_input",
                            label = "Choose how many years you would like your scenario to last",
                            min = 3,        
                            max = 50,    
                            value = 15), #END duration of scenario slider
                
                # Sampling window picker ----
                sliderInput(inputId = "sampling_window_slider_input",
                            label = "Select your preferred sampling window",
                            min = 10,        
                            max = 75,    
                            value = 50), #END duration of scenario slider
                
                ), # END start date, duration, sampling window box ----
              
              
              # Climate variables input Box ----
              box(title = tags$strong("Adjust climate variable ranges"),
                  width = NULL,
                  
                  # sliderInput MIN TEMP
                  sliderInput(inputId = "min_temp_slider_input",
                              label = "Average Minimum Temperature (°C)",
                              min = round(min(canESM2_rcp85_time_series_summary$mean_min_temp), 2),
                              max = round(max(canESM2_rcp85_time_series_summary$mean_min_temp), 2),
                              value = c(round(min(canESM2_rcp85_time_series_summary$mean_min_temp),2),
                                        round(max(canESM2_rcp85_time_series_summary$mean_min_temp),2))),
                  
                  # sliderInput MAX TEMP
                  sliderInput(inputId = "max_temp_slider_input",
                              label = "Average Maximum Temperature (°C)",
                              min = round(min(canESM2_rcp85_time_series_summary$mean_max_temp),2),
                              max = round(max(canESM2_rcp85_time_series_summary$mean_max_temp),2),
                              value = c(round(min(canESM2_rcp85_time_series_summary$mean_max_temp),2), 
                                        round(max(canESM2_rcp85_time_series_summary$mean_max_temp),2))),
                  
                  # sliderInput PRECIPITATION
                  sliderInput(inputId = "precipitation_slider_input",
                              label = "Total Precipitation (Meters)",
                              min = round(min(canESM2_rcp85_time_series_summary$total_precip), 2),
                              max = round(max(canESM2_rcp85_time_series_summary$total_precip), 2),
                              value = c(round(min(canESM2_rcp85_time_series_summary$total_precip),2), 
                                        round(max(canESM2_rcp85_time_series_summary$total_precip), 2))),
                  
                  # sliderInput WIND
                  sliderInput(inputId = "wind_slider_input",
                              label = "Average Wind Speed (m/s)",
                              min = round(min(canESM2_rcp85_time_series_summary$mean_wind),2),
                              max = round(max(canESM2_rcp85_time_series_summary$mean_wind), 2),
                              value = c(round(min(canESM2_rcp85_time_series_summary$mean_wind),2), 
                                        round(max(canESM2_rcp85_time_series_summary$mean_wind),2))),
                  
                  # sliderInput MIN HUMIDITY
                  sliderInput(inputId = "min_humidity_slider_input",
                              label = "Average Minimum Humidity (0-1)",
                              min = round(min(canESM2_rcp85_time_series_summary$mean_min_humidity), 2),
                              max = round(max(canESM2_rcp85_time_series_summary$mean_min_humidity), 2),
                              value = c(round(min(canESM2_rcp85_time_series_summary$mean_min_humidity), 2),
                                        round(max(canESM2_rcp85_time_series_summary$mean_min_humidity, 2)))),
                  
                  #sliderInput MAX HUMIDITY
                  sliderInput(inputId = "max_humidity_slider_input",
                              label = "Average Maximum Humidity (0-1)",
                              min = round(min(canESM2_rcp85_time_series_summary$mean_max_humidity), 2),
                              max = round(max(canESM2_rcp85_time_series_summary$mean_max_humidity), 2),
                              value = c(round(min(canESM2_rcp85_time_series_summary$mean_max_humidity), 2),
                                        round(max(canESM2_rcp85_time_series_summary$mean_max_humidity, 2)))
                              )

              ), #END Climate variables input box
              
              ), #END column with pickers and sliders
              
              #Right column with visualizations
              column(width = 6,
                     
                     #Box for the histograms
                     box(title = tags$strong("Visualize available years statistics"),
                         width = NULL,
                         
                         #plotting the climate histograms
                         plotOutput(outputId = "climate_histograms") %>%
                           withSpinner(type = 1, color = "steelblue")
                         
                         ), #END box for histograms 
                         
                     
                     #Box for climate scenario time series
                     box(title = tags$strong("How your scenario looks for each climate variable"),
                         width = NULL,
                         
                         #plotting the climate variables time series 
                         plotOutput(outputId = "climate_variables_time_series") %>%
                           withSpinner(type = 1, color = "steelblue")
                        
                         ), # END box for climate scenario time series
                     
                     
                     #Box for climate criteria table
                     box(title = tags$strong("Now, finalize your climate criteria!"),
                         width = NULL,
                         
                         #plotting the climate variables time series 
                         rHandsontableOutput("myTable") %>%
                           withSpinner(type = 1, color = "steelblue")
                         
                     ), # END box for climate criteria table
                     
                     
                     
                     #Download button
                     downloadButton(outputId = "download_button", 
                                    label = "Download Scenario"),
                     
                     # TESTING OUTPUT OF CENTROIDS TABLE
                     tableOutput("centroids_table"), 
                     
                     # Display the class of centroids_of_interest
                     textOutput("centroids_class"),
                     
                     # TEST LENGTH OF DATAFRAMES FROM GET CELL DATA
                     textOutput("data_list_length"),
                     
                     #TEST WHETHER GETTING GRID CELL INFO IS WORKING
                     tableOutput("grid_cell_table_check"),
                     
                     # TEST that run_samples() is working
                     textOutput("sample_runs"),
                     
                     #TEST THAT NUMBER MATCHES TABLES IS WORKING
                     tableOutput("number_matches_table"),
                     
                     #TEST TO MAKE SURE RANDOM SAMPLES IS WORKING
                     textOutput("random_samples"),
                     
                     #TEST TO MAKE SURE CUT STITCH WORKING
                     tableOutput("ts_table")
                    
                     
                     ) #END Column 
              
            ) # END fluidRow
            
    ) # END dashboard tabItem
    
  ) # END tabItem  

) #END dashboardBody

#combine all ----
dashboardPage(header, sidebar, body)