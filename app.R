# list of libraries
packages <- c(
  # basic packages
  "tidyverse",           # summary of basic r packages
  "shiny",               # basic shiny functions
  "shinydashboard",      # shiny dashboard package
  "shinyWidgets",        # date/ time picker
  "htmltools",           # html styling options
  "lubridate",           # date and time manipulation
  "plotly",              # 3d plot
  "leaflet",             # maps
  "pracma",              # distance calculation between geographical points
  "bslib",               # bootstrap ui toolkit
  "bsicons",             # bootstrap icons

  
  #api call
  "reticulate",          # run python commands in r
  "stars",               # data file access
  "ncdf4"               # marine data files
)

# loading the installed packages
for(pkg in packages){
  library(pkg, character.only = TRUE)
}


# global functions: -------------------------------------------------------
# set working directory
directory <- "/var/lib/weclo/temp"


# api call function ------------------------------------------------------
  
# import python package for handling copernicus marine data
use_virtualenv("/var/lib/weclo//copernicusmarine", required = TRUE)
CopernicusMarine <- import("copernicusmarine")


# wind direction function -------------------------------------------------

# wind direction function to calc. wind direction based on wind degrees
degrees_to_direction <- function(degree) {
  if (is.na(degree)) {return("unkown")            # in case of Na's in data
  } else 
    {
  if (degree >= 348.75 || degree < 11.25) {
    return("N")
  } else if (degree >= 11.25 && degree < 33.75) {
    return("NNE")
  } else if (degree >= 33.75 && degree < 56.25) {
    return("NE")
  } else if (degree >= 56.25 && degree < 78.75) {
    return("ENE")
  } else if (degree >= 78.75 && degree < 101.25) {
    return("E")
  } else if (degree >= 101.25 && degree < 123.75) {
    return("ESE")
  } else if (degree >= 123.75 && degree < 146.25) {
    return("SE")
  } else if (degree >= 146.25 && degree < 168.75) {
    return("SSE")
  } else if (degree >= 168.75 && degree < 191.25) {
    return("S")
  } else if (degree >= 191.25 && degree < 213.75) {
    return("SSW")
  } else if (degree >= 213.75 && degree < 236.25) {
    return("SW")
  } else if (degree >= 236.25 && degree < 258.75) {
    return("WSW")
  } else if (degree >= 258.75 && degree < 281.25) {
    return("W")
  } else if (degree >= 281.25 && degree < 303.75) {
    return("WNW")
  } else if (degree >= 303.75 && degree < 326.25) {
    return("NW")
  } else {
    return("NNW")
    }
  }
}


# Ui section --------------------------------------------------------------

ui <- page_navbar(
  theme = bs_theme(preset = "shiny"),
  lang = "en",
  title = tags$strong("Welcome to WECLO"),

  tags$head(
    tags$script(
      HTML("
          $(document).ready(function() {$('#custom_datepicker').css('height', '47px');});
          ")
    )
  ),

  tags$style(HTML("
    .custom-card-spacing {
      margin: -4px;  /* Adjust the value as needed */
    }
  ")),
  
  tags$style(HTML("
    .custom-text-margin-left-right {
      margin-left: 20px;  /* Adjust the value as needed */
      margin-right: 20px; /* Added right margin */
    }
  ")),
  
  tags$style(HTML("
    .custom-text-margin-right {
      margin-right: 20px; /* Added right margin */
    }
  ")),
  

  tags$style(HTML("
    .bold-heading {
        font-weight: bold; /* bold text for headings */
    }
  ")),
  
  tags$style(HTML("
    .popover .irs {
       margin-left: 13px ; /* adjusting margin of popover body */
       margin-right: 13px ;
    }
  ")),
  
  
  nav_spacer(),
  
  # navigation menu including natural and simulated data selection
  nav_menu(
    title = "Data Selection",
    value = "Data Selection",
    align = c("left", "right"),
    
    tabPanel(
      title = "Natural Wave Data",
      value = "natural_wave_data",
      
      
      page_sidebar(
        #actual sidebar content
        sidebar = sidebar(
        width = 350,
        gap = 15,
        title = h4("Date & Time:"),
        #tags$br(),
         airDatepickerInput(inputId = "custom_datepicker",
                                 placeholder = "Please select a Date or Time period",
                                 multiple = 2,
                                 value = c("2024-01-01 00:00:00", "2024-01-05 00:00:00"),
                                 minDate = "2021-10-01 06:00:00",
                                 clearButton = TRUE,
                                 timepicker = TRUE,
                                 todayButton = TRUE,
                                 addon = "none",
                                 position = "bottom right",
                                 range = TRUE,
                                 toggleSelected = FALSE,
                                 timepickerOpts = timepickerOptions(timeFormat = "HH:mm:00", hoursStep = 3, minutesStep = 60)),
         actionButton(inputId = "start_api_call", label = "Start API Call", class = "btn-success btn-custom"),
         downloadButton("download_csv_nat_wave", "Download CSV"),
        ),
      
      
        # map card for natural wave data selection
        card(
          min_height = "200px",
          max_height = "600px",
          full_screen = FALSE,
          card_header("Area and Date Selection", 
                      popover(
                        title = "Plot Controls:",
                        id = "plot_popover",
                        bs_icon("gear"),
                        uiOutput("timeSlider")
                      ),
                      class = "d-flex justify-content-between align-items-center"),
          
          #map output + text + button
          leafletOutput("map"),
          fluidRow(
            column( width = 12,
            htmlOutput("map_text")),
          )
        ),  
        
        # data plots
        layout_columns(
          # natural wave plot
          card(
            height = "280px",
            full_screen = TRUE,
            card_header("Natural Wave Landscape"),
            uiOutput("natural_wave_output", fill = TRUE)
            #plotlyOutput("natural_wave_plot")
          ), 
          
          # natural wind direction plot
          card(
            height = "280px",
            full_screen = TRUE,
            card_header("Wind-Wave Direction"),
            uiOutput("natural_wind_output", fill = TRUE)
            #plotlyOutput("natural_wind_directions")
          ),
        ),
      ),  
    ), 
    
    tabPanel(
          title = "Simulated Wave Data",
          value = "simulated_wave_data",
          
          # sidebar page
          page_sidebar(
            #actual sidebar content
            sidebar = sidebar(
            width = 350,
            gap = 1,
            title = h4("Input Parameters:"),
            #tags$br(),
             textInput("seed", "Seed for replication (opt):", value = ""),
             sliderInput("number_waves", "Number of Sine Waves (\\(n\\)):", min = 1, max = 20, value = 13, step = 1),
             sliderInput("amplitude", "Ampliutude Range (\\(a\\)):", min = -20, max = 30, value = c(1, 8), step = 0.1),
             sliderInput("wavelength", "Wavelength Range (\\(l\\)):", min = 0, max = 100, value = c(30, 65), step = 1),
             sliderInput("angle", "Direction Range (\\(d\\)):", min = 0, max = 360, value = c(0, 70), step = 1),
             sliderInput("k_value", "Wave Modulation Range (\\(k\\)):", min = 0, max = 15, value = c(1, 5), step = 0.1),
             sliderInput("phase", "Phase Range (\\(p\\)):", min = 0, max = 100, value = c(15, 30), step = 0.5),
             sliderInput("grid_size", "Grid Size:", min = 0, max = 200, value = 100, step = 10), 
             downloadButton("download_csv_sim_wave", "Download CSV")
            ),
            
            layout_column_wrap(
              width = 1/2,
              card(
               card_header("Wave Landscape"), 
               plotlyOutput("simulated_waveplot")
              ),
              card(
                card_header("Wind Landscape"),
                plotlyOutput("simulated_wind_directions")
              )
            )    
          ),
    )
   
  ), 
  
  nav_panel(
    "Optimization",
    class = "bslib-page-dashboard",
    fluidPage(
              title = h2("Work in Progress")
    )
  ),
  
  tabPanel(
          title = "Wiki",
          value = "wiki",
          
          #actual sidebar content
          page_sidebar(
            sidebar = sidebar(
            width = 350,
            gap = 10,
            title = h2("Overview"),
            tags$ol(
              tags$li(tags$a(href = "#wiki_data_selection", "Data Selection")),
              tags$ul(
                tags$li(tags$a(href = "#wiki_natural_data", "Natural Data Selection")),
                tags$li(tags$a(href = "#wiki_simulated_data", "Simulated Data Selection"))
              ),
              tags$li(tags$a(href = "#optimization", "Optimization")),
              )
            ),
            
          fluidPage(
            withMathJax(),
            div(id = "wiki_data_selection",
                h4(class = "bold-heading", "Data Selection:"),
                class = "custom-text-margin-right",
                p("To optimize the location of wave power plants, the first step in WECLO is to select suitable data. At this point it's 
                  possible to use natural wave data or simulated wave data.")
            ),
            div(id = "wiki_natural_data",
                h4(class = "bold-heading", "Natural Data Selection:"),
                class = "custom-text-margin-left-right",
                p("The natural wave and wind data is derived from the", tags$a(href = "https://www.copernicus.eu/en", "European Union's Earth Observation Program Copernicus."),
                  "In WECLO the", tags$a(href = "https://data.marine.copernicus.eu/viewer/expert?view=dataset&dataset=GLOBAL_ANALYSISFORECAST_WAV_001_027", "Global Ocean Waves Analysis and Forecast"), 
                  "dataset is used since it's a global model with one of the highest spatial resolution of 0.083° × 0.083° which roughly translates to 6.5 km x 9.5 km for each data point. 
                  The resolution presumably will get higher with future", tags$a(href = "https://marine.copernicus.eu/", "Copernicus Marine"), "products. At this point only 2 out of 19  available
                  variables are utilised which are sea surface wave significant height (VHM0) and sea surface wind wave from direction (VMDR_WW)."),
                
                p(class = "bold-heading", "Steps to perform data retrival:"),
                tags$ol(
                  tags$li("Select a desired date or timeframe"),
                  tags$li("Select area on the map by clicking at two points"),
                  tags$li("Press API-CALL to start the data retrival"),
                ),
                p("Depending on the selected area and time period the download process will take a moment.
                  The small gear icon in the wave plot header allows browsing the map resource in three hour increments within the selected time period."),
                br()
                
            ),
            div(id = "wiki_simulated_data",
                h4(class = "bold-heading", "Simulated Data Selection"),
                class = "custom-text-margin-left-right",
                p("To give a greater range of data WECLO offers to simulate individual wave landscapes. The simulation process is basically relying on the superposition of different sine waves.
                  Each of these waves is fitted to a randomly generated parameter set. Summing the individual sine waves creates a dynamic wave landscape."),
                
                p(class = "bold-heading", "Steps to perform wave landscape simulation:"),
                tags$ol(
                  tags$li("To reproduce the results, set a seed to fix the random number generator. (optional)"),
                  tags$li("Select the disired input parameters for the wave landscape"),
                ),
                br(),
                
                p(class = "bold-heading", "Explanation:"),
                p("Superposition and interference are fundamental principles to understand the interaction of different wave forms (light, sound, water) in physics. For a better comprehension,
                  a simple example is shown below in which the parameters of two simple sine waves can be adjusted. Depending on the selected parameters, the overlap (sum) of the sine waves will
                  amplify (constructive interference) or flatten (destructive interference) the amplitude and appearance of the superposition wave."),
                
                p("To learn more about the simulation of 3d water surfaces, please consider: ", tags$a(href = "https://developer.nvidia.com/gpugems/gpugems/part-i-natural-effects/chapter-1-effective-water-simulation-physical-models",
                    "\"Chapter 1. Effective Water Simulation from Physical Models\" of GPU Gems, by Mark Finch & Cyan Worlds. "), "The wave simulation in WECLO is a static representation of the sum of sines approximation in this resource."),
                
                p(class = "bold-heading", "Equation 1: $$\\text{Superposition Wave} \\, (x) = \\sum_{i=1}^{n} a_i \\times \\sin(f_i \\cdot x + p_i)$$"),
                tags$ul(
                  tags$li("Number of Waves (\\(n\\)): number of sine waves to calculate the superpostion wave"),
                  tags$li("Wave-Frequency (\\(f\\)): relates to wavelength (\\(l\\)) as (\\(f = \\frac{2}{l}\\))"),
                  tags$li("Amplitude (\\(a\\)): wave hight from the x-axis to the wave crest"),
                  tags$li("Phase (\\(p\\)): horizontal shift along the x-axis"),
                ),
                br(),
                
                accordion(
                  open = FALSE,
                  accordion_panel(
                    HTML("<h5>Superpositioning of Sine Waves Demo:</h5>"),
                    width = 12,
                    height = "60%",
                    # plot out put sine wave example
                    plotlyOutput("sine_wave_example", width = "95%", height = "300px"),
                    hr(), # dividing line
                    # input values for sine wave example
                    fluidRow(
                      column(6,
                        h5("Slider Wave 1"),
                        sliderInput("amplitude_wave1", "Amplitude (\\(a\\)):", min = -3, max = 3, value = 1.5, step = 0.25, width = "90%"),
                        sliderInput("wavelength_wave1", "Wavelength (\\(l\\)):", min = 0, max = 5, value = 1, step = 0.25, width = "90%"),
                        sliderInput("phase_wave1", "Phase-Shift (\\(p\\)):", min = 0, max = 8, value = 0, step = 0.05, width = "90%")
                      ),
                      column(6,
                        h5("Slider Wave 2"),
                        sliderInput("amplitude_wave2", "Amplitude (\\(a\\)):", min = -3, max = 3, value = 2.5, step = 0.25, width = "90%"),
                        sliderInput("wavelength_wave2", "Wavelength (\\(l\\)):", min = 0, max = 5, value = 1, step = 0.25, width = "90%"),
                        sliderInput("phase_wave2", "Phase-Shift (\\(p\\)):", min = 0, max = 8, value = 0, step = 0.05, width = "90%")
                      )
                    )
                  ) 
                ),
                br(),
                br(),
                p("To extend the equation to all three dimensions, it is necessary to include further parameters. The parameter (\\(y\\)) is added along with (\\(x\\)) 
                  to act as the coordinate reference. The direction vector \\((\\vec{d})\\) is added to give each wave a specfic direction and ultimatly create a dynamic wave landscape."),
                p(class = "bold-heading", "Equation 2: $$\\text{W} \\, (x,y) = \\sum_{i=1}^{n} a_i \\times \\sin(d_i \\cdot (x,y) \\times f_i + p_i)$$"),
                tags$ul(
                  tags$li("Wave Landscape (\\(W\\)): sum of individual sine waves"),
                  tags$li("Direction \\((\\vec{d})\\): relates to wave direction vector in radians \\(\\vec{d}(x, y) = \\left( cos \\cdot \\varphi \\, , \\, sin \\cdot \\varphi \\right)\\)"),
                ),
                br(),
                br(),
                p("As the waves generated are strongly characterized by the typical sine wave form, only a very smooth wave landscape is obtained. To give the waves sharper peaks and wider
                  troughs the expression shown in Equation 2 can be extended by the modulation value (\\(k\\)). Increasing the modulation value is resulting in a sharper overall wave landscape,
                  which is a much better representation of the reality."),
                p(class = "bold-heading", "Equation 3: $$\\text{W} \\, (x,y) = \\sum_{i=1}^{n} 2 a_i \\times \\left(\\frac{{ \\left(\\sin(d_i \\cdot (x,y) \\times f_i + p_i\\right)}}{2}\\right)^{k_i}$$"),
                tags$ul(
                  tags$li("Modulation (\\(k\\)): gives a sharper or softer sine wave"),
                ),
                
                br(),
                accordion(
                  open = FALSE,
                  accordion_panel(
                    HTML("<h5>Sine Modulation Demo:</h5>"),
                    width = 12,
                    height = "60%",
                    # plot modulation wave example
                    plotlyOutput("sine_modulation", width = "95%", height = "250px"),
                    hr(), # dividing line
                    # input values for modulation wave example
                    fluidRow(
                      column(12,
                        h5("Slider Wave 1"),
                        sliderInput("k_modulation", "Modulation Value (\\(k\\)):", min = 0, max = 3.5, value = 1, step = 0.1, width = "100%"),
                      ),
                    )
                  ) 
                ),
                br(),
                br(),
                p("At this point the wave landscape is the sum of the individual sine waves, this implements that there is no guarantee that the selected input criteria and ultimately
                  the selected wave heights are met. Therefore, it is necessary to normalize the wave landscape between the given intervals, which are defined by the amplitude range min
                  and max values. The formula which is utilized is given in Equation 4. To learn more about feature normalization and scaling please consider the following resource: ",
                  tags$a(href = "https://www.atoti.io/articles/when-to-perform-a-feature-scaling/", "\"When to perform a Feature Scaling?\", by Raghav Vashisht.")),
                p(class = "bold-heading", "Equation 4: $$\\text{W'} = \\min(a) + \\frac{(W - \\min(W)) \\cdot (\\max(a) - \\min(a) )}{\\max(W) - \\min(W)}$$"),
                tags$ul(
                  tags$li("Normilized Wave Landscape (\\(W'\\)): normalized wave landscape between input values"),
                  tags$li(HTML("Maximum Wave Landscape \\(\\max(W)\\) : max value of wave landscape matrix")),
                  tags$li(HTML("Minimum Wave Landscape \\(\\max(W)\\) : minimum value of wave landscape matrix")),
                  tags$li(HTML("Maximum Amplitude \\(\\max(a)\\) : max value of Amplitude Slider")),
                  tags$li(HTML("Minimum Amplitude \\(\\min(a)\\) : min value of Amplitude Slider")),
                ),
                br(),
                br(),
                
            ),
            div(id = "optimization",
                h4(class = "bold-heading", "Optimization"),
                p("to be continued ...")
            ),
          )
        ),
    ),
  
  nav_panel(
    "About",
    class = "bslib-page-dashboard",
    fluidPage(
                  title = h2("About"),
                  p("This dashboard was developed as part of a master thesis at the", tags$a(href = "https://tu-dresden.de/bu/verkehr/ivw/bda?set_language=en",
                    "Chair of Big Data Analytics in Transportation (TT)"), "of TUD Dresden University of Technology. The aim is to create a highly user-friendly
                    interface for wave energy converter location optimization (WECLO). In case of questions or occurring problems, please use the email address below:"),
                  p(tags$a(href = "mailto:weclo-dashboard@outlook.com", "WECLO-Dashboard@outlook.com")
                  )
                )
  ),
  
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

# Server section ----------------------------------------------------------

server <- function(input, output, session) {
  
  # natural data selection functions -----------------------------------------
  
  # function to update progress
  update_progress <- function(progress) {
    if (progress$getValue() < progress$getMax()) {
      progress$inc(1)
    }
  }

  # define selected_bounds as reactive value
  selected_bounds <- reactiveValues(bounds = NULL)
  
  # defining a slider input for the card date (used in popover)
  output$timeSlider <- renderUI({
      page_fillable(
        sliderInput("wave_plot_date", "Select Date & Time:",
                  min = as.POSIXct(input$custom_datepicker[1]),                   
                  max = as.POSIXct(input$custom_datepicker[2]),
                  value = as.POSIXct(input$custom_datepicker[1]),
                  step = 3*3600,                                                 # when values are in date-times, steps need to specified in sec. here 3h steps 
                  ticks = TRUE, 
                  animate = animationOptions(interval = 850, loop = FALSE),
                  timeFormat = "%Y-%m-%d %H:%M") # Format for displaying time
        )
  })


  # initial leaflet map 
  output$map <- renderLeaflet({
  leaflet() %>%
    setView(lng = 0, lat = 0, zoom = 1.5)
    
  })
  
  # observe if the input of the wave plot date slider is changing and render the "animation" map
  observeEvent(c(input$dark_mode, input$wave_plot_date), {
    
    leafletProxy("map") %>%
      #clearTiles() %>%
      removeTiles(layerId = "initial_layer_wave_map")%>%
      addProviderTiles(
        if (input$dark_mode == "dark") {
          "Stadia.AlidadeSmoothDark"
        } else {
          "Stadia.AlidadeSmooth"
        })%>%
      
      # when clicking on the popover gear icon the map is rendered by altering the time attribute in the url, this is depending on the wave_plot_date slider
      addTiles(urlTemplate = paste0("https://wmts.marine.copernicus.eu/teroWmts/?service=WMTS&version=1.0.0&request=GetTile&layer=GLOBAL_ANALYSISFORECAST_WAV_001_027/cmems_mod_glo_wav_anfc_0.083deg_PT3H-i_202311/VHM0&tilematrixset=EPSG:3857@3x&tilematrix={z}&tilerow={y}&tilecol={x}&time=", format(as.POSIXct(input$wave_plot_date), "%Y-%m-%dT%H:%M:%S.000Z")), 
               layerId = "wave_map_animation",
               attribution = '<a href="https://data.marine.copernicus.eu/viewer/expert?view=viewer&crs=epsg%3A4326&t=1712232000000&z=0&center=0%2C0&zoom=9.843922654284418&layers=W3sib3BhY2l0eSI6MSwiaWQiOiJjMSIsImxheWVySWQiOiJHTE9CQUxfQU5BTFlTSVNGT1JFQ0FTVF9XQVZfMDAxXzAyNy9jbWVtc19tb2RfZ2xvX3dhdl9hbmZjXzAuMDgzZGVnX1BUM0gtaV8yMDIzMTEvVkhNMCIsInpJbmRleCI6MjAsImxvZ1NjYWxlIjpmYWxzZX1d&basemap=dark" target="_blank">Copernicus Marine Service</a>')
        
    })
  
  # render initial map, if wave plot date slider was adjusted remove the render layer
  observeEvent(c(input$dark_mode, input$custom_datepicker), {
    
    leafletProxy("map") %>%
      #clearTiles() %>%
      removeTiles(layerId = "wave_map_animation")  %>%
      addProviderTiles(
        if (input$dark_mode == "dark") {
          "Stadia.AlidadeSmoothDark"
        } else {
          "Stadia.AlidadeSmooth"
        })%>%
      # initial map setup using the date picker input for rendering copernicus data
      addTiles(urlTemplate = paste0("https://wmts.marine.copernicus.eu/teroWmts/?service=WMTS&version=1.0.0&request=GetTile&layer=GLOBAL_ANALYSISFORECAST_WAV_001_027/cmems_mod_glo_wav_anfc_0.083deg_PT3H-i_202311/VHM0&tilematrixset=EPSG:3857@3x&tilematrix={z}&tilerow={y}&tilecol={x}&time=", format(as.POSIXct(input$custom_datepicker[1]), "%Y-%m-%dT%H:%M:%S.000Z")),
               layerId = "initial_layer_wave_map",
              attribution = '<a href="https://data.marine.copernicus.eu/viewer/expert?view=viewer&crs=epsg%3A4326&t=1712232000000&z=0&center=0%2C0&zoom=9.843922654284418&layers=W3sib3BhY2l0eSI6MSwiaWQiOiJjMSIsImxheWVySWQiOiJHTE9CQUxfQU5BTFlTSVNGT1JFQ0FTVF9XQVZfMDAxXzAyNy9jbWVtc19tb2RfZ2xvX3dhdl9hbmZjXzAuMDgzZGVnX1BUM0gtaV8yMDIzMTEvVkhNMCIsInpJbmRleCI6MjAsImxvZ1NjYWxlIjpmYWxzZX1d&basemap=dark" target="_blank">Copernicus Marine Service</a>')
    })

  # if else loop for selecting area on map
  # creates a list of bounds in form of: (lat1, lon1, lat2, lon2)
  observeEvent(input$map_click, {
    click <- input$map_click
    if (!is.null(click)) {
      lat <- click$lat
      lon <- click$lng
      if (is.null(selected_bounds$bounds)) {
        selected_bounds$bounds <- c(lat, lon, NA, NA)
      } else if (is.na(selected_bounds$bounds[3]) || is.na(selected_bounds$bounds[4])) {
        selected_bounds$bounds[3] <- lat
        selected_bounds$bounds[4] <- lon
        leafletProxy("map") %>%
          addRectangles(
            lng1 = selected_bounds$bounds[2], lat1 = selected_bounds$bounds[1],
            lng2 = selected_bounds$bounds[4], lat2 = selected_bounds$bounds[3],
            stroke = TRUE, weight = 1, color = "#3c8dbc", fillOpacity = 0.2
          )
      } else {
        selected_bounds$bounds <- c(lat, lon, NA, NA)
        leafletProxy("map") %>%
          clearShapes()
      }
    }
  })
  

  # text output under map
    output$map_text <- renderUI({
    if (!is.null(selected_bounds$bounds) && !anyNA(selected_bounds$bounds)) {
      
      
      # calculation of distance between 2 points on earth using haversine formula (great circle distance)
      lat_distance <- haversine(c(selected_bounds$bounds[1], selected_bounds$bounds[2]), c(selected_bounds$bounds[3], selected_bounds$bounds[2]), R = 6378.137)
      lon_distance <- haversine(c(selected_bounds$bounds[3], selected_bounds$bounds[2]), c(selected_bounds$bounds[3], selected_bounds$bounds[4]), R = 6378.137)
      
      # actual text output under the map
      HTML(paste(
        "<strong>Selected Coordinates:</strong>",
        "&nbsp;<strong>Point 1:</strong>", paste(c(selected_bounds$bounds[1], selected_bounds$bounds[2]), collapse = ", "),
        "&nbsp;<strong>Point 2:</strong>", paste(c(selected_bounds$bounds[3], selected_bounds$bounds[4]), collapse = ", "),
        "&nbsp;<strong>Latitudinal Distance:</strong>", paste(round(lat_distance, 3), "km"),
        "&nbsp;<strong>Longitudinal Distance:</strong>", paste(round(lon_distance, 3), "km")
      ))
    } else {
      HTML("Select a date or time period and click at two points on the map to define an area.")
    }
  })
  

  # update progress function
    update_progress <- function(progress, step) {
      if (progress$getValue() < progress$getMax()) {
        progress$inc(1)
        progress$set(message = step)
      }
    }
  
  # api call parameters
  observeEvent(input$start_api_call, {
    
    # to prevent downloading of the entire data set, check if selected_bounds$bounds is empty
    if (is.null(selected_bounds$bounds) || anyNA(selected_bounds$bounds)) {
    # show error massage
    showNotification("No area selected!", type = "error")
    } else {
    
    # progress notification
    progress <- shiny::Progress$new(min = 0, max = 5)
    progress$set(message = "API Call started", value = 0)
    on.exit(progress$close())
    
    # sort the lon & lat coordinates to meet the api call parameters
    lon = sort(c(selected_bounds$bounds[2], selected_bounds$bounds[4]))   # lon_min, lon_max
    lat = sort(c(selected_bounds$bounds[3], selected_bounds$bounds[1]))   # lat_min, lat_max
    
    # transforming time argument from air date picker
    date_min <- format(input$custom_datepicker[1], "%Y-%m-%dT%H:%M:%S")
    date_max <- format(input$custom_datepicker[2], "%Y-%m-%dT%H:%M:%S")
    
    # progress update 1: api call started
    update_progress(progress, "API-Call started")
    
    # api call command (reticulate python function)
    CopernicusMarine$subset(
      dataset_id= "cmems_mod_glo_wav_anfc_0.083deg_PT3H-i",               # dataset from copernicus https://data.marine.copernicus.eu/product/GLOBAL_ANALYSISFORECAST_WAV_001_027/download?dataset=cmems_mod_glo_wav_anfc_0.083deg_PT3H-i_202311
      dataset_version = "202311",
      dataset_part = "default",
      service = "arco-time-series",
      variables= r_to_py(list("VHM0", "VMDR_WW")),                        # VMH0 = significant wave height, VMDR_WW = wind-wave direction
      minimum_longitude = lon[1],
      maximum_longitude = lon[2],
      minimum_latitude = lat[1],
      maximum_latitude = lat[2],
      start_datetime = date_min,
      end_datetime = date_max,
      output_filename = "wave_data.nc",
      output_directory = directory,                                       # use working directory as output directory
      overwrite_output_data = TRUE,
      force_download = TRUE)
    
    
    data <- nc_open(paste0(directory, "wave_data.nc"))
    
    dim_lon <- ncvar_get(data, "longitude")
    dim_lat <- ncvar_get(data, "latitude")
    dim_time <- ncvar_get(data, "time")
    
    t_units <- ncatt_get(data, "time", "units")
    t_ustr <- strsplit(t_units$value, " ")
    t_dstr <- strsplit(unlist(t_ustr)[3], "-")
    date <- ymd(t_dstr) + dhours(dim_time)
    
    coords <- as.data.frame(expand.grid(dim_lon, dim_lat, date), stringsAsFactors = FALSE)
    attr(coords, "out.attrs") <- NULL
    
    VHM0 <- data.matrix(ncvar_get(data, "VHM0", collapse_degen=FALSE))
    VMDR_WW <- data.matrix(ncvar_get(data, "VMDR_WW", collapse_degen=FALSE))
    
    wave <- bind_cols(coords, VHM0, VMDR_WW)
    names(wave) <- c("lon", "lat", "time", "VMH0", "VMDR_WW")
    
    # close wave_data.nc, if file is left "open", python download script can not override existing files
    nc_close(data)
    
    # omit na's in case missing values
    wave <- na.omit(wave)
    
    # progress update 2: wave data loaded and prepared
    update_progress(progress, "Wave Data loaded")
    
    #crating natural wave landscape matrix
    natural_df <- wave[,-3]
    natural_df <- wave %>% 
      group_by(lat, lon) %>%
      summarise(mean_VMH0 = mean(VMH0))
    
    # grouping data by lat and lon
    grouped_natural_df <- natural_df %>% 
                            group_by(lat, lon)
    
    # creating empty geo matrix for z (hight) values
    geo_matrix <- matrix(NA, nrow = length(unique(natural_df$lat)), ncol = length(unique(natural_df$lon)))
    
    # loop for assigning z values to each geographic point 
    for (i in 1:nrow(grouped_natural_df)) {
      lat_index <- which(unique(natural_df$lat) == grouped_natural_df$lat[i])
      lon_index <- which(unique(natural_df$lon) == grouped_natural_df$lon[i])
      geo_matrix[lat_index, lon_index] <- grouped_natural_df$mean_VMH0[i]
      if (length(lat_index) > 0 & length(lon_index) > 0) {
        geo_matrix[lat_index, lon_index] <- grouped_natural_df$mean_VMH0[i]
      } else {
        geo_matrix[lat_index, lon_index] <- 0
      }
    }
    
    # converting of matrix into list of x(lon), y(lat) and z(hight) components
    natural_df_plot <- list(
      lon = unique(natural_df$lon),
      lat = unique(natural_df$lat),
      VMH0 = geo_matrix
    )
    
    
    # actual natural wave landscape plot with plotly
    output$natural_wave_plot <- renderPlotly({
      
      # calculating x and y distances using the haversine (great circle distance)
      lat_distance <- isolate(haversine(c(selected_bounds$bounds[1], selected_bounds$bounds[2]), c(selected_bounds$bounds[3], selected_bounds$bounds[2]), R = 6378.137))
      lon_distance <- isolate(haversine(c(selected_bounds$bounds[3], selected_bounds$bounds[2]), c(selected_bounds$bounds[3], selected_bounds$bounds[4]), R = 6378.137))
      
      plot_ly(x = natural_df_plot$lon, y = natural_df_plot$lat, z = natural_df_plot$VMH0) %>% 
      add_surface()  %>% 
        layout(
          modebar = list(color = "lightgrey",
                         activecolor = "grey",
                         bgcolor = if (input$dark_mode == "dark") "transparent" else "transparent"),
          plot_bgcolor  = "transparent",
          paper_bgcolor = "transparent",
          font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default"),
          scene = list(
                      xaxis = list(title = "Lon"),
                      yaxis = list(title = "Lat"),
                      zaxis = list(title = "VMH0"),
          aspectratio = list(x =lon_distance/100, y = lat_distance/100 , z = (max(natural_df_plot$VMH0)-min(natural_df_plot$VMH0))/10), # significant wave height boosted by factor of 10 for better visualization 
          aspectmode = "manual",
          camera = list(eye = list(x = -(lon_distance/100), y = -(lat_distance/100), z = (max(natural_df_plot$VMH0)-min(natural_df_plot$VMH0))))
          )
        )%>%
        colorbar(title = "Wave Height in m")
    })
    
    # progress update 3: wave landscape data processed and plotted
    update_progress(progress, "Wave Landscape processed and plotted")
    
    # creating a new data frame with all wind directions from "wave"
    wind <- data.frame(degrees = wave$VMDR_WW)
    
    # creating empty data drame, note: "direction" in Direction necessary for radar plot
    summary_wind <- data.frame(
      Direction = c("E", "ENE", "NE", "NNE", "N", "NNW", "NW", "WNW", "W", "WSW", "SW", "SSW", "S", "SSE", "SE","ESE"),
      Max = nrow(wind),
      Count = rep(0, 16),
      Percent = rep(0, 16)
      )
    
    # using wind direction function and counting wind directions in wind for each row
    for (i in 1:nrow(wind)) {
      wind_direction <- degrees_to_direction(wind$degrees[i])
      rof_index <- match(wind_direction, summary_wind$Direction)  # using the more efficient match() function instead of which() to find row index
      summary_wind$Count[rof_index] <- summary_wind$Count[rof_index] + 1
    }
    
    # calc. percentage values of wind directions 
    summary_wind$Percent <- (summary_wind$Count / sum(summary_wind$Count))
    
    # progress update 4: wind data processed
    update_progress(progress, "Wind Data processed")
    
    
    
    # create the radar chart
    output$natural_wind_directions <-renderPlotly({
      plot_ly(summary_wind, type = "scatterpolar", fill = "toself", mode = "lines+markers", hoverinfo = "r+theta") %>%                                # initial plot setup
        add_trace(r = summary_wind$Percent, theta = summary_wind$Direction , name = "Direction in %",
                  fillcolor = "rgba(48, 106, 142, 0.6)",  
                  line = list(color = "rgba(19, 86, 149, 0.8)"),
                  marker = list(color = "rgba(198, 224, 42, 1)")) %>%
        layout(
               modebar = list(color = "lightgrey", activecolor = "grey", bgcolor = if (input$dark_mode == "dark") "transparent" else "transparent"),  # adjust modebar for dark mode
               polar = list(bgcolor = "transparent", radialaxis = list(tickformat = ".0%")),                                                          # adjust background of radial plot and scale/ unit of y-axis 
               paper_bgcolor = "transparent",                                                                                                         # set background of plot environment
               font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default"))                                                        # adjust font color when using dark mode
    }) 
    
    #progress update 5: wind plot created
    update_progress(progress, "API Call done")
    }
    
  })
  
  # if dashboard is loaded and no api call is executed, display massage in plot outputs
  output$natural_wave_output <- renderUI({
    if (input$start_api_call == 0) {
      div(
        p("Wating for API Call..."),
        style = "display: flex; align-items: center; justify-content: center; height: 100%;"
      )
    } else {
      plotlyOutput("natural_wave_plot")
    }
  })
  
  output$natural_wind_output <- renderUI({
    if (input$start_api_call == 0) {
      div(
        p("Wating for API Call..."),
        style = "display: flex; align-items: center; justify-content: center; height: 100%;"
      )
    } else {
      plotlyOutput("natural_wind_directions")
    }
  })
  
  # natural wave download section -----------------------------------------
    
    output$download_csv_nat_wave <- downloadHandler(
      filename = "natural_wave_data.csv",
      content = function(file) {
        write.csv(wave, file, row.names = FALSE)
      }
    )

  # wave landscape simulation functions -------------------------------------
  
  # reactive values to store generated wave parameters
  waveParams <- reactiveValues(amplitude = NULL,
                               wavelength = NULL, 
                               phase = NULL, 
                               angle = NULL, 
                               kValue = NULL)
  
  observe({
    
    
    if (is.null(input$seed) || input$seed == "") {
      set.seed(NULL)  # reset seed
    } else {
      # generate seed using simple sum function
      set.seed(sum(utf8ToInt(input$seed)))
    }
    
    nWaves <- input$number_waves
    waveParams$amplitude <- runif(nWaves, input$amplitude[1], input$amplitude[2])
    waveParams$wavelength <- runif(nWaves, input$wavelength[1], input$wavelength[2])
    waveParams$phase <- runif(nWaves, input$phase[1], input$phase[2])
    waveParams$angle <- runif(nWaves, input$angle[1], input$angle[2]) 
    waveParams$kValue <- runif(nWaves, input$k_value[1], input$k_value[2])
    
    # create grid boundaries were n = input$grid_size + 1
    x <- seq(0, input$grid_size, by = 1)
    y <- seq(0, input$grid_size, by = 1)
    
    # initialize the landscape matrix 
    landscape <- matrix(0, nrow = length(x), ncol = length(y), byrow = TRUE)
    
    # loop over each wave to calculate the superposition
    for (i in 1:input$number_waves) {
      amplitude <- waveParams$amplitude[i]
      wavelength <- waveParams$wavelength[i]
      phase <- waveParams$phase[i]
      angle <- waveParams$angle[i]
      k <- waveParams$kValue[i]
      
      # wave vector components
      w <- 2 * pi / wavelength
      angle * pi / 180  # convert degrees to radians
      dx <- cos(angle)
      dy <- sin(angle)
      
      # calculate the individual sine wave
      wave <- outer(x, y, function(x, y) {
        2 * amplitude * ((sin(dx * x * w + dy * y * w + phase) + 1) / 2)^k
      })
      
      # Add the sine wave of current iteration to landscape matrix
      landscape <- landscape + wave
    }
    
    # normalization of the superposition matrix
    landscape_max <- max(landscape)
    landscape_min <- min(landscape)
    amplitude_min <- input$amplitude[1]
    amplitude_max <- input$amplitude[2]
    
    # apply normalization between the given intervals of the amplitude inputs: source: https://www.atoti.io/articles/when-to-perform-a-feature-scaling/ 
    if (landscape_max != landscape_min) {
      landscape <- amplitude_min + ((landscape - landscape_min) * (amplitude_max - amplitude_min)) / (landscape_max - landscape_min)
    }
    
    # simulated wave plots ----------------------------------------------------
   
    output$simulated_waveplot <- renderPlotly({
      plot_ly(x = ~x, y = ~y, z = ~landscape, type = "surface")%>%
        layout(
          scene = list(
                       xaxis = list(title = "X"),
                       yaxis = list(title = "Y"),
                       zaxis = list(title = "Wave Height"),
                       aspectmode = "manual", 
                       aspectratio = list(x=1, y=1, z=0.25),
                       camera = list(eye = list(x = -1.85, y = -1.85, z = 1.85))
                      ),
          modebar = list(
                      color = "lightgrey",                                                          # adjust modebar for dark mode
                      activecolor = "grey",
                      bgcolor = if (input$dark_mode == "dark") "transparent" else "transparent" 
                      ),
        plot_bgcolor  = "transparent",                                                              # set background of plot environment to transparent
        paper_bgcolor = "transparent",                                                              # set background of plot to transparent
        font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default")              # adjust font color when using dark mode
        ) %>%
        colorbar(title = "Wave Height")
        
    })
    
    
    # simulated wind plot
    # creating a new data frame with all wind directions from the input angle vector
    sim_wind <- data.frame(degrees = waveParams$angle)  
    
    # creating empty data frame, with columns for the direction, count and percentage
    summary_sim_wind <- data.frame(
      Direction = c("E", "ENE", "NE", "NNE", "N", "NNW", "NW", "WNW", "W", "WSW", "SW", "SSW", "S", "SSE", "SE","ESE"),
      Max = nrow(sim_wind),
      Count = rep(0, 16),
      Percent = rep(0, 16)
      )

    # using wind direction function and counting wind directions in sim_wind for each row
    for (i in 1:nrow(sim_wind)) {
      sim_wind_direction <- degrees_to_direction(sim_wind$degrees[i])
      rof_index <- match(sim_wind_direction, summary_sim_wind$Direction)  # using the more efficient match() function instead of which() to find row index
      summary_sim_wind$Count[rof_index] <- summary_sim_wind$Count[rof_index] + 1
    }

    # calc. percentage values of wind directions
    summary_sim_wind$Percent <- (summary_sim_wind$Count / sum(summary_sim_wind$Count))

    # create the radar chart
    output$simulated_wind_directions <-renderPlotly({
      plot_ly(summary_sim_wind, type = "scatterpolar", fill = "toself", mode = "lines+markers", hoverinfo = "r+theta") %>%                            # initial plot setup
        add_trace(r = summary_sim_wind$Percent, theta = summary_sim_wind$Direction , name = "Direction in %",
                  fillcolor = "rgba(48, 106, 142, 0.6)",
                  line = list(color = "rgba(19, 86, 149, 0.8)"),
                  marker = list(color = "rgba(198, 224, 42, 1)")) %>%
        layout(
               modebar = list(color = "lightgrey", activecolor = "grey", bgcolor = if (input$dark_mode == "dark") "transparent" else "transparent"),  # adjust modebar for dark mode
               polar = list(bgcolor = "transparent", radialaxis = list(tickformat = ".0%")),                                                          # adjust background of radial plot and scale/ unit of y-axis
               paper_bgcolor = "transparent",                                                                                                         # set background of plot environment
               font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default"))                                                        # adjust font color when using dark mode
    })
    

    # simulated wave download section -----------------------------------------
    
    output$download_csv_sim_wave <- downloadHandler(
      filename = "landscape.csv",
      content = function(file) {
        write.csv(landscape, file, row.names = FALSE)
      }
    )
  })
  

  # example superposition of sine waves -------------------------------------
  output$sine_wave_example <- renderPlotly({
    # create  sqeuenz for x values
    x <- seq(0, 2 * pi, length.out = 100)
    
    # simple calculation of 2 sine waves
    sine_1 <- input$amplitude_wave1 * sin(2/input$wavelength_wave1 * x + input$phase_wave1)
    sine_2 <- input$amplitude_wave2 * sin(2/input$wavelength_wave2 * x + input$phase_wave2)
    sine_sum <- sine_1 + sine_2
    
    # plotting sine wave 1 & 2 and superposition wave  
    plot_ly() %>%
      add_trace(x = x, y = sine_1, type = "scatter", mode = "lines", name = "Wave 1") %>%
      add_trace(x = x, y = sine_2, type = "scatter", mode = "lines", name = "Wave 2") %>%
      add_trace(x = x, y = sine_sum, type = "scatter", mode = "lines", name = "Superposition Wave", line = list(dash = "dash")) %>%
      layout(
        legend = list(x = 0.8, y = 0.95),
        yaxis = list(
          range = c(-6, 6), 
          autorange = FALSE 
        ),
        xaxis = list(
          range = c(0, 2*pi), 
          autorange = FALSE 
        ),
        modebar = list(
                      color = "lightgrey",                                                          # adjust modebar for dark mode
                      activecolor = "grey",
                      bgcolor = if (input$dark_mode == "dark") "transparent" else "transparent" 
                      ),
        plot_bgcolor  = "transparent",                                                              # set background of plot environment to transparent
        paper_bgcolor = "transparent",                                                              # set background of plot to transparent
        font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default")              # adjust font color when using dark mode
      )
  })
  

  # example modulation value k ----------------------------------------------
  output$sine_modulation <- renderPlotly({
    # create  sqeuenz for x values
    x <- seq(0, 11, length.out = 150)
    
    # calculation of sine wave with modulation value k
    sine_modulation <- 2 * (sin(x )+ 1)^ input$k_modulation
    
    # plotting sine wave with modulation value k  
    plot_ly() %>%
      add_trace(x = x, y = sine_modulation , type = "scatter", mode = "lines", name = "$$ f(x) = 2 \\left( \\frac{\\sin(x) + 1}{2} \\right)^k $$") %>%
      layout(
        showlegend = TRUE,
        legend = list(x = 0.8, y = 0.95),
        yaxis = list(
          range = c(0, 25), 
          autorange = FALSE 
        ),
        xaxis = list(
          range = c(0, 11), 
          autorange = FALSE 
        ),
        modebar = list(
                      color = "lightgrey",                                                          # adjust modebar for dark mode
                      activecolor = "grey",
                      bgcolor = if (input$dark_mode == "dark") "transparent" else "transparent" 
                      ),
        plot_bgcolor  = "transparent",                                                              # set background of plot environment to transparent
        paper_bgcolor = "transparent",                                                              # set background of plot to transparent
        font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default")              # adjust font color when using dark mode
      )
  })  
  
  
  # end application without terminating R session
  session$onSessionEnded(function() {
    stopApp()
  })
  
}


shinyApp(ui, server)