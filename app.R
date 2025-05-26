# ==== Global Functions: =======================================================
# ------ working directory and packages ----------------------------------------
# list of libraries
packages <- c(
  # basic packages
  "tidyverse",           # summary of basic r packages
  "lubridate",           # date and time manipulation
  "data.table",          # efficient large table handling
  "arrow",               # reading and writing parquet files
  
  # plot packages
  "plotly",              # 3d plot
  "ggspatial",           # create maps using ggplot
  "maps",                # map polygons for ggplot 

  # R to Python package
  "reticulate",          # run python commands in r (used for Copernicus api call)
  
  # spatial packages
  "ncdf4",               # marine data files access
  "terra",               # spatial raster datasets and manipulation
  "pracma",              # distance calculation between geographical points
  
  # mlr3 framwork
  "mlr3verse",           # machine learning for R
  "mlr3spatiotempcv",    # spatiotemporal resampling methods
  
  # shiny dashboard      
  "shiny",               # basic shiny functions
  "shinydashboard",      # shiny dashboard package
  "shinyWidgets",        # date/ time picker
  "leaflet",             # maps
  "bslib",               # bootstrap ui toolkit (predefined shiny theme) 
  "bsicons"              # bootstrap icons
)


# loading the installed packages
for(pkg in packages){
  library(pkg, character.only = TRUE)
}

# set working directory
working_directory <- "/var/lib/wave-dash/temp/"

# import python package for handling copernicus marine data
use_virtualenv("/var/lib/wave-dash//copernicusmarine", required = TRUE)
CopernicusMarine <- import("copernicusmarine")

# ------ wave direction function -----------------------------------------------
# wave direction function to calc. wind direction based on wave degrees
degrees_to_direction <- function(degree) {
  if (is.na(degree)) {return("unkown")            
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


# ------ ml-model training -----------------------------------------------------
# read copernicus file of all stations
model_data <- as.data.table(read_parquet(paste0(working_directory, "wave_cop.parquet")))

# final training data
final_data <- model_data %>% 
                  select(lat, lon, time, station, VHM0, VCMX, VHM0_SW1, VTM01_WW, VTM02)

# defining benchmark task
final_tsk <- as_task_regr_st(final_data,
                           id = "final", target = "VHM0",
                           coordinate_names = c("lon", "lat"), coords_as_features = FALSE,
                           crs = "4326")

# setting role for time/ date variable 
final_tsk$set_col_roles("time", roles = "time")
final_tsk$set_col_roles(c("lon","lat"), roles = "space")
final_tsk$set_col_roles("station", roles = "name")

# define learner
xgboost = lrn("regr.xgboost")

# train model xgboost
future::plan("multisession")
xgboost$train(final_tsk)


# ==== Dashboard: ==============================================================
# ------ ui section ------------------------------------------------------------

# initial ui setup using bs_lib  
ui <- page_navbar(
  theme = bs_theme(preset = "shiny"),
  lang = "en",
  id = "navbar",
  title = tags$strong("Wave Dashboard"),
  
  
# -------- defining ui style tags ----------------------------------------------
  # adjust height of custom date picker (nav_panel data selection)
  tags$style(HTML("
  #custom_datepicker {
    height: 47px;
  }
  ")),
  
  # adjust height of custom date picker (nav_panel wave prediction)
  tags$style(HTML("
  #synced_datepicker {
    height: 47px;
  }
  ")),
  
  # custom text margin left and right for dashboard wiki
  tags$style(HTML("
    .custom-text-margin-left-right {
      margin-left: 20px;  
      margin-right: 20px; 
    }
  ")),
  
  # custom text margin right for dashboard wiki
  tags$style(HTML("
    .custom-text-margin-right {
      margin-right: 20px; 
    }
  ")),
  
  # bold text headings
  tags$style(HTML("
    .bold-heading {
      font-weight: bold; 
    }
  ")),
  
  # adjust left-right margin of map popover 
  tags$style(HTML("
    .popover .irs {
      margin-left: 13px ; 
      margin-right: 13px ;
    }
  ")),
  
  # adjust text size for map text
  tags$style(HTML("
    .map-output-text {
      font-size: 15.5px;
      margin-top: -10px;
  }
  ")),
  
  # style tag for all dashboard headings
  tags$style(HTML("
    .dashboard-heading {
      font-size: 17px;
    }
  ")),
  
  # style tag for all sidebar headings
  tags$style(HTML("
    .sidebar-heading {
      font-size: 18px;
      font-weight: 600; 
    }
  ")),
  

# -------- defining data selection nav panel ----------------------------------
  
  nav_spacer(),
  
  # navigation menu including natural and simulated data selection
  nav_panel(
    title = div(class = "dashboard-heading", "Data Selection"),
    value = "data_Selection",
    
    page_sidebar(
      #actual sidebar content
      sidebar = sidebar(
      width = 350,
      gap = 15,
      title = div(class = "sidebar-heading", "Date & Time Selection:"),
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
       actionButton(inputId = "start_api_call", label = "Start API Call", class = "btn-primary"),
       tags$hr(style = "margin-top: 1px; margin-bottom: 1px;"),
       actionButton(inputId = "nav_to_prediction", label = "Continue with Prediction Results", class = "btn-success"),
      ),
     
      # map card for wave data selection
      card(
        min_height = "60%",
        full_screen = FALSE,
        card_header(div(class = "dashboard-heading", "Area and Date Selection"), 
                    popover(
                      title = "Plot Controls",
                      id = "plot_popover",
                      bs_icon("gear"),
                      uiOutput("timeSlider")
                    ),
                    class = "d-flex justify-content-between align-items-center"),
        
        #map output + text + button
        leafletOutput("map"),
        fluidRow(
          column(width = 12,
          htmlOutput("map_text")),
        )
      ),  
      
      # initial mean variable plot 
      card(
        min_height = "60%",
        full_screen = TRUE,
        card_header(div(class = "dashboard-heading", "Mean Values of Selected Area by Variable"),
                    popover(
                      title = "Variable Description",
                      bs_icon("info-circle"),
                      # html table of variable description
                      HTML("
                      <table cellspacing='4' cellpadding='4'>
                        <tr><td style='vertical-align:top; text-align:left;'><b>VHM0_SW1</b></td><td>Sea surface primary swell wave significant height</td></tr>
                        <tr><td style='vertical-align:top; text-align:left;'><b>SWH</b></td><td>Sea surface wave significant height</td></tr>
                        <tr><td style='vertical-align:top; text-align:left;'><b>VCMX</b></td><td>Sea surface wave maximum height</td></tr>
                        <tr><td style='vertical-align:top; text-align:left;'><b>VTM02</b></td><td>Sea surface wave mean period from variance spectral density second frequency moment</td></tr>
                        <tr><td style='vertical-align:top; text-align:left;'><b>VTM01_WW</b></td><td>Sea surface wind wave mean period</td></tr>
                        <tr>
                          <td style='text-align:left; padding-top:10px;'><b>More Info</b></td>
                          <td style='text-align:left; padding-top:10px;'>
                            <a href='https://data.marine.copernicus.eu/product/GLOBAL_ANALYSISFORECAST_WAV_001_027/description' target='_blank'>Copernicus Marine Service</a>
                          </td>
                        </tr>
                      </table>
                      ")
                    ),
                    class = "d-flex justify-content-between align-items-center"
                    ),
        uiOutput("ts_wave_meters_output", fill = TRUE)
      )
    ),
  ), 


# -------- defining data prediction and upscaling nav panel -------------------
  
   nav_panel(
    title = div(class = "dashboard-heading", "Wave Prediction & Upscaling"),
    value = "wave_prediction",
    
    page_sidebar(
      #actual sidebar content
      sidebar = sidebar(
        width = 350,
        gap = 15,
        sliderInput(inputId = "upscale_factor", label = div(class = "sidebar-heading","Upscaling Factor:"), value = 2, min = 1, max = 10),
        tags$hr(style = "margin-top: 1px; margin-bottom: 1px;"),
        div(class = "sidebar-heading", "Adjust Date & Time:"),
        uiOutput("synced_datepicker"),
        actionButton(inputId = "start_api_call", label = "Restart API Call", class = "btn-primary"),
        tags$hr(style = "margin-top: 1px; margin-bottom: 1px;"),
        div(class = "sidebar-heading", "Download Data:"),
        downloadButton("download_pred_wave_data", "Predicted Data"),
        downloadButton("download_upscaled_wave_data", "Upscaled Data"),
        tags$hr(style = "margin-top: 1px; margin-bottom: 1px;"),
        actionButton(inputId = "nav_to_wave_modulation", label = "Continue with Wave Modulation", class = "btn-success"),
      ),
     
      
      # data plots
      layout_columns(
        min_height = "60%",
        # predicted wave field plots
        card(
          full_screen = TRUE,
          card_header(div(class = "dashboard-heading", "Wave Field Prediction")),
          uiOutput("wave_field_pred_output", fill = TRUE)
        ),
        
        # upscaled wave field plot
        card(
          full_screen = TRUE,
          card_header(div(class = "dashboard-heading", "Upscaled Wave Field")),
          uiOutput("upscaled_wave_field_output", fill = TRUE)
        ),
      ),
      
      # ts plot if actual vs. predicted SWH 
      div(style = "margin-bottom: 1%;", 
        card(
          min_height = "60%",
          full_screen = TRUE,
          card_header(div(class = "dashboard-heading", "Actual vs. Predicted Mean Significant Wave Height")),
          uiOutput("ts_wave_pred_actual_output", fill = TRUE),
          tags$hr(style = "margin-top: 1px; margin-bottom: 1px;"),
          fluidRow(
            column(width = 12,
            htmlOutput("prediction_scores")),
          )
        )
      )
    ),
  ), 
  

# -------- defining wave modulation nav panel ---------------------------------

  # wave modulation panel
  nav_panel(
    title = div(class = "dashboard-heading", "Wave Modulation"),
    value = "wave_modulation",
      # sidebar page
      page_sidebar(
        #actual sidebar content
        sidebar = sidebar(
        width = 350,
        gap = 1,
        
        # dynamic card output depending on whether api data is present or not 
        uiOutput("dynamic_sidebar", fill = TRUE),
        ),
        
        # dynamic card output depending on whether api data is present or not 
        uiOutput("dynamic_cards", fill = TRUE),
      ),
  ),

# -------- defining wiki nav panel --------------------------------------------  
  
  tabPanel(
    title = div(class = "dashboard-heading", "Wiki"),
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
            tags$li(tags$a(href = "#wiki_data_origin", "Data Origin")),
            tags$li(tags$a(href = "#wiki_data_steps", "Data Selection Steps"))
          ),
        br(),
        tags$li(tags$a(href = "#wiki_pred_upscaling", "Wave Prediction & Upscaling")),
          tags$ul(
            tags$li(tags$a(href = "#wiki_pred_upscaling_model", "Model Training")),
            tags$li(tags$a(href = "#wiki_pred_upscaling_wave", "Wave Field Upsacling"))
          ),
        br(),
        tags$li(tags$a(href = "#wiki_modulation", "Wave Field Modulation")),
                  tags$ul(
            tags$li(tags$a(href = "#wiki_wave_mod_steps", "Modulation Steps")),
            tags$li(tags$a(href = "#wiki_wave_mod_explanation", "Explanation"))
          ),
        )
      ),
     
    fluidPage(
      withMathJax(),
      div(id = "wiki_data_selection",
          h4(class = "bold-heading", "Data Selection:"),
          class = "custom-text-margin-right",
          p("Wave Dashboard is designed to retrieve a variety of descriptive wave features and use them to predict the Significant Wave Height (SWH) through a specially trained machine learning model.
            After the prediction section, the spatial resolution of the wave data can be enhanced through an bilinear upscaling process. In the final Wave Modulation section of this dashboard, it is possible
            to apply a modulation to the predicted wave field at a specific point in time, allowing to tailor the wave characteristics to particular requirements or scenarios.")
      ),
      div(id = "wiki_data_origin",
        p(class = "bold-heading", "Data Origin:"),
        class = "custom-text-margin-right",
        p("The initial wave data used in this project is provided by the", tags$a(href = "https://www.copernicus.eu/en", "European Union's Earth Observation Program Copernicus"),".",
          "Specifically, the data originates from the", tags$a(href = "https://data.marine.copernicus.eu/viewer/expert?view=dataset&dataset=GLOBAL_ANALYSISFORECAST_WAV_001_027", "Global Ocean Waves Analysis and Forecast"), 
          "dataset, a global oceanographic wave dataset with a temporal interval of 3h and a spatial resolution of 0.083° x 0.083°, which roughly translates to an area of 9.2 km x 8 km. While this resolution is sufficient for large scale
          oceanographic analysis, it is often too coarse for applications requiring high spatial precision, i. e. location optimization of ocean energy buoys. To efficiently predict the SWH wave fields, this
          dashboard requires only four descriptive variables which are:"),
        p(
        tags$ul(
          tags$li(tags$b("VCMX:"), " Sea surface wave maximum height"),
          tags$li(tags$b("VHM0_SW1:"), " Sea surface primary swell wave significant height"),
          tags$li(tags$b("VTM02:"), " Sea surface wave mean period from variance spectral density second frequency moment"),
          tags$li(tags$b("VTM01_WW:"), " Sea surface wind wave mean period"))),
      ),  
      div(id = "wiki_data_steps",
        p(class = "bold-heading", "Steps to perform data retrival:"),
        tags$ol(
          tags$li("Select a desired date or timeframe"),
          tags$li("Select area on the map by clicking at two points"),
          tags$li("Press API-CALL to start the data retrival"),
        ),
        p("Depending on the selected area and time period the download process will take a moment.
          The small gear icon in the wave plot header allows browsing the map resource in three hour increments within the selected time period."),
        br(),
      ),
      
      div(id = "wiki_pred_upscaling",
          h4(class = "bold-heading", "Wave Field Prediction & Upscaling"),
          p("The model training within this dashboard was conducted using the machine learning framework mlr3. To learn more about the capabilities of this framework, please consider visiting the ",
            tags$a(href = "https://mlr3book.mlr-org.com/", "mlr3 Book"),".")
      ),
      
      div(id = "wiki_pred_upscaling_model",
        p(class = "bold-heading", "Model Training:"),
        class = "custom-text-margin-right",
        p("During model training, Gaussian Process, Kriging, Random Forest, and an Extreme Gradient Boosting learner were evaluated. The results showed that
          decision tree based learners, particularly XGBoost, delivered highly accurate predictions while also demonstrating strong efficiency in terms of
          feature selection and computation time. As a result, the Wave Dashboard employs the XGBoost algorithm for wave field prediction. The model was trained
          on data from four selected zones, each covering an area of approximately 50 × 50 km, using data from the whole year of 2024. A map of the selected
          research area and zones is provided below:"),
        br(),
        # accordeon panel of research are
        accordion(
          open = FALSE,
          accordion_panel(
            HTML("<h5>Map of Research Area:</h5>"),
            width = 12,
            height = "100%",
            # plot out put sine wave example
            plotOutput("gg_map_example", width = "100%", height = "600px"),
          ) 
        ),
        br(),
        
        p("To assess the quality of the model predictions, Wave Dashboard not only shows a plot, comparing predicted and actual SWH, but also evaluate the results using the performance metrics listed below.
          For more information on performance metrics, please consider:", tags$a(href = "https://github.com/NannyML/The-Little-Book-of-ML-Metrics", "The Little Book of ML Metrics"),"."), 
        p(class = "bold-heading", "Mean Absolute Error (MAE): $$\\text{MAE} = \\frac{1}{n} \\sum_{i=1}^{n} | y_i - \\hat{y}_i |$$"),
          tags$ul(
            tags$li("Number of Observations (\\(n\\)): total number of data points"),
            tags$li("Actual Values (\\(y_i\\)): observed true values"),
            tags$li("Predicted Values (\\(\\hat{y}_i\\)): model predictions"),
            tags$li("MAE measures the average magnitude of errors in a set of predictions")
        ),
        br(),
          
          p(class = "bold-heading", "Root Mean Squared Error (RMSE): $$\\text{RMSE} = \\sqrt{\\frac{1}{n} \\sum_{i=1}^{n} (y_i - \\hat{y}_i)^2}$$"),
          tags$ul(
            tags$li("Number of Observations (\\(n\\)): total number of data points"),
            tags$li("Actual Values (\\(y_i\\)): observed true values"),
            tags$li("Predicted Values (\\(\\hat{y}_i\\)): model predictions"),
            tags$li("RMSE measures the square root of the average squared differences between prediction and actual observation with special emphasis on extreme values")
        ),
        br(),
          
          p(class = "bold-heading", "Mean Absolute Percentage Error (MAPE): $$\\text{MAPE} = \\frac{100}{n} \\sum_{i=1}^{n} \\left| \\frac{y_i - \\hat{y}_i}{y_i} \\right|$$"),
          tags$ul(
            tags$li("Number of Observations (\\(n\\)): total number of data points"),
            tags$li("Actual Values (\\(y_i\\)): observed true values"),
            tags$li("Predicted Values (\\(\\hat{y}_i\\)): model predictions"),
            tags$li("MAPE expresses accuracy as a percentage and is scale independent")
          ),
        br(),
      ),  
      
      div(id = "wiki_pred_upscaling_wave",
        p(class = "bold-heading", "Wave Field Upscaling:"),
        class = "custom-text-margin-right",
        p("The upscaling process in this application is implemented using the ", tags$a(href = "https://cran.r-project.org/package=terra", "terra"), "R package.", 
          "Internally, terra uses the method of bilinear interpolation, which means estimating values at any point within a 2D grid by using a weighted average
          of the four nearest known grid points. To learn more about bilinear interpolation, consider the following resource:", tags$a(href = "https://x-engineer.org/bilinear-interpolation/", "Bilinear interpolation"),"."
        ),

      ),
      br(),
      br(),
      div(id = "wiki_modulation",
        h4(class = "bold-heading", "Wave Field Modulation"),
        class = "custom-text-margin-right",
        p("To provide a broader range of data, Wave Dashboard allows the modulation of individual wave fields, which can be combined with wave fields forecasted by the machine learning model. Since the forecasted
          realistic wave landscapes may not always exhibit the specific characteristics needed to address certain research requirements. Potential applications include, i. e. training optimization algorithms for
          the placement of wave energy converters."),
      ),
      
      div(id = "wiki_wave_mod_steps",
        p(class = "bold-heading", "Steps to perform wave field modulation:"),
        tags$ol(
          tags$li("To reproduce the results, set a seed to fix the random number generator."),
          tags$li("Select a specific date input of the predicted wave field data."),
          tags$li("Select the disired input parameters for the modulation wave field."),
        ),
        br(),
      ),
      
      div(id = "wiki_wave_mod_explanation",
        p(class = "bold-heading", "Explanation:"),
        class = "custom-text-margin-right",
        p("Superposition and interference are fundamental principles to understand the interaction of different wave forms (light, sound, water) in physics. For a better comprehension,
          a simple demos is shown below in which the parameters of two simple sine waves can be adjusted. Depending on the selected parameters, the overlap (sum) of the sine waves will
          amplify (constructive interference) or flatten (destructive interference) the amplitude and appearance of the superposition wave."),
        
        p("To learn more about the simulation of 3d water surfaces, please consider: ", tags$a(href = "https://developer.nvidia.com/gpugems/gpugems/part-i-natural-effects/chapter-1-effective-water-simulation-physical-models",
            "\"Chapter 1. Effective Water Simulation from Physical Models\" of GPU Gems, by Mark Finch & Cyan Worlds. "), "The wave field modulation in this Wave Dashboard is a static representation of the sum of sines approximation in this resource."),
        
        p(class = "bold-heading", "Equation 1: $$\\text{Individual Superposition Wave} \\, (x) = \\sum_{i=1}^{n} a_i \\times \\sin(f_i \\cdot x + p_i)$$"),
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
          tags$li("Wave Field (\\(W\\)): sum of individual sine waves"),
          tags$li("Direction \\((\\vec{d})\\): relates to wave direction vector in radians \\(\\vec{d}(x, y) = \\left( cos \\cdot \\varphi \\, , \\, sin \\cdot \\varphi \\right)\\)"),
        ),
        br(),
        br(),
        p("As the waves generated are strongly characterized by the typical sine wave form, only a very smooth wave field is obtained. To give the waves sharper peaks and wider
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
        p("At this point the wave field is the sum of the individual sine waves, this implements that there is no guarantee that the selected input criteria and ultimately
          the selected wave heights are met. Therefore, it is necessary to normalize the wave landscape between the given intervals, which are defined by the amplitude range min
          and max values. The formula which is utilized is given in Equation 4. To learn more about feature normalization and scaling please consider the following resource: ",
          tags$a(href = "https://www.atoti.io/articles/when-to-perform-a-feature-scaling/", "\"When to perform a Feature Scaling?\", by Raghav Vashisht"),"."),
        p(class = "bold-heading", "Equation 4: $$\\text{W'} = \\min(a) + \\frac{(W - \\min(W)) \\cdot (\\max(a) - \\min(a) )}{\\max(W) - \\min(W)}$$"),
        tags$ul(
          tags$li("Normilized Wave Field (\\(W'\\)): input normalized modulated wave field"),
          tags$li(HTML("Maximum Wave Field \\(\\max(W)\\): max value of of wave field matrix")),
          tags$li(HTML("Minimum Wave Field \\(\\max(W)\\): minimum value of of wave field matrix")),
          tags$li(HTML("Maximum Amplitude \\(\\max(a)\\): max value of amplitude of wave field matrix")),
          tags$li(HTML("Minimum Amplitude \\(\\min(a)\\): min value of amplitude of wave field matrix")),
        ),
        br(),
        withMathJax(
          p(HTML("The generated and normalized wave field \\(W'\\) is combined with the predicted wave field \\(W_{\\text{pred}}\\) by the surrogate model to obtain the final modulated wave field \\(W_{\\text{mod}}\\)."))
        ),
        p(class = "bold-heading", "Equation 5: $$\\text{W}_{\\text{mod}} = \\text{W}' + \\text{W}_{\\text{pred}}$$"),
          tags$ul(
            tags$li("Modulated Wave Field (\\(W_{\\text{mod}}\\)): combined modulated wave field"),
            tags$li("Normalized Wave Field (\\(W'\\)): input normalized modulated wave field"),
            tags$li("Predicted Wave Field (\\(W_{\\text{pred}}\\)): wave field predicted by the surrogate model")
          ),
        br(),
        br(),
    ),
  )
  ),
),


# -------- defining about nav panel -------------------------------------------
  
  nav_panel(
    title = div(class = "dashboard-heading", "About"),
    fluidPage(
                  title = h2("About"),
                  p("This dashboard was developed as part of the master’s thesis", tags$b("\"A Machine Learning-Based Approach for Efficient and High-Resolution Modeling of Ocean Waves\""),  "at the "
                    , tags$a(href = "https://tu-dresden.de/bu/verkehr/ivw/bda?set_language=en", "Chair of Big Data Analytics in Transportation (TT)"), "of TUD Dresden University of Technology.",
                    "The aim is to provide an intuitive and efficient interface for ocean wave data prediction, with additional options to upscale and modulate the results."),
                  p("The dashboard was created using the software ", tags$a("R", href = "https://www.r-project.org/"), " in conjunction with the ", tags$a("shiny", href = "https://shiny.posit.co/"), "package.",
                    "Shiny provides a framework for building interactive web applications and enables dynamic user interfaces with real time responses."),
                  p("For any questions or issues, feel free to use the email address below:"),
                  p(tags$a(href = "mailto:wave-dashboard@outlook.com", "WAVE-Dashboard@outlook.com")),
                )
  ),
  
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)


# ------ server section --------------------------------------------------------
server <- function(input, output, session) {
  
# -------- time slider inputs and in dashboard navigation functions ------------

# function to update progress
update_progress <- function(progress) {
  if (progress$getValue() < progress$getMax()) {
    progress$inc(1)
  }
}

# synchronize date picker on wave prediction panel
  observeEvent(input$custom_datepicker, {
    output$synced_datepicker <- renderUI({
      airDatepickerInput(
        inputId = "synced_datepicker",  
        multiple = 2,
        value = input$custom_datepicker,       
        minDate = "2021-10-01 06:00:00",
        clearButton = TRUE,
        timepicker = TRUE,
        todayButton = TRUE,
        addon = "none",
        position = "bottom right",
        range = TRUE,
        toggleSelected = FALSE,
        timepickerOpts = timepickerOptions(timeFormat = "HH:mm:00", hoursStep = 3, minutesStep = 60)
      )
    })
  })
  
# observe if synchronized date input gets changed, if yes update date input on data selection panel
# important to check if length of synced_datepicker is 2 otherwise the update function will close input panel 
observeEvent(input$synced_datepicker, {
  if (!is.null(input$synced_datepicker) && length(input$synced_datepicker) == 2) {
    updateAirDateInput(session, "custom_datepicker", value = input$synced_datepicker)
    }
  }, ignoreInit = TRUE)


# navigation within the dashboard buttons (data selection to wave prediction)
observeEvent(input$nav_to_prediction, {
    updateNavbarPage(session, inputId = "navbar", selected = "wave_prediction")
  })

# navigation within the dashboard buttons (wave prediction to wave modulation)
observeEvent(input$nav_to_wave_modulation, {
    updateNavbarPage(session, inputId = "navbar", selected = "wave_modulation")
  })


# defining a slider input for the card date (used in popover)
output$timeSlider <- renderUI({
    page_fillable(
      sliderInput("wave_plot_date", "Select Date & Time:",
                min = as.POSIXct(input$custom_datepicker[1]),                   
                max = as.POSIXct(input$custom_datepicker[2]),
                value = as.POSIXct(input$custom_datepicker[1]),
                # when values are in date-times, steps need to specified in sec. here 3h steps 
                step = 3*3600,                                                 
                ticks = TRUE, 
                animate = FALSE,
                timeFormat = "%Y-%m-%d %H:%M") 
      )
})


# -------- data selection map functions ----------------------------------------

# initial leaflet map 
output$map <- renderLeaflet({
leaflet() %>%
  setView(lng = 0, lat = 4, zoom = 2.15)%>% 
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)
  
})


# observe if the input of the wave plot date slider is changing and render the "animation" map
observeEvent(c(input$dark_mode, input$wave_plot_date), {
  
  leafletProxy("map") %>%
    #clearTiles() %>%
    removeTiles(layerId = "initial_layer_wave_map")%>%
    addProviderTiles(
      if (input$dark_mode == "dark") {
        "CartoDB.DarkMatter"
      } else {
        "CartoDB.Positron"
      })%>%
   # GLOBAL_ANALYSISFORECAST_WAV_001_027/cmems_mod_glo_wav_anfc_0.083deg_PT3H-i_202411
    # when clicking on the popover gear icon the map is rendered by altering the time attribute in the url, this is depending on the wave_plot_date slider
    addTiles(urlTemplate = paste0("https://wmts.marine.copernicus.eu/teroWmts/?service=WMTS&version=1.0.0&request=GetTile&layer=GLOBAL_ANALYSISFORECAST_WAV_001_027/cmems_mod_glo_wav_anfc_0.083deg_PT3H-i_202411/VHM0&tilematrixset=EPSG:3857@3x&tilematrix={z}&tilerow={y}&tilecol={x}&time=", format(as.POSIXct(input$wave_plot_date) + 3600, "%Y-%m-%dT%H:%M:%S.000Z")), 
             layerId = "wave_map_animation",
             attribution = '&copy; <a href="https://data.marine.copernicus.eu/viewer/expert?view=viewer&crs=epsg%3A4326&t=1712232000000&z=0&center=0%2C0&zoom=9.843922654284418&layers=W3sib3BhY2l0eSI6MSwiaWQiOiJjMSIsImxheWVySWQiOiJHTE9CQUxfQU5BTFlTSVNGT1JFQ0FTVF9XQVZfMDAxXzAyNy9jbWVtc19tb2RfZ2xvX3dhdl9hbmZjXzAuMDgzZGVnX1BUM0gtaV8yMDIzMTEvVkhNMCIsInpJbmRleCI6MjAsImxvZ1NjYWxlIjpmYWxzZX1d&basemap=dark" target="_blank">Copernicus Marine Service</a>')
      
  })

# render initial map, if wave plot date slider was adjusted remove the render layer
observeEvent(c(input$dark_mode, input$custom_datepicker), {
  
  leafletProxy("map") %>%
    #clearTiles() %>%
    removeTiles(layerId = "wave_map_animation")  %>%
    addProviderTiles(
      if (input$dark_mode == "dark") {
        "CartoDB.DarkMatter"
      } else {
        "CartoDB.Positron"
      })%>%
    # initial map setup using the date picker input for rendering copernicus data
    addTiles(urlTemplate = paste0("https://wmts.marine.copernicus.eu/teroWmts/?service=WMTS&version=1.0.0&request=GetTile&layer=GLOBAL_ANALYSISFORECAST_WAV_001_027/cmems_mod_glo_wav_anfc_0.083deg_PT3H-i_202411/VHM0&tilematrixset=EPSG:3857@3x&tilematrix={z}&tilerow={y}&tilecol={x}&time=", format(as.POSIXct(input$custom_datepicker[1]) + 3600, "%Y-%m-%dT%H:%M:%S.000Z")),
             layerId = "initial_layer_wave_map",
            attribution = '&copy; <a href="https://data.marine.copernicus.eu/product/GLOBAL_ANALYSISFORECAST_WAV_001_027/description" target="_blank">Copernicus Marine Service</a>')
  })


# define selected_bounds as reactive value
selected_bounds <- reactiveValues(bounds = NULL)

# if else loop for selecting area on map
# creates a list of bounds in form of: (lat1, lon1, lat2, lon2) which are the corners of a rectangle
# source: https://gis.stackexchange.com/questions/345610/shiny-leaflet-click-event-returns-null-when-clicked-again

observeEvent(input$map_click, {

  # each map click is saved to click 
  click <- input$map_click
  
  # if click is empty (first click), the click data will be save es the first two coordinates in selected_bounds$bounds
  if (!is.null(click)) {
    lat <- click$lat
    lon <- click$lng
    if (is.null(selected_bounds$bounds)) {
      selected_bounds$bounds <- c(lat, lon, NA, NA)
      
    # when click is not empty (second click), the coordinates will be saved as the third and fourth value of selected_bounds$bounds
    } else if (is.na(selected_bounds$bounds[3]) || is.na(selected_bounds$bounds[4])) {
      selected_bounds$bounds[3] <- lat
      selected_bounds$bounds[4] <- lon
      
      # the selected_bounds will be plotted as a rectangle over the initial map
      leafletProxy("map") %>%
        addRectangles(
          lng1 = selected_bounds$bounds[2], lat1 = selected_bounds$bounds[1],
          lng2 = selected_bounds$bounds[4], lat2 = selected_bounds$bounds[3],
          stroke = TRUE, weight = 1, color = "rgba(0, 179, 252, 1)"
        )
      
      # a third click will clear the previous leaflet shapes and click information, but also set the new "first click" of the new rectangle
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
    div(class = "map-output-text",
      HTML(paste(
      "<strong>Selected Coordinates:</strong>",
      "&nbsp;<strong>Point 1:</strong>", paste(c(selected_bounds$bounds[1], selected_bounds$bounds[2]), collapse = ", "),
      "&nbsp;<strong>Point 2:</strong>", paste(c(selected_bounds$bounds[3], selected_bounds$bounds[4]), collapse = ", "),
      "&nbsp;<strong>Latitudinal Distance:</strong>", paste(round(lat_distance, 3), "km"),
      "&nbsp;<strong>Longitudinal Distance:</strong>", paste(round(lon_distance, 3), "km")
      ))
    )
  } else {
    div(class = "map-output-text",
      HTML("Select a date or time period and click at two points on the map to define an area.")
    )
  }
})
  

# update progress function
  update_progress <- function(progress, step) {
    if (progress$getValue() < progress$getMax()) {
      progress$inc(1)
      progress$set(message = step)
    }
  }


# -------- data api call functions ---------------------------------------------

# define reactive value for data_to_upscale (prediction result data) 
data_to_upscale <- reactiveValues(data = NULL)

# api call parameters
observeEvent(input$start_api_call, {
  
  # to prevent downloading of the entire data set, check if selected_bounds$bounds is empty
  if (is.null(selected_bounds$bounds) || anyNA(selected_bounds$bounds)) {
  # show error massage
  showNotification("No area selected!", type = "error")
  } else 
  
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
  update_progress(progress, "API-Call started, this will take a moment")
  
  # api call command (reticulate python function)
  CopernicusMarine$subset(
    dataset_id= "cmems_mod_glo_wav_anfc_0.083deg_PT3H-i",               
    dataset_part = "default",
    service = "arco-time-series",
    variables= r_to_py(list("VHM0", "VCMX", "VHM0_SW1", "VTM01_WW", "VTM02")),                        
    minimum_longitude = lon[1],
    maximum_longitude = lon[2],
    minimum_latitude = lat[1],
    maximum_latitude = lat[2],
    start_datetime = date_min,
    end_datetime = date_max,
    output_filename = "dashboard_data.nc",
    output_directory = working_directory,,                                     
    overwrite = "True")
  
  
  # open netcdf file
  data_cop <- nc_open(paste0(working_directory, "dashboard_data.nc"))
  
  # get time dimensions
  dim_lon <- ncvar_get(data_cop, "longitude")
  dim_lat <- ncvar_get(data_cop, "latitude")
  dim_time <- ncvar_get(data_cop, "time")
  
  # format date and time 
  t_units <- ncatt_get(data_cop, "time", "units")
  t_ustr <- strsplit(t_units$value, " ")
  t_dstr <- strsplit(unlist(t_ustr)[3], "-")
  date <- ymd(t_dstr) + dhours(dim_time)
  
  # extract coordinates  and create grid of lat, lon, time
  coords <- as.data.frame(expand.grid(dim_lon, dim_lat, date), stringsAsFactors = FALSE)
  attr(coords, "out.attrs") <- NULL
  names(coords) <- c("lon", "lat", "time")
  
  # get all variable names
  nc_var <- names(data_cop$var)
  
  # create empty list
  wave_cop <- list()
  
  # loop for extracting every variable and add to data matrix
  for (var in nc_var) {
    wave_cop[[var]] <- data.matrix(ncvar_get(data_cop, var, collapse_degen=FALSE))
  }
  
  # close the netcdf file
  nc_close(data_cop)
  
  # create finial data table
  wave <- as.data.table(bind_cols(coords, wave_cop))
  
  # omit na's in case missing values
  wave <- na.omit(wave)
  
  # progress update 2: wave data loaded and prepared
  update_progress(progress, "Wave data loaded")
  
  # group selected data by time in order to plot mean time series of selected area
  wave_summary <- wave %>%
  group_by(time) %>%
  summarise(
    VHM0     = round(mean(VHM0, na.rm = TRUE), 2),
    VCMX     = round(mean(VCMX,      na.rm = TRUE), 2),
    VHM0_SW1 = round(mean(VHM0_SW1,  na.rm = TRUE), 2),
    VTM01_WW = round(mean(VTM01_WW,  na.rm = TRUE), 2),
    VTM02    = round(mean(VTM02,  na.rm = TRUE), 2)
  )
  
# ---------- plot mean predictive variables ------------------------------------
  # ploty output mean time series from selected area for variables measured in meters
  output$ts_wave_meters_plot <- renderPlotly({
    plot_ly() %>%
    add_trace(data = wave_summary, x = ~time, y = ~VCMX, type = "scatter", mode = "lines",
              name = "VCMX [m]", line = list(color = "rgba(198, 229, 10, 0.95)", width = 4)) %>%
    add_trace(data = wave_summary, x = ~time, y = ~VHM0, type = "scatter", mode = "lines",
              name = "SWH [m]", line = list(color = "rgba(71, 40, 118, 0.95)", width = 4)) %>%
    add_trace(data = wave_summary, x = ~time, y = ~VHM0_SW1, type = "scatter", mode = "lines",
              name = "VHM0_SW1 [m]", line = list(color = "rgba(37, 161, 136, 0.95)", width = 4)) %>%
    add_trace(data = wave_summary, x = ~time, y = ~VTM02, type = "scatter", mode = "lines", visible = "legendonly",
              name = "VTM02 [s]", line = list(color = "rgba(0, 179, 252, 0.95)", width = 4)) %>%
    add_trace(data = wave_summary, x = ~time, y = ~VTM01_WW, type = "scatter", mode = "lines", visible = "legendonly",
              name = "VTM01_WW [s]", line = list(color = "rgba(255, 148, 31, 0.95)", width = 4)) %>%
    layout(
      title = "",
      xaxis = list(
        title = "Date",
        gridcolor = "lightgrey",
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "cursor",
        showline = TRUE,
        spikethickness = 1,
        rangeselector = list(  
          buttons = list(
            list(count = 7, label = "1W", step = "day", stepmode = "backward"),
            list(count = 1, label = "1M", step = "month", stepmode = "backward"),
            list(count = 3, label = "3M", step = "month", stepmode = "backward"),
            list(count = 6, label = "6M", step = "month", stepmode = "backward"),
            list(step = "all", label = "All")
          ),
          font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default", family = "Arial"), 
          activecolor = if (input$dark_mode == "dark") "grey" else "default",           
          bgcolor = "transparent"              
        ),
        rangeslider = list(visible = TRUE)
      ),
      yaxis = list(
        title = "Mean Values",
        gridcolor = "lightgrey"
      ),
      showlegend = TRUE, 
      hovermode = "x unified",
      hoverlabel = list(
        bgcolor = "#444444",
        bordercolor = "#f7f7f7",
        font = list(color = "#f7f7f7")
      ),
      plot_bgcolor  = "transparent",
      paper_bgcolor = "transparent",
      font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default", family = "Arial", size = 15),
      modebar = list(color = "lightgrey",
                     activecolor = "grey",
                     bgcolor = "transparent")
    )
  })

  
# ---------- plot predicted wave field -----------------------------------------  
  # create new prediction dataset
  pred_data <- wave %>%
                select(lat, lon, time, VHM0, VCMX, VHM0_SW1, VTM01_WW, VTM02) 
  
  # in domain normal (10080 obs)
  pred_xgboost <- xgboost$predict_newdata(final_tsk, newdata = pred_data)
  pred_results <- pred_xgboost$score(msrs(list("regr.mae", "regr.rmse", "regr.mape")))
  
  # add prediction to selected area dataset 
  pred_data$VHM0_pred <- pred_xgboost$response
  
  data_to_upscale$data <- pred_data
  
  # plotly output of predicted wave field
  output$wave_field_plot <- renderPlotly({
    
    pred_data %>%
      plot_ly(
        x= ~lat, 
        y= ~lon, 
        z= ~VHM0_pred,
        color = ~VHM0_pred,
        frame = as.character(pred_data$time),
        mode ='markers',
        marker = list(size = 5),
        type = 'scatter3d') %>%
      layout(
        modebar = list(color = "lightgrey",
                       activecolor = "grey",
                       bgcolor = "transparent"),
        plot_bgcolor  = "transparent",
        paper_bgcolor = "transparent",
        font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default", family = "Arial", size = 12.5),
        scene = list(
                  xaxis = list(title = "Lat", range = c(max(pred_data$lat), min(pred_data$lat))),
                  yaxis = list(title = "Lon", range = c(min(pred_data$lon), max(pred_data$lon))),
                  zaxis = list(title = "SWH"),
                  aspectratio = list(x = 1, y = 1 , z = 1/3), 
                  aspectmode = "manual",
                  camera = list(eye = list(x = 1.2, y = -1.2, z = 0.45))
                )
      )%>%
      colorbar(title = "SWH in m")
  })
  
  # progress update 3: wave landscape data processed and plotted
  update_progress(progress, "Wave field predicted and plotted")
  
# ---------- plot predicted vs actual data wave field --------------------------  
  
  output$ts_wave_pred_actual_plot <- renderPlotly({
    
    # group selected data by time in order to plot mean time series of selected area
    pred_summary <- pred_data %>%
    group_by(time) %>%
    summarise(
      VHM0      = round(mean(VHM0, na.rm = TRUE), 2),
      VHM0_pred = round(mean(VHM0_pred, na.rm = TRUE), 2)
    )
    
    plot_ly() %>%
    add_trace(data = pred_summary, x = ~time, y = ~VHM0, type = "scatter", mode = "lines",
              name = "SWH [m]", line = list(color = "rgba(71, 40, 118, 0.95)", width = 4)) %>%
    add_trace(data = pred_summary, x = ~time, y = ~VHM0_pred, type = "scatter", mode = "lines",
              name = "SWH Predicted [m]", line = list(color = "rgba(198, 229, 10, 0.95)", width = 4)) %>%
    layout(
      title = "",
      xaxis = list(
        title = "Date",
        gridcolor = "lightgrey",
        showspikes = TRUE,
        spikemode = "across",
        spikesnap = "cursor",
        showline = TRUE,
        spikethickness = 1,
        rangeselector = list(  
          buttons = list(
            list(count = 7, label = "1W", step = "day", stepmode = "backward"),
            list(count = 1, label = "1M", step = "month", stepmode = "backward"),
            list(count = 3, label = "3M", step = "month", stepmode = "backward"),
            list(count = 6, label = "6M", step = "month", stepmode = "backward"),
            list(step = "all", label = "All")
          ),
          font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default", family = "Arial"), 
          activecolor = if (input$dark_mode == "dark") "grey" else "default",           
          bgcolor = "transparent"              
        ),
        rangeslider = list(visible = TRUE)
      ),
      yaxis = list(
        title = "Mean SWH Values",
        gridcolor = "lightgrey"
      ),
      showlegend = TRUE, 
      hovermode = "x unified",
      hoverlabel = list(
        bgcolor = "#444444",
        bordercolor = "#f7f7f7",
        font = list(color = "#f7f7f7")
      ),
      plot_bgcolor  = "transparent",
      paper_bgcolor = "transparent",
      font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default", family = "Arial", size = 15),
      modebar = list(color = "lightgrey",
                     activecolor = "grey",
                     bgcolor = "transparent")
    )
  })
  
  
  # prediction_scores text output under mean prediction plot
  output$prediction_scores <- renderUI({
    # actual text output under the map
    div(class = "map-output-text",
    span(HTML(paste0(
      "<strong>MAE:</strong> ", round(pred_results[1], 3), " m&nbsp;&nbsp;",
      "<strong>RMSE:</strong> ", round(pred_results[2], 3), " m&nbsp;&nbsp;",
      "<strong>MAPE:</strong> ", round(pred_results[3], 3), " %"
    )))
  )  
  })


  #progress update 5: upscaled wave field created
  update_progress(progress, "API Call done")
  
  
})


# ---------- plot upscaled wave field -----------------------------------------  

# define reactive value for upscaled waveplot
data_to_modulate <- reactiveValues(data = NULL)

observeEvent(c(input$start_api_call,input$upscale_factor), {
  
  # check for reuired values (predicted wave field data)
  req(data_to_upscale$data)
  
  # define data from the reactive values data_to_upscale$data
    data <- data_to_upscale$data %>%
                select(lat, lon, time, VHM0_pred)
    
    # custom function to create a SpatRaster from dataframe
    # source: https://stackoverflow.com/questions/72135211/how-to-convert-dataframe-to-rasterstack-object
    #         https://stackoverflow.com/questions/73721955/terra-go-back-from-data-frame-to-raster-object-inverse-of-as-data-framex-cel
    #         https://stackoverflow.com/questions/59059923/r-loop-over-raster-objects-objects
    
    # this function will create a Spatraster of one variable with all unique time points and essentially
    # recreates how terra is reading netcdf files
    # in order to work the functions requires:
    # the dataframe to be named: "data"
    # geo location cols to be named: "lon", "lat"
    # time variable named: "time"
    
    create_variable_rasterstack <- function(var_name) {
    
    # extract and sort unique time points for selected variable
    t_unique<- sort(unique(data$time))
    
    # iterate over all time points given by t_unique and create raster
    rast_layers <- lapply(seq_along(t_unique), function(i) {
      # select time point i
      t <- t_unique[i]
      # create raster of variable and time point i
      rast <- rast(select(filter(data, time == t), lon, lat, all_of(var_name)), type = "xyz", crs = "EPSG:4326")
    
      # add the iteration index to variable name (optional: recreates exact naming scheme of how terra reads netcdf)
      #names(rast) <- paste0(var_name, "_", i)
    
      return(rast)
    })
    
    # create a terra Spatraster object for one variable with all time points
    rast(rast_layers)
    }
    
    # get all variable names from data frame which are not lon, lat, time
    all_feat <- setdiff(names(data), c("lon", "lat", "time"))
    
    # interate over all variables and create a list of rasters
    raster_list <- lapply(all_feat, create_variable_rasterstack)
    
    # convert the list of rasters to a Spatraster object
    data_spatraster <- rast(raster_list)
    
    # match the the number of layers for the time argument by repeating the unique times of data
    repeated_times <- rep(unique(data$time), length.out = nlyr(data_spatraster))
    
    # set time attribute of spatraster
    time(data_spatraster) <- repeated_times
    
    # interpolate by upscaling factor
    # aggregate SpatRaster-Stack to lower resolution https://stackoverflow.com/questions/71969689/understanding-aggregate-from-the-terra-package-in-r
    data_upscaled <- disagg(data_spatraster, fact = input$upscale_factor, method = "bilinear")
    
    # convert back to dataframe for defining mlr task
    data_upscaled <- as.data.frame(data_upscaled, xy = TRUE, time = TRUE, wide = FALSE)
    
    # convert back into wide format
    data_upscaled_df <- data_upscaled %>%
    pivot_wider(
      names_from = layer,
      values_from = values
    ) %>%
    rename(lon = x, lat = y)
  
  # save upscaled dataframe as reactive values for plot and modulation panel
  data_to_modulate$data <- data_upscaled_df  

})


output$upscaled_wave_field_plot <- renderPlotly({
  # check for required values
  req(data_to_modulate$data)
  
  # define reactive values as plot data 
  data_upscaled_df <- data_to_modulate$data 
  
  # actual upscaled wave field plot
  data_upscaled_df %>%
    plot_ly(
      x= ~lat,
      y= ~lon,
      z= ~VHM0_pred,
      color = ~VHM0_pred,
      frame = as.character(data_upscaled_df$time),
      mode ='markers',
      marker = list(size = 5),
      type = 'scatter3d') %>%
    layout(
      modebar = list(color = "lightgrey",
                     activecolor = "grey",
                     bgcolor = "transparent"),
      plot_bgcolor  = "transparent",
      paper_bgcolor = "transparent",
      font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default", family = "Arial", size = 12.5),
      scene = list(
                xaxis = list(title = "Lat", range = c(max(data_upscaled_df$lat), min(data_upscaled_df$lat))),
                yaxis = list(title = "Lon", range = c(min(data_upscaled_df$lon), max(data_upscaled_df$lon))),
                zaxis = list(title = "SWH"),
                aspectratio = list(x = 1, y = 1 , z = 1/3),
                aspectmode = "manual",
                camera = list(eye = list(x = 1.2, y = -1.2, z = 0.45))
              )
    )%>%
    colorbar(title = "SWH in m")
})


# ---------- plot massages when no api call is executed ------------------------ 

# display text when no api call executed for wave_field_plot
output$wave_field_pred_output <- renderUI({
  if (input$start_api_call == 0) {
    div(
      p("Wating for API Call..."),
      style = "display: flex; align-items: center; justify-content: center; height: 100%;"
    )
  } else {
    plotlyOutput("wave_field_plot")
  }
})

# display text when no api call executed for upscaled_wave_field_plot
output$upscaled_wave_field_output <- renderUI({
  if (input$start_api_call == 0) {
    div(
      p("Wating for API Call..."),
      style = "display: flex; align-items: center; justify-content: center; height: 100%;"
    )
  } else {
    plotlyOutput("upscaled_wave_field_plot")
  }
})

# display text when no api call executed for ts_wave_meters_plot
output$ts_wave_meters_output <- renderUI({
  if (input$start_api_call == 0) {
    div(
      p("Wating for API Call..."),
      style = "display: flex; align-items: center; justify-content: center; height: 100%;"
    )
  } else {
    plotlyOutput("ts_wave_meters_plot")
  }
})

# display text when no api call executed for ts_wave_pred_actual_plot
output$ts_wave_pred_actual_output <- renderUI({
  if (input$start_api_call == 0) {
    div(
      p("Wating for API Call..."),
      style = "display: flex; align-items: center; justify-content: center; height: 100%;"
    )
  } else {
    plotlyOutput("ts_wave_pred_actual_plot")
  }
})

# ---------- download handling prediction panel --------------------------------  

# download handling for upsacled wave field data
output$download_upscaled_wave_data <- downloadHandler(
  filename = "upscaled_wave_data.csv",
  content = function(file) {
  # define data from the reactive values data_to_upscale$data
    data <- data_to_upscale$data 
    
    create_variable_rasterstack <- function(var_name) {
    
    # extract and sort unique time points for selected variable
    t_unique<- sort(unique(data$time))
    
    # iterate over all time points given by t_unique and create raster
    rast_layers <- lapply(seq_along(t_unique), function(i) {
      # select time point i
      t <- t_unique[i]
      # create raster of variable and time point i
      rast <- rast(select(filter(data, time == t), lon, lat, all_of(var_name)), type = "xyz", crs = "EPSG:4326")
      return(rast)
    })
    
    # create a terra Spatraster object for one variable with all time points
    rast(rast_layers)
    }
    
    # get all variable names from data frame which are not lon, lat, time
    all_feat <- setdiff(names(data), c("lon", "lat", "time"))
    
    # interate over all variables and create a list of rasters
    raster_list <- lapply(all_feat, create_variable_rasterstack)
    
    # convert the list of rasters to a Spatraster object
    data_spatraster <- rast(raster_list)
    
    # match the the number of layers for the time argument by repeating the unique times of data
    repeated_times <- rep(unique(data$time), length.out = nlyr(data_spatraster))
    
    # set time attribute of spatraster
    time(data_spatraster) <- repeated_times
    
    # interpolate by upscaling factor
    data_upscaled <- disagg(data_spatraster, fact = input$upscale_factor, method = "bilinear")
    
    # convert back to dataframe for defining mlr task
    data_upscaled <- as.data.frame(data_upscaled, xy = TRUE, time = TRUE, wide = FALSE)
    
    # convert back into wide format
    data_upscaled_df <- data_upscaled %>%
    pivot_wider(
      names_from = layer,
      values_from = values
    ) %>%
    rename(lon = x, lat = y)

    # CSV exportieren
    write.csv(data_upscaled_df, file, row.names = FALSE)
  }
)


# download handling for predicted wave field data
output$download_pred_wave_data <- downloadHandler(
  filename = "predicted_wave_data.csv",
  content = function(file) {
    write.csv(pred_data, file, row.names = FALSE)
  }
)



# -------- wave field modulation functions -------------------------------------

# reactive values to store generated wave parameters
waveParams <- reactiveValues(amplitude = NULL,
                             wavelength = NULL, 
                             phase = NULL, 
                             angle = NULL, 
                             kValue = NULL)

# ---------- dynamic sidebar and modulation nav panel content ------------------  

# dynamic sidebar content
output$dynamic_sidebar <- renderUI({
  if (is.null(data_to_upscale$data)) {
    page_fillable(
    # sidebar content if api data is not present
      textInput("seed", label = div(class = "sidebar-heading", "Seed for Replication:"), value = "Modulation"),
      tags$div(style = "margin-top: 15px;"), 
      sliderInput("number_waves", label = div(class = "sidebar-heading", "Number of Sine Waves:"), min = 1, max = 20, value = 13, step = 1),
      sliderInput("amplitude", label = div(class = "sidebar-heading", "Ampliutude Range:"), min = -20, max = 30, value = c(1, 8), step = 0.1),
      sliderInput("wavelength", label = div(class = "sidebar-heading", "Wavelength Range:"), min = 0, max = 100, value = c(30, 65), step = 1),
      sliderInput("angle", label = div(class = "sidebar-heading", "Direction Range:"), min = 0, max = 360, value = c(0, 70), step = 1),
      sliderInput("k_value", label = div(class = "sidebar-heading", "Wave Steepness Range:"), min = 0, max = 15, value = c(1, 5), step = 0.1),
      sliderInput("phase", label = div(class = "sidebar-heading", "Phase Range:"), min = 0, max = 100, value = c(15, 30), step = 0.5),
      sliderInput("grid_size", label = div(class = "sidebar-heading", "Grid Size:"), min = 0, max = 200, value = 100, step = 10), 
      tags$hr(style = "margin-top: 15px; margin-bottom: 15px;"),
      downloadButton("download_csv_sim_wave", "Download Wave Field")
    )
  
  } else {

    # sidebar content if api data is present
    page_fillable(
      textInput("seed", label = div(class = "sidebar-heading", "Seed for Replication:"), value = "Modulation"),
      tags$div(style = "margin-top: 15px;"), 
      div(style = "display: flex; justify-content: center;",
        sliderInput("wave_modulation_date", label = div(class = "sidebar-heading", "Select Initial Wave Field Time:"),
         min = as.POSIXct(input$custom_datepicker[1], tz = "UTC"),                   
         max = as.POSIXct(input$custom_datepicker[2], tz = "UTC"),
         value = as.POSIXct(input$custom_datepicker[1], tz = "UTC"),
         step = 3 * 3600,
         ticks = TRUE,
         animate = FALSE,
         width = "92.5%",
         timeFormat = "%Y-%m-%d %H:%M"
        )
      ),
      sliderInput("number_waves", label = div(class = "sidebar-heading", "Number of Sine Waves:"), min = 1, max = 20, value = 13, step = 1),
      sliderInput("amplitude", label = div(class = "sidebar-heading", "Ampliutude Range:"), min = -20, max = 30, value = c(-1, 1.5), step = 0.1),
      sliderInput("wavelength", label = div(class = "sidebar-heading", "Wavelength Range:"), min = 0, max = 100, value = c(5, 20), step = 1),
      sliderInput("angle", label = div(class = "sidebar-heading", "Direction Range:"), min = 0, max = 360, value = c(0, 75), step = 1),
      sliderInput("k_value", label = div(class = "sidebar-heading", "Wave Steepness Range:"), min = 0, max = 15, value = c(1, 5), step = 0.25),
      sliderInput("phase", label = div(class = "sidebar-heading", "Phase Range:"), min = 0, max = 100, value = c(15, 30), step = 1),
      tags$hr(style = "margin-top: 15px; margin-bottom: 15px;"),
      downloadButton("download_final_wavefield", "Download Final Wave Field")
    )
  }
})


# page content if api call is not executed 
output$dynamic_cards <- renderUI({
  if (is.null(data_to_upscale$data)) {
    page_fillable(
      layout_column_wrap(
        min_height = "100%",
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(div(class = "dashboard-heading", "Modulation Wave Field")), 
          plotlyOutput("simulated_waveplot")
        ),
        card(
          full_screen = TRUE,
          card_header(div(class = "dashboard-heading", "Modulation Wave Direction")),
          plotlyOutput("simulated_wind_directions")
        )
      )
    )
  
  # page content if api call was executed  
  } else {
    page_fillable(
      layout_column_wrap(
        min_height = "60%",
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(div(class = "dashboard-heading", "Initial Upscaled Wave Field")), 
          plotlyOutput("upscaled_waveplot_surface")
        ),
        card(
          full_screen = TRUE,
          card_header(div(class = "dashboard-heading", "Final Modulation Wave Field")),
          plotlyOutput("final_waveplot_surface")
        )
      ),
      br(),
      layout_column_wrap(
        min_height = "60%",
        width = 1/2,
        card(
          full_screen = TRUE,
          card_header(div(class = "dashboard-heading", "Modulation Wave Field")), 
          plotlyOutput("modulation_wavefield_plot")
        ),
        card(
          full_screen = TRUE,
          card_header(div(class = "dashboard-heading", "Modulation Wave Direction")),
          plotlyOutput("simulated_wind_directions")
        )
      ),
      br(),
    )
  }
})


# ---------- wave modulation calc. based on api call --------------------------- 

# wave modulation calculation based on if api call was already executed 
observe({
  
  # first check if prediction data is already present if not, wave modulation can be executed on its own
  if (is.null(data_to_upscale$data)) {
    
# ---------- plot wave modulation when no api call is executed -----------------
  # check if the wave modulation input variables are already present (if this part is missing it will cause an error)
  req(
    input$number_waves,
    input$amplitude,
    input$wavelength,
    input$phase,
    input$angle,
    input$k_value
  )
  
  # set seed for reproducible wave modulation plots in this form strings as inputs are possible 
  # source: https://mehrad.ai/posts/20231022-use-string-as-random-seed-in-r/
  if (is.null(input$seed) || input$seed == "") {
    set.seed(NULL)  # reset seed
  } else {
    # generate seed using simple sum function
    set.seed(sum(utf8ToInt(input$seed)))
  }
  
  # generate a set of wave parameters, depending on the number of sine waves  
  nWaves <- input$number_waves
  waveParams$amplitude <- runif(nWaves, input$amplitude[1], input$amplitude[2])
  waveParams$wavelength <- runif(nWaves, input$wavelength[1], input$wavelength[2])
  waveParams$phase <- runif(nWaves, input$phase[1], input$phase[2])
  waveParams$angle <- runif(nWaves, input$angle[1], input$angle[2]) 
  waveParams$kValue <- runif(nWaves, input$k_value[1], input$k_value[2])
  
  # create grid boundaries of empty wave matrix 
  x <- seq(1, input$grid_size, by = 1)
  y <- seq(1, input$grid_size, by = 1)
  
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
  
  # wave simulation plot
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
                    color = "lightgrey",                                                          
                    activecolor = "grey",
                    bgcolor = if (input$dark_mode == "dark") "transparent" else "transparent" 
                    ),
      plot_bgcolor  = "transparent",                                                              
      paper_bgcolor = "transparent",                                                             
      font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default")              
      ) %>%
      colorbar(title = "Wave Height")
      
  })
  
  
# ---------- plot wave direction when no api call is executed ------------------
  # simulated wave direction plot
  # creating a new data frame with all wind directions from the input angle vector
  sim_wind <- data.frame(degrees = waveParams$angle)  
  
  # creating empty data frame, with columns for the direction, count and percentage
  summary_sim_wind <- data.frame(
    Direction = c("E", "ENE", "NE", "NNE", "N", "NNW", "NW", "WNW", "W", "WSW", "SW", "SSW", "S", "SSE", "SE","ESE"),
    Max = nrow(sim_wind),
    Count = rep(0, 16),
    Percent = rep(0, 16)
    )

  # using wave direction function and counting wave directions in sim_wind for each row
  for (i in 1:nrow(sim_wind)) {
    sim_wind_direction <- degrees_to_direction(sim_wind$degrees[i])
    rof_index <- match(sim_wind_direction, summary_sim_wind$Direction)  # using the more efficient match() function instead of which() to find row index
    summary_sim_wind$Count[rof_index] <- summary_sim_wind$Count[rof_index] + 1
  }

  # calc. percentage values of wind directions
  summary_sim_wind$Percent <- (summary_sim_wind$Count / sum(summary_sim_wind$Count))

  # create the radar chart
  output$simulated_wind_directions <-renderPlotly({
    plot_ly(summary_sim_wind, type = "scatterpolar", fill = "toself", mode = "lines+markers", hoverinfo = "r+theta") %>%                            
      add_trace(r = summary_sim_wind$Percent, theta = summary_sim_wind$Direction , name = "Direction in %",
                fillcolor = "rgba(48, 106, 142, 0.6)",
                line = list(color = "rgba(19, 86, 149, 0.8)"),
                marker = list(color = "rgba(198, 224, 42, 1)")) %>%
      layout(
             modebar = list(color = "lightgrey", activecolor = "grey", bgcolor = if (input$dark_mode == "dark") "transparent" else "transparent"),  
             polar = list(bgcolor = "transparent", radialaxis = list(tickformat = ".0%")),                                                          
             paper_bgcolor = "transparent",                                                                                                         
             font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default", family = "Arial", size = 14.5))                                                    
  })
  

  # modulated wave field download section
  output$download_csv_sim_wave <- downloadHandler(
    filename = "modulation_wave_field.csv",
    content = function(file) {
      write.csv(landscape, file, row.names = FALSE)
    }
  )
  
  } else {
  
# ---------- plot wave modulation when api call is present ---------------------
  # check if the wave modulation input variables are already present (if this part is missing it will cause an error)
  req(
    input$number_waves,
    input$amplitude,
    input$wavelength,
    input$phase,
    input$angle,
    input$k_value,
    input$wave_modulation_date
  )
  
  # set seed for reproducible wave modulation plots in this form strings as inputs are possible 
  # source: https://mehrad.ai/posts/20231022-use-string-as-random-seed-in-r/
  if (is.null(input$seed) || input$seed == "") {
    set.seed(NULL)  # reset seed
  } else {
    # generate seed using simple sum function
    set.seed(sum(utf8ToInt(input$seed)))
  }
  
  # generate a set of wave parameters, depending on the number of sine waves  
  nWaves <- input$number_waves
  waveParams$amplitude <- runif(nWaves, input$amplitude[1], input$amplitude[2])
  waveParams$wavelength <- runif(nWaves, input$wavelength[1], input$wavelength[2])
  waveParams$phase <- runif(nWaves, input$phase[1], input$phase[2])
  waveParams$angle <- runif(nWaves, input$angle[1], input$angle[2]) 
  waveParams$kValue <- runif(nWaves, input$k_value[1], input$k_value[2])
  
  # import the predicted wave field data for modulation
  df_up_mod <- data_to_modulate$data

  # filter wave_up_mod according to selected date input, adding 3600 sec in order to add 1h (time mismatch between slider input needs to be investigated)
  wave_up_mod <- df_up_mod %>%
                    select(lat, lon, time, VHM0_pred) %>%
                    filter(time == as.POSIXct(input$wave_modulation_date, tz = "UTC") + 3600)

  # creating a matrix of wave landscape
  # source: https://stackoverflow.com/questions/60369552/how-do-i-create-a-3d-surface-plot-in-r-if-i-have-a-dataframe-of-3-columns
  wave_up <- xtabs(VHM0_pred ~ lon + lat, data = wave_up_mod)

  # at the moment only symmetrical matrices work for this application so the first step is to find the minimum dimension of wave_up 
  wave_up_sym <- wave_up[1:min(ncol(wave_up), nrow(wave_up)), 1:min(ncol(wave_up), nrow(wave_up))]

  # create grid boundaries of wave field
  x <- seq(1, min(ncol(wave_up), nrow(wave_up)), by = 1)
  y <- seq(1, min(ncol(wave_up), nrow(wave_up)), by = 1)

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

  # add created wave field to original prediction wave field
  modulated_wave_field <- landscape + wave_up_sym
  
  
  # modulated wave field download section
  output$download_final_wavefield <- downloadHandler(
    filename = "final_modulation_wave_field.csv",
    content = function(file) {
      write.csv(modulated_wave_field, file, row.names = FALSE)
    }
  )
  
  # plot initial symmetric wave field to upscale
  output$upscaled_waveplot_surface <- renderPlotly({
    
    plot_ly() %>%
     add_surface(
       # defining the x and y axis as numeric dimension names of symmetric wave matrix 
       x = as.numeric(dimnames(wave_up_sym)[[2]]),
       y = as.numeric(dimnames(wave_up_sym)[[1]]),
       z = wave_up_sym
      ) %>%
      layout(
        modebar = list(color = "lightgrey",
                       activecolor = "grey",
                       bgcolor = "transparent"),
        plot_bgcolor  = "transparent",
        paper_bgcolor = "transparent",
        font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default", family = "Arial", size = 12.5),
        scene = list(
                  xaxis = list(title = "Lat", range = c(max(as.numeric(dimnames(wave_up_sym)[[2]])), min(as.numeric(dimnames(wave_up_sym)[[2]])))),
                  yaxis = list(title = "Lon", range = c(min(as.numeric(dimnames(wave_up_sym)[[1]])), max(as.numeric(dimnames(wave_up_sym)[[1]])))),
                  zaxis = list(title = "SWH"),
                  aspectratio = list(x = 1, y = 1 , z = 1/4), 
                  aspectmode = "manual",
                  camera = list(eye = list(x = 1.2, y = -1.2, z = 0.65))
                )
      )%>%
      colorbar(title = "SWH in m")
  })
  
  # plot final symmetric modulated wave field 
  output$final_waveplot_surface <- renderPlotly({
    
    plot_ly() %>%
     add_surface(
       # defining the x and y axis as numeric dimension names of symmetric wave matrix 
       x = as.numeric(dimnames(modulated_wave_field)[[2]]),
       y = as.numeric(dimnames(modulated_wave_field)[[1]]),
       z = modulated_wave_field
      ) %>%
      layout(
        modebar = list(color = "lightgrey",
                       activecolor = "grey",
                       bgcolor = "transparent"),
        plot_bgcolor  = "transparent",
        paper_bgcolor = "transparent",
        font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default", family = "Arial", size = 12.5),
        scene = list(
                  xaxis = list(title = "Lat", range = c(max(as.numeric(dimnames(modulated_wave_field)[[2]])), min(as.numeric(dimnames(modulated_wave_field)[[2]])))),
                  yaxis = list(title = "Lon", range = c(min(as.numeric(dimnames(modulated_wave_field)[[1]])), max(as.numeric(dimnames(modulated_wave_field)[[1]])))),
                  zaxis = list(title = "SWH"),
                  aspectratio = list(x = 1, y = 1 , z = 1/4), 
                  aspectmode = "manual",
                  camera = list(eye = list(x = 1.2, y = -1.2, z = 0.65))
                )
      )%>%
      colorbar(title = "SWH in m")
  })
  
  
  # plot initial symmetric wave field to upscale
  output$modulation_wavefield_plot <- renderPlotly({
    
    plot_ly() %>%
     add_surface(
       # defining the x and y axis as numeric dimension names of landscape wave matrix 
       x = as.numeric(dimnames(landscape)[[2]]), 
       y = as.numeric(dimnames(landscape)[[1]]), 
       z = landscape
      ) %>%
      layout(
        modebar = list(color = "lightgrey",
                       activecolor = "grey",
                       bgcolor = "transparent"),
        plot_bgcolor  = "transparent",
        paper_bgcolor = "transparent",
        font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default", family = "Arial", size = 12.5),
        scene = list(
                  xaxis = list(title = "Lat"),
                  yaxis = list(title = "Lon"),
                  zaxis = list(title = "SWH"),
                  aspectratio = list(x = 1, y = 1 , z = 1/4), 
                  aspectmode = "manual",
                  camera = list(eye = list(x = 1.2, y = -1.2, z = 0.65))
                )
      )%>%
      colorbar(title = "SWH in m")
  })
  

# ---------- plot wave direction when api call is present ----------------------
  # creating a new data frame with all wave directions from the input angle vector
  sim_wind <- data.frame(degrees = waveParams$angle)  
  
  # creating empty data frame, with columns for the direction, count and percentage
  summary_sim_wind <- data.frame(
    Direction = c("E", "ENE", "NE", "NNE", "N", "NNW", "NW", "WNW", "W", "WSW", "SW", "SSW", "S", "SSE", "SE","ESE"),
    Max = nrow(sim_wind),
    Count = rep(0, 16),
    Percent = rep(0, 16)
    )

  # using wave direction function and counting wave directions in sim_wind for each row
  for (i in 1:nrow(sim_wind)) {
    sim_wind_direction <- degrees_to_direction(sim_wind$degrees[i])
    rof_index <- match(sim_wind_direction, summary_sim_wind$Direction)  
    summary_sim_wind$Count[rof_index] <- summary_sim_wind$Count[rof_index] + 1
  }

  # calc. percentage values of wave directions
  summary_sim_wind$Percent <- (summary_sim_wind$Count / sum(summary_sim_wind$Count))

  # create the radar chart
  output$simulated_wind_directions <-renderPlotly({
    plot_ly(summary_sim_wind, type = "scatterpolar", fill = "toself", mode = "lines+markers", hoverinfo = "r+theta") %>%                            
      add_trace(r = summary_sim_wind$Percent, theta = summary_sim_wind$Direction , name = "Direction in %",
                fillcolor = "rgba(48, 106, 142, 0.6)",
                line = list(color = "rgba(19, 86, 149, 0.8)"),
                marker = list(color = "rgba(198, 224, 42, 1)")) %>%
      layout(
             modebar = list(color = "lightgrey", activecolor = "grey", bgcolor = if (input$dark_mode == "dark") "transparent" else "transparent"),  
             polar = list(bgcolor = "transparent", radialaxis = list(tickformat = ".0%")),                                                          
             paper_bgcolor = "transparent",                                                                                                         
             font = list(color = if (input$dark_mode == "dark") "lightgrey" else "default", family = "Arial", size = 14.5))                                                        
  })
    
  }  
})

# ---------- plot example selection zones --------------------------------------
output$gg_map_example <- renderPlot({

# create buoy location dataframe
buoys <- data.frame(
  id = c("41008", "41049", "44011", "44014", ""),
  lat = c(31.4000000, 27.5047222, 41.0930556, 36.6027778, 32.4445035),
  lon = c(-80.8663889, -62.2705556, -66.5619444, -74.8369444, -66.7551315)
)

# test zone label
label_test <- data.frame(
  text = "Test in Domain",
  lat = 32.4445035,
  lon = -66.7551315
)

# create a citiy name/ location dataframe
cities <- data.frame(
  name = c("New York", "Miami", "Washington"),
  lat = c(40.7128, 25.7617, 38.9072),
  lon = c(-74.0060, -80.1918, -77.0369)
)

# atlantic-label
label_a <- data.frame(
  text = "Atlantic Ocean",
  lat = 31,
  lon = -72
)

# bermuda-label
label_b <- data.frame(
  text = "Bermuda Islands",
  lat = 32,
  lon = -63.2
)

# get the world map shape files
world <- map_data("world")

# create study area plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "gray80", color = "gray35") +
  # set buoy locations 
  geom_point(data = buoys, aes(x = lon, y = lat), 
             fill = "red", color = "black", shape = 21, size = 1, alpha = 1) +
  # set square 50km selection zones 
  geom_point(data = buoys, aes(x = lon, y = lat), 
             fill = "#d1495b", color = "#d6063a", shape = 22, size = 5.5, alpha = 0.6) +
  # add buoy names to locations
  geom_text(data = buoys, aes(x = lon, y = lat, label = id),
            hjust = -0.3, vjust = -0.2, size = 4.8, color = "#d40035", fontface = "bold") +
  # add text of test location
  geom_text(data = label_test, aes(x = lon, y = lat, label = text),
            hjust = 0.6, vjust = -2, size = 4, color = "#d40035", fontface = "bold") +
  # add city locations 
  geom_point(data = cities, aes(x = lon, y = lat), 
             fill = "#3c91c2", color = "#044f7a", shape = 21, size = 2, alpha = 0.8, stroke = 1) +
  # add city names 
  geom_text(data = cities, aes(x = lon, y = lat, label = name),
            hjust = 1, vjust = -1, size = 4, color = "#044f7a", fontface = "bold.italic") +
  # add atlantic oacean label 
  geom_text(data = label_a, aes(x = lon, y = lat, label = text),
            size = 5, fontface = "italic", color = "#044f7a", alpha = 0.6) +
   # add bermuda island -label
  geom_text(data = label_b, aes(x = lon, y = lat, label = text),
            size = 3, fontface = "bold.italic", color = "#044f7a", alpha = 0.6) +
  # set the aspectration and boundaries for the map
  coord_fixed(ratio = 0.8, xlim = c(-85, -61), ylim = c(25, 45)) +
  # adjust axis titles
  labs(title = "", x = "Longitude", y = "Latitude") +
  # add theme minimal for clean look
  theme_minimal() +
  # adjust text heights and appearance   
  theme(
    axis.text = element_text(color = "gray30", size = 11, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold", color = "gray20"),
    axis.title.y = element_text(size = 12, face = "bold", color = "gray20"),
    plot.margin = grid::unit(c(1,1,1,1), "mm")
  )
})


# ---------- plot example superposition of sine waves --------------------------
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


# ---------- plot example modulation value k -----------------------------------
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