library(shiny)
library(shiny.semantic)
library(httr)
library(jsonlite)
library(lutz)
library(shinyjs)

ui <- semanticPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$link(rel = "shortcut icon", href = "icon.png", type = "image/x-icon")
    ),
    useShinyjs(),
    title = "WeatheR app",
    
    div(class="ui grid mt10 items-flex",
        div(class="",
            div(class="app-name",
              span("Weather"), span(class="letter-R", "R"), span( "app"),  
            ),
        ),
        div(class="",
            form(
                div(class="ui grid centered column mt10",
                    textInput("oras", label="", type = "text", placeholder = "Search city"),
                    actionButton("button_cautare", label= "", icon("search icon"))
                )
            ),
        ),
    ),
    
    div(class="ui two column grid stackable", id = "app-content",
        div(class="ten wide column",
            div(class="ui raised segment weather-info temp-info",
                div(
                    div(
                        div(class = "ui block ", 
                           textOutput("CityName", ),
                        ),
                        div(class="flex-center flex-space", 
                            div(class="flex-center",
                                tags$i(class="temperature high blue icon", id = "termometru_blue"), 
                                tags$i(class="temperature high red icon", id = "termometru_red"),
                                textOutput("CityTemp"),
                            ),
                            uiOutput("weather_icon")
                        ),

                        div(
                            textOutput("mainWeatherDesc"),
                            textOutput("feelsLike"),
                        ),
                    ),
                ),
            ),
            
            div(class="ui raised segment weather-info ",
                div(
                    class="ui two column divided grid ",
                    div(class="row",
                        div(
                            class="column flex-center flex-start",
                            tags$i(class="angle double down icon"),
                            textOutput("pressure"),
                        ),
                        div(
                            class="column flex-center flex-start ",
                            tags$i(class="tint icon"),
                            textOutput("humidity"),
                        ),
                    ),
                    div(class="row",
                        div(
                            class="column flex-center flex-start",
                            tags$i(class="wind icon"),
                            textOutput("windSpeed"),
                        ),
                        div(
                            class="column flex-center flex-start ", 
                            tags$i(class="eye icon"),
                            textOutput("visibility"),
                        ),
                    ),
                    div(class="row",
                        div(
                            class="column flex-center flex-start ",
                            tags$i(class="coffee icon"),
                            textOutput("sunrise"),
                        ),
                        div(
                            class="column flex-center flex-start ", 
                            tags$i(class="moon outline icon"),
                            textOutput("sunset"),
                        ),
                    ),
                ),
            ),
            
            # prognoza ore
            div(class="ui raised segment weather-info",
                h3(
                    "Next hours"
                ),
                div(class="ui grid divided ",
                    # next 3 hours
                    div(class="ui four wide column center aligned ",
                        textOutput("prognozaOre1_ora"),
                        div(class = "flex-center prognoza-temp",
                            textOutput("prognozaOre1_temp"), 
                            uiOutput("weather-icon1"),
                        ),
                        textOutput("WeatherDesc_1",),
                        div(class = "flex-center",
                            tags$i(class="umbrella icon"),
                            textOutput("prognozaOre1_precipitation"), 
                        ),
                    ),
                    # next 6 hours
                    div(class="ui four wide column center aligned despartitor",
                        textOutput("prognozaOre2_ora"), 
                        div(class = "flex-center prognoza-temp",
                            textOutput("prognozaOre2_temp"),
                            uiOutput("weather-icon2"),
                        ),
                        textOutput("WeatherDesc_2",),
                        div(class = "flex-center",
                            tags$i(class="umbrella icon"),
                            textOutput("prognozaOre2_precipitation"), 
                        ),
                    ),
                    # next 9 hours
                    div(class="ui four wide column center aligned despartitor",
                        textOutput("prognozaOre3_ora"),
                        div(class = "flex-center prognoza-temp",
                            textOutput("prognozaOre3_temp"), 
                            uiOutput("weather-icon3"),
                        ),
                        textOutput("WeatherDesc_3",),
                        div(class = "flex-center",
                            tags$i(class="umbrella icon"),
                            textOutput("prognozaOre3_precipitation"), 
                        ),
                    ),
                    # next 12 hours
                    div(class="ui four wide column center aligned despartitor",
                        textOutput("prognozaOre4_ora"), 
                        div(class = "flex-center prognoza-temp",
                            textOutput("prognozaOre4_temp"), 
                            uiOutput("weather-icon4"),
                        ),
                        textOutput("WeatherDesc_4",),
                        div(class = "flex-center",
                            tags$i(class="umbrella icon"),
                            textOutput("prognozaOre4_precipitation"), 
                        ),
                    ),
                )    
            ),
        ),
        
        # Pollution
        div(class="six wide column ",
            div(class = "ui raised segment weather-info",
                textOutput("poluare_index"),
                div(
                    span(class="ui small text", "*Based on Common Air Quality Index (or CAQI)"),
                ),
                action_button ("buton_acordeon", "See more info", icon("angle down icon"), class="tertiary"),
                div(
                    id="pollutionInfo",
                    div(class = "ui two column divided grid row", 
                        div(
                            class = "column pb10",
                            textOutput("poluare_PM25"),
                            textOutput("poluare_PM10"),
                            textOutput("poluare_CO"),
                        ),
                        div(
                            class = "column pb10",
                            textOutput("poluare_NO2"),
                            textOutput("poluare_O3"),
                            textOutput("poluare_SO2"),
                        ),
                    ),
                )
            ),
        ),
    )
)

server <- function(input, output) {
    
    # Start city
    LocationR <- reactiveValues(City = "Bucharest")
    
    # Reactive as intermediate values
    City_Name <- reactiveValues(CityName = NULL)
    LastCorrectCity <- reactiveValues(City = "Bucharest")
    
    # reactiv meteo 
    data <- reactive({
        base <- "https://api.openweathermap.org/data/2.5/weather?q="
        info <- "&units=metric&appid=XXX&lang=en"
        API_URL <- paste0(base, LocationR$City, info)
        raw_data_meteo <- httr::GET(API_URL)
        City_Name$CityName <- content(raw_data_meteo)$name
        readable_data <- fromJSON(rawToChar(raw_data_meteo$content), flatten = TRUE)
    })
    
    # reactive forecast
    forecast_temp <- reactive({
        base <- "https://api.openweathermap.org/data/2.5/forecast?lat="
        longitude <- data()$coord$lon
        latitude <- data()$coord$lat
        info <- "&units=metric&appid=XXX"
        link_final <- paste0(base, latitude, "&lon=", longitude, info)
        raw_data_forecast <- httr::GET(link_final)
        readable_data <- fromJSON(rawToChar(raw_data_forecast$content), flatten = TRUE)
    })

    # reactive pollution
    poluare <- reactive({
        base <- "http://api.openweathermap.org/data/2.5/air_pollution?lat="
        longitude <- data()$coord$lon
        latitude <- data()$coord$lat
        info <- "&appid=XXX"
        link_final <- paste0(base, latitude, "&lon=", longitude, info)
        raw_data_forecast <- httr::GET(link_final)
        readable_data <- fromJSON(rawToChar(raw_data_forecast$content), flatten = TRUE)
    })

    observeEvent(input$button_cautare, {
        base <- "https://api.openweathermap.org/data/2.5/weather?q="
        info <- "&units=metric&appid=XXX&lang=en"
        API_URL <- paste0(base, input$oras, info)
        raw_data_meteo <- httr::GET(API_URL)
        City_Name$CityName <- content(raw_data_meteo)$name
        readable_data <- fromJSON(rawToChar(raw_data_meteo$content), flatten = TRUE)
        if (readable_data$cod == 200){
            LocationR$City <- input$oras
            LastCorrectCity$City <- City_Name$CityName
            updateAllData(City_Name$CityName)
        }else if(readable_data$cod == 400){
            if (is.null(LocationR$City)){
                LocationR$City <- City_Name$CityName
                updateAllData(City_Name$CityName)
            } else{
                LocationR$City = LastCorrectCity$City
                updateAllData(LastCorrectCity$City)
            }
        }else{
            if (is.null(LocationR$City)){
                LocationR$City <- City_Name$CityName
                updateAllData(City_Name$CityName)
            }else{
                LocationR$City = LastCorrectCity$City
                updateAllData(LastCorrectCity$City)
            }
            showNotification("City not found!", type = "error")
        }
    })
    
    # Pollution details
    observeEvent(input$buton_acordeon, {
        pollutionDetails()
    })

    # Updates all data
    updateAllData <- function(CityName){
        updateWeatherData(output, data, CityName)
        updatePolution(output, poluare)
        updateForecast(output, forecast_temp, data()$coord$lat, data()$coord$lon)
    }
    
    # Populate data for starting city
    updateAllData(City_Name$CityName)
}

# Run the application 
shinyApp(ui = ui, server = server)
