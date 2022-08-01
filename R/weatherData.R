updateWeatherData <- function(output, data, cityName){
    
    # City name and info
    output$CityName <- renderText({
        timeZone <- tz_lookup_coords(data()$coord$lat, data()$coord$lon)
        LocalTime <- paste0(format(Sys.time(), "%H:%M", tz = timeZone))
        paste0(cityName, ", ", data()$sys$country,  ", local time: ", LocalTime)
    })
    
    # weather description
    output$mainWeatherDesc <- renderText({
        data()$weather$description[[1]]
    })
    
    # City temperature
    output$CityTemp <- renderText({

        # icon for extreme values
        if(round(data()$main$temp, 0) > 35){
            runjs("
                document.getElementById('termometru_red').style.display = 'block';
                document.getElementById('termometru_blue').style.display = 'none';
            ")    
        }else if(round(data()$main$temp, 0) < -5){
            runjs("
                document.getElementById('termometru_blue').style.display = 'block';
                document.getElementById('termometru_red').style.display = 'none';
            ") 
        }else{
            runjs("
                document.getElementById('termometru_red').style.display = 'none';
                document.getElementById('termometru_blue').style.display = 'none';
            ") 
        }
        
        paste0(round(data()$main$temp, 0), '°', "C")
    })
    
    # feelsLike
    output$feelsLike <- renderText({
        paste0("Feels like ", round(data()$main$feels_like, 0), '°', "C")
    })
    
    
    output$weather_icon <- renderUI({
        img(src = paste0("poze/", data()$weather$icon[[1]], ".png"))
    })
    
    ## Other info
    
    # pressure
    output$pressure <- renderText({
        paste0("Pressure: ", data()$main$pressure, " hPa")
    })
    
    # humidity
    output$humidity <- renderText({
        paste0("Humidity: ", data()$main$humidity, "%")
    })
    
    # wind speed
    output$windSpeed <- renderText({
        paste0("Wind speed: ", data()$wind$speed, " m/s")
    })
    
    # visibility
    output$visibility <- renderText({
        paste0("Visibility: ", data()$visibility / 1000, " km")
    })
    
    # local Hour
    output$localHour <- renderText({
        timeZone <- tz_lookup_coords(data()$coord$lat, data()$coord$lon)
        paste0("Ora locala: ", format(Sys.time(), "%H:%M", tz = timeZone))
    })
    
    # sunrise
    output$sunrise <- renderText({
        sunrise <- as.POSIXct(data()$sys$sunrise, origin="1970-01-01")
        timeZone <- tz_lookup_coords(data()$coord$lat, data()$coord$lon)
        paste0("Sunrise: ", format(sunrise, "%H:%M", tz = timeZone))
    })
    
    # sunset
    output$sunset <- renderText({
        sunset <- as.POSIXct(data()$sys$sunset, origin="1970-01-01")
        timeZone <- tz_lookup_coords(data()$coord$lat, data()$coord$lon)
        paste0("Sunset: ", format(sunset, "%H:%M", tz = timeZone))
    })
}
