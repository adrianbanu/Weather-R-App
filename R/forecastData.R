updateForecast <- function(output, forecast_temp, lat, lon){
    
    # Three hours
    output$prognozaOre1_ora <- renderText({
        timp <- as.POSIXct(forecast_temp()$list$dt[[1]], origin="1970-01-01")
        timeZone <- tz_lookup_coords(lat, lon)
        format(timp, "%H:%M", tz = timeZone)
    })
    
    output$prognozaOre1_temp <- renderText({
        paste0(round(forecast_temp()$list$main.temp[[1]], 0), " °C")
    })
    
    output$WeatherDesc_1 <- renderText({
        forecast_temp()$list$weather[[1]]$main
    })
    
    output$prognozaOre1_precipitation <- renderText({
        paste0(forecast_temp()$list$pop[[1]] * 100, "%")
    })
    
    # Six hours
    output$prognozaOre2_ora <- renderText({
        timp <- as.POSIXct(forecast_temp()$list$dt[[2]], origin="1970-01-01")
        timeZone <- tz_lookup_coords(lat, lon)
        format(timp, "%H:%M", tz = timeZone)
    })
    
    output$prognozaOre2_temp <- renderText({
        paste0(round(forecast_temp()$list$main.temp[[2]], 0), " °C")
    })
    
    output$WeatherDesc_2 <- renderText({
        forecast_temp()$list$weather[[2]]$main
    })
    
    output$prognozaOre2_precipitation <- renderText({
        paste0(forecast_temp()$list$pop[[2]] * 100, "%")
    })
    
    # Nine hours
    output$prognozaOre3_ora <- renderText({
        timp <- as.POSIXct(forecast_temp()$list$dt[[3]], origin="1970-01-01")
        timeZone <- tz_lookup_coords(lat, lon)
        format(timp, "%H:%M", tz = timeZone)
    })
    
    output$prognozaOre3_temp <- renderText({
        paste0(round(forecast_temp()$list$main.temp[[3]], 0), " °C")
    })
    
    output$WeatherDesc_3 <- renderText({
        forecast_temp()$list$weather[[3]]$main
    })
    
    output$prognozaOre3_precipitation <- renderText({
        paste0(forecast_temp()$list$pop[[3]] * 100, "%")
    })
    
    # 12 hours
    output$prognozaOre4_ora <- renderText({
        timp <- as.POSIXct(forecast_temp()$list$dt[[4]], origin="1970-01-01")
        timeZone <- tz_lookup_coords(lat, lon)
        format(timp, "%H:%M", tz = timeZone)
    })
    
    output$prognozaOre4_temp <- renderText({
        paste0(round(forecast_temp()$list$main.temp[[4]], 0), " °C")
    })
    
    output$WeatherDesc_4 <- renderText({
        forecast_temp()$list$weather[[4]]$main
    })
    
    output$prognozaOre4_precipitation <- renderText({
        paste0(forecast_temp()$list$pop[[4]] * 100 , "%")
    })
    
    # Weather icons
    output$weather_icon1 <- renderUI({
        img(src = paste0("poze/", forecast_temp()$list$weather[[1]]$icon, ".png"))
    })
    
    output$weather_icon2 <- renderUI({
        img(src = paste0("poze/", forecast_temp()$list$weather[[2]]$icon, ".png"))
    })
    
    output$weather_icon3 <- renderUI({
        img(src = paste0("poze/", forecast_temp()$list$weather[[3]]$icon, ".png"))
    })
    
    output$weather_icon4 <- renderUI({
        img(src = paste0("poze/", forecast_temp()$list$weather[[4]]$icon, ".png"))
    })
}
