updatePolution <- function(output, cityPollution){
    output$poluare_index <- renderText({
        air_quality <- switch(
            cityPollution()$list$main.aqi,
            "Good",
            "Fair",
            "Moderate",
            "Poor",
            "Very poor"
        )
        paste0("Air quality: ", air_quality, "*")
    })
    
    output$poluare_PM25 <- renderText({
        paste0("PM25: ", cityPollution()$list$components.pm2_5, " µ/m3")
    })
    
    output$poluare_PM10 <- renderText({
        paste0("PM10: ", cityPollution()$list$components.pm10, " µ/m3")
    })
    
    output$poluare_CO <- renderText({
        paste0("CO: ", cityPollution()$list$components.co, " µ/m3")
    })
    
    output$poluare_NO2 <- renderText({
        paste0("NO2: ", cityPollution()$list$components.no2, " µ/m3")
    })
    
    output$poluare_O3 <- renderText({
        paste0("O3: ", cityPollution()$list$components.o3, " µ/m3")
    })
    
    output$poluare_SO2 <- renderText({
        paste0("SO2: ", cityPollution()$list$components.so2, " µ/m3")
    })
}

pollutionDetails <- function(){
    runjs(
        "
            var panel = document.getElementById('pollutionInfo');
            if (panel.style.maxHeight) {
              panel.style.maxHeight = null;
            } else {
              panel.style.maxHeight = panel.scrollHeight + 'px';
            } 
        "
    )
}
