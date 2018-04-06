## examples taken from https://plot.ly/r/shiny-coupled-hover-events/
## with some slight modifications

server <- function(input, output) {

    ## Read data
    stockdata <- read.csv(
        "https://cdn.rawgit.com/plotly/datasets/master/stockdata.csv"
    )

    ## Create dates
    stockdata$Date <- as.Date(stockdata$Date)

    ## Reshape
    ds <- reshape2::melt(stockdata, id = "Date")
    ds <- filter(ds, variable != "GSPC")

    ## Set some colors
    plotcolor <- "#F5F1DA"
    papercolor <- "#E3DFC8"

    ## Plot time series chart
    output$timeseries <- renderPlotly({
        p <- plot_ly(source = "source", height = 450) %>%
            add_lines(data = ds, x = ~Date, y = ~value, color = ~variable,
                      mode = "lines", line = list(width = 3))

        ## Add SP500
        p <- p %>%
            add_lines(data = stockdata, x = ~Date, y = ~GSPC, mode = "lines",
                      yaxis = "y2", name = "SP500", opacity = 0.3,
                      line = list(width = 5)) %>%
            layout(
                title = "Stock prices for different stocks overlaid with SP500",
                xaxis = list(title = "Dates", gridcolor = "#bfbfbf",
                             domain = c(0, 0.98)),
                yaxis = list(title = "Stock Price", gridcolor = "#bfbfbf"),
                plot_bgcolor = plotcolor,
                paper_bgcolor = papercolor,
                yaxis2 = list(title = "SP500", side = "right",
                              overlaying = "y"),
                legend = list(orientation = 'v', x = 0, y = 1.0),
                margin = list(r = 50)
                )
        p$elementId <- NULL
        config(p, collaborate = FALSE, displaylogo = FALSE)
    })

    ## Coupled hover event
    output$correlation <- renderPlotly({

        ## Read in hover data
        eventdata <- event_data("plotly_hover", source = "source")
        validate(
            need(!is.null(eventdata),
                 "Hover over the time series chart to populate this heatmap"
                 ))

        ## Get point number
        datapoint <- as.numeric(eventdata$pointNumber)[1]

        ## Get window length
        window <- as.numeric(input$window)

        ## Show correlation heatmap
        rng <- (datapoint - window):(datapoint + window)
        cormat <- round(cor(stockdata[rng, 1:5]),2)

        ply <- plot_ly(x = rownames(cormat), y = colnames(cormat),
                       z = cormat, type = "heatmap",
                       colors = colorRamp(c('#e3dfc8', '#808c6c')),
                       height = 450) %>%
            layout(title = "Correlation heatmap",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
        ply$elementId <- NULL
        config(ply, collaborate = FALSE, displaylogo = FALSE)
    })

}
