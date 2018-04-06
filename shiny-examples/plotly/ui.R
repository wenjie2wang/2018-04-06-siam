## examples taken from https://plot.ly/r/shiny-coupled-hover-events/
## with some slight modifications

library(shiny)
library(plotly)
## library(shinythemes)
library(dplyr)

ui <- fluidPage(
    ## Set theme
    ## theme = shinytheme("spacelab"),

    ## Some help text
    ## h2("Coupled hover-events in plotly charts using Shiny"),
    ## h4("This Shiny app showcases coupled hover-events using Plotly's ",
    ##    tags$code("event_data()"), " function."),

    ## Vertical space
    ## tags$hr(),

    ## Window length selector
    selectInput("window", label = "Select Window Length",
                choices = c(10, 20, 30, 60, 90), selected = 10),
    ## Plotly Chart Area
    fluidRow(
        column(6, plotlyOutput(outputId = "timeseries")),
        column(6, plotlyOutput(outputId = "correlation"))
    )

    ## tags$hr(),
    ## tags$blockquote(
    ##          paste("Hover over time series chart to fix a specific date.",
    ##                "Correlation chart will update with historical correlations",
    ##                "(time span will be hover date +/- selected window length)"
    ##                )
    ##      )
)
