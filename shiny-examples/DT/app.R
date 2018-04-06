library(shiny)
library(DT)

ui <- fluidPage(

    titlePanel("a simple Shiny app with DT"),

    fluidRow(
        column(width = 4,
               selectInput("cyl", "Cylinders",
                           choices = sort(unique(mtcars$cyl)),
                           selected = 4)
               )
    ),

    fluidRow(
        column(width = 12,
               dataTableOutput("dt")
               )
    )
)

server <- function(input, output) {

    dataInput <- reactive({
        subset(mtcars, cyl == input$cyl, select = - cyl)
    })

    output$dt <- renderDataTable({
        dataInput()
    }, options = list(
           lengthMenu = list(
               c(6, 15, 30),
               c('6', '15', '30')
           ))
    )

}

shinyApp(ui, server)
