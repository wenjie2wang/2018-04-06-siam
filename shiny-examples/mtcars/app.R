library(shiny)

ui <- fluidPage(
    titlePanel("a simple example of reactive output"),
    sidebarLayout(
        sidebarPanel(
            selectInput("cyl", "Cylinders",
                        choices = sort(unique(mtcars$cyl)),
                        selected = 4)
        ),
        mainPanel(plotOutput("mpg_boxplot"))
    )
)

server <- function(input, output) {
    ## the `renderPlot` runs every time a user changes input$cyl
    output$mpg_boxplot <- renderPlot({
        ## filtering the cars models in the mtcars dataset
        ## by a input cylinder number
        dat <- subset(mtcars, cyl == input$cyl)
        ## draw a boxplot of mpg for the filtered data
        with(dat, boxplot(mpg))
    })
}

shinyApp(ui, server)
