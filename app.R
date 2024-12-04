# testing

library(shiny)
library(plotly)

source(here('testing.R'))

ui <- fluidPage(
    fluidRow(plotlyOutput("testPlotly"))
)

server <- function(input, output) {
    output$testPlotly <- renderPlotly({
        ggplotly(cal_plots) |> 
            style(textposition = "bottom right") # from plotly.js
    })
}

shinyApp(ui = ui, server = server)
