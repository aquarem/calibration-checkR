# testing

library(shiny)
library(plotly)
library(rhandsontable)

source(here('testing.R'))
source(here("functionized.R"))


ui <- fluidPage(
    fluidRow(
        
        rHandsontableOutput("calSettings"),
        actionButton("calcCal", "Calc Cal")
        
    ),
    
    fluidRow(plotlyOutput("testPlotly"))
)

server <- function(input, output) {
    # pretending person selected correct tabs
    
    level <- here('sample.xlsx') |> 
        readxl::read_excel(sheet = "standards")
    
    response <- here('sample.xlsx') |> 
        readxl::read_excel('areas')
    # testing behavior of missing bromide lowest level
    cal_data <- level |> 
        pivot_longer(!1) |> 
        mutate(type = "level") |> 
        bind_rows(
            response |> 
                pivot_longer(!1) |> 
                mutate(type = "response")
        ) |> 
        { \(x) rename(
            x, 
            standard = names(x)[1],
            constituent = name
        ) }() |> 
        pivot_wider(
            names_from = type,
            values_from = value
        )
    
    
    # default settings
    output$calSettings <- tibble(
        constituent = c("fluoride", "chloride", "bromide"),
        calibration = c("linear", "avgRF", "quadratic") |> 
            factor(levels = c(
                "linear", "avgRF", "quadratic"
            )),
        yIntercept = c(TRUE, FALSE, TRUE),
        weight = c("1/x", "none", "1/x") |> 
            factor(levels = c(
                "none", "1/x", "1/x^2"
            ))
    ) |> 
        rhandsontable(height = 120) |> 
        renderRHandsontable()
    
    cal_reg <- reactiveVal()
    
    observeEvent(input$calcCal, {
        make_cal(
            cal_data,
            input$calSettings |> hot_to_r()
        ) |> 
            cal_reg()
    })
    
    
    # is this reactive by default? idk
    output$testPlotly <- renderPlotly({
        cal_reg() |> 
            plot_cal() |> 
            ggplotly() |> 
            style(textposition = "bottom right") # from plotly.js
    })
    
    
    
    
    
}

shinyApp(ui = ui, server = server)
