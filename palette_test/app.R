#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#remotes::install_github("nowosad/colorblindcheck")

library(shiny)
#library(plotly)
library(tidyverse)
library(colourpicker)
library(colorblindcheck)

source("palettes.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bivariate Palette Explorer"),

    # Sidebar with colour pickers and slider for number of steps
    sidebarLayout(
        sidebarPanel(
            colourpicker::colourInput(inputId = "var12min",
                                      label = "Minimum",
                                      value = "#f3f3f3"),
            colourpicker::colourInput("var1max",
                             "Variable 1 Max",
                             "#509dc2"),
            colourpicker::colourInput("var2max",
                             "Variable 2 Max",
                             "#f3b300"),
            colourpicker::colourInput("var12max",
                             "Variable 1 & 2 Max",
                             "#000000"),

            sliderInput("bins",
                        "Number of Steps:",
                        min = 2,
                        max = 5,
                        value = 3),
            shiny::actionButton("evolve_one",
                                "Evolve One Generation"),
            shiny::actionButton("evolve_ten",
                                "Evolve Ten Generations")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                # shiny::column(6, 
                #               plotOutput("palette_plot")
                # ),
                shiny::column(12,
                              shiny::plotOutput("colourblind_check_plot")
                ),
                
                #plotly::plotlyOutput("palette_plot_ly"),        
                #  plotOutput("palette_plot_gg"),
                #  shiny::textOutput("text_palette"),
            ),
            
          fluidRow(
            DT::dataTableOutput("colourblind_check_table",
                                width = "90%")
          )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    p <- reactive({new_bivariate_palette(var12min = input$var12min,#input$var12min,#input$var12min,
                               var1max = input$var1max,
                               var2max = input$var2max,
                               var12max = input$var12max,
                               num_steps = input$bins)})
    
    output$text_palette <- renderText(p())
    
    output$palette_plot_gg <- renderPlot({
        ggplot_palette(p())
    })

    
    output$palette_plot <- renderPlot({
        plot_palette(p())
        
    })
    
    output$palette_plot_ly <- plotly::renderPlotly({
        ggplot_palette(p(), use_plotly = TRUE)
    })
    
    output$colourblind_check_plot <- renderPlot({
        p() %>%
        #matrix(nrow = input$bins) %>% t() %>% c() %>%
        colorblindcheck::palette_bivariate_plot()
    })
    
    output$colourblind_check_table <- DT::renderDataTable({
        colorblindcheck::palette_check(p(), bivariate = TRUE) %>%
            tibble::as_tibble() %>%
            dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 1)) %>%
            select(name, ncp, ndcp, min_dist, mean_dist, max_dist) %>%
            DT::datatable(options = list(dom = 't'))
    })
    
    
    observeEvent(input$evolve_one, {
        do_evolution(num_generations = 1)
    })
    
    observeEvent(input$evolve_ten, {
        do_evolution(num_generations = 10)
    })
    
    do_evolution <- function(num_generations){
        new_palette <- palette_evolution(c(var12min = input$var12min,#input$var12min,#input$var12min,
                                           var1max = input$var1max,
                                           var2max = input$var2max,
                                           var12max = input$var12max),
                                         
                                         num_steps = input$bins,
                                         num_children = 10,
                                         num_generations = num_generations,
                                         verbose = TRUE)
        
        message(new_palette)
        
        
        colourpicker::updateColourInput(session,
                                        inputId = "var12min",
                                        value = new_palette[[1]])
        
        colourpicker::updateColourInput(session,
                                        inputId =  "var1max",
                                        value = new_palette[[2]])
        
        colourpicker::updateColourInput(session,
                                        inputId =  "var2max",
                                        value = new_palette[[3]])
        
        colourpicker::updateColourInput(session,
                                        inputId =  "var12max",
                                        value = new_palette[[4]])
        
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
