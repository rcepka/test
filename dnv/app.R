#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins

  fluidRow(
    column(12,

           tabsetPanel(

             tabPanel("Tab 1",
                      wellPanel(
                        sliderInput("bins",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 30),
                        tags$p(textOutput("distPlot_text")),
                        plotOutput("distPlot")

                      )
             ),

             tabPanel("Tab 2",

                        sliderInput("bins2",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 30),
                        plotOutput("distPlot2")

             )
           )
    ),


    ),


  fluidRow(

    column(4,
           selectInput("var",
                       label = "Choose a variable to display",
                       choices = c("Percent White",
                                   "Percent Black",
                                   "Percent Hispanic",
                                   "Percent Asian"),
                       selected = "Percent White"),
           ),
    column(8,
           "just text",
           textOutput("selected_text")
           )

    ),

  fluidRow(
    column(4,
           tableOutput("table")
           ),
    column(8,
          # tableOutput("table")
          plotOutput("myplot")
          )

  ),




)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  output$distPlot2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins2 <- seq(min(x), max(x), length.out = input$bins2 + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins2, col = 'darkgray', border = 'white')
  })

  output$selected_text <- renderText({
    paste("You have selected this:", input$var)
  })

  output$distPlot_text <- renderText({
    paste("You have selected this:", input$bins)
  })

  output$myplot <- renderPlot({
    ggplot(data = dnv_demand_L_FinEnDem_by_carrier) +
      aes(x = Year, y = Value, color = EnergyCarrier) +
      geom_point()
  })

  output$table <- renderTable(dnv_demand_L_FinEnDem_by_carrier,
                              pageLength = 5,
                              width = "100%")


}

# Run the application
shinyApp(ui = ui, server = server)
