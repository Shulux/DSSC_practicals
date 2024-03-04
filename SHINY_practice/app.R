#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library("shiny")
library("ggplot2")

ui <- fluidPage(

  sidebarLayout(

    sidebarPanel(

      textInput("name", "What's your name?"),

      selectInput("xvar", "What is the x variable?", names(iris), selected = names(iris)[2]),

      selectInput("yvar", "What is the y variable?", names(iris), selected = names(iris)[1]),

      ),

    mainPanel(

      textOutput("greet"),

      plotOutput("myplot", width = "400px")

      )
  )
)

server <- function(input, output, session) {

  name <- reactive({
    toupper(input$name)
  })

  output$greet <- renderText({

    if(nchar(input$name) > 0) {
      return(paste0("Hello ", name(), ", here is your plot ..."))
    } else {
      return("Hello friend, tell me your name!")
    }
  })

  output$myplot <- renderPlot({

    if(nchar(input$name) > 0) {
      ggplot(iris, aes_string(x = input$xvar, y = input$yvar)) +
        geom_point() +
        labs(title = paste0(name(), "'s plot!"))
    }
  }, res = 96)
}

shinyApp(ui, server)
