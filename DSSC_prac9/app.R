#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

nbd <- ukc_neighbourhoods("durham")
nbd2 <- nbd$id
names(nbd2) <- nbd$name

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DSSC Practical 9"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("date",
                      label = "Year:Month",
                      value = "2023-09"),
            selectInput("nbd",
                        label = "Neighborhood",
                        choices = nbd2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("plot"),
          leafletOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  nbh.bd <- reactive({

    durham.city.boundary <- ukc_neighbourhood_boundary("durham", input$nbd)

    durham.city.boundary |>
      mutate(latitude = as.numeric(latitude),
             longitude = as.numeric(longitude))
  })

  nbh.crimes <- reactive({
    bdy2 <- nbh.bd() |>
      select(lat = latitude,
             lng = longitude)

    ukc_crime_poly(bdy2[round(seq(1, nrow(bdy2), length.out = 100)), ], input$date)
  })

  output$plot <- renderPlot(
    ggplot(nbh.crimes()) +
      geom_bar(aes(y = category, fill = outcome_status_category)) +
      labs(y = "Crime", fill = "Outcome Status")
  )

  output$map <- renderLeaflet(
    leaflet() |>
      addTiles() |>
      addPolygons(lng = nbh.bd()$longitude,
                  lat = nbh.bd()$latitude) |>
      addCircles(lng = as.numeric(nbh.crimes()$longitude),
                 lat = as.numeric(nbh.crimes()$latitude),
                 label = nbh.crimes()$category,
                 col = "red")
  )
}

# Run the application
shinyApp(ui = ui, server = server)
