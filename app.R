##features: check box alcohol types, all option for countries

library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

bcl <- read.csv("https://gist.githubusercontent.com/daattali/720431961c6c5394ae96/raw/10222380b7d3c9c43d22cc67e6afe84fef98ab9a/bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  # first I added a theme: not particularly useful, just pretty :)
  theme = shinytheme("superhero"),
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      #I also changed the radio buttons to check boxes, since people may be interested in more than one type of alcohol
      checkboxGroupInput("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                c("ALL", sort(unique(bcl$Country))),
                selected = "CANADA")
  })

  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
# finally, I added an "all" option to the country drop down, since people may care
    #less about the country and more about the price and type of booze
    if (input$countryInput == "ALL"){
      bcl %>%
        filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput)
    } else {
     bcl %>%
        filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput)
    }
    })



  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })

  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)
