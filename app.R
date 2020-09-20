library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)
print(View(bcl))

ui <- fluidPage(
  setBackgroundColor(
    color = c("#2E8B57", "#90EE90"),
    gradient = "radial",
    direction = c("top", "left")
  ),
  titlePanel(strong(em("Liquor Store prices"))),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
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
                sort(unique(bcl$Country)),
                selected = "GERMANY")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(x=Alcohol_Content,fill=Alcohol_Content)) +
      geom_bar(width=0.1)+
    geom_text(
      aes(label=stat(count)),
      stat='count',
      colour="red",
      nudge_y=0.07,
      va='bottom',
      size=5
    )
  })
  
  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)