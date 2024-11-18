library(shiny)

source("koding.r") # Get lasso_fit

ui <- pageWithSidebar(
  headerPanel("Tittel??"),
  sidebarPanel(
                numericInput("Age", label = "Alder", value = 10),
                selectInput("Pclass", label = "Klasse", c("1", "2", "3")),
                selectInput("Sex", label = "Kjønn", c("male", "female")),
                sliderInput("Fare", label = "Bilettpris", min = 1, max = 500, value = 15),
                selectInput("Embarked", label = "Gikk om bord", 
                            c("S", "C", "Q")),
                numericInput("family_size", label = "Familie", value = 1),
                actionButton("submit", "Bekreft", class = "btn btn-primary")
                
  ),
  mainPanel(
    textOutput("prediction_output"),
    verbatimTextOutput("prediction_details")
  )
)


server <- function(input, output, session) {
  
  datasetInput <- eventReactive(input$submit, {
  
    ny_tibble <- tibble(
      Pclass = as.factor(input$Pclass),
      Sex = input$Sex,
      Age = input$Age,
      Fare = input$Fare,
      Embarked = as.factor(input$Embarked),
      family_size = input$family_size,
      alone = as.factor(ifelse(input$family_size == 1, 1, 0)))
    
    output <- predict(lasso_fit, ny_tibble, type = "prob")
  })
  output$prediction_output <- renderText({
    prob <- datasetInput()
    if (!is.null(prob)) {
      paste0("Probability of Survival: ", round(prob$.pred_1 * 100, 2), "%")
    } else {
      "Waiting for input..."
    }
  })
  
  output$prediction_details <- renderPrint({
    prediction <- datasetInput()
    if (!is.null(prediction)) {
      print(prediction)
    }
  })

}

shinyApp(ui, server)

