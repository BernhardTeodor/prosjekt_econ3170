library(shiny)

source("app_data.r") # henter modell

ui <- pageWithSidebar(
  headerPanel("Titanic overlevelseskalkulator"),
  sidebarPanel(
                numericInput("Age", label = "Alder", value = 10),
                selectInput("Pclass", label = "Klasse", c("1", "2", "3")),
                selectInput("Sex", label = "Kjønn", c("male", "female")),
                selectInput("Fare", label = "Bilettpris", c("Lav", "Middels", "Høy")),
                #sliderInput("Fare", label = "Bilettpris", min = 1, max = 500, value = 15),
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
      
      Sex = as.factor(input$Sex),
      
      Age = input$Age,
      
      #Fare = input$Fare,
      Fare = case_when(
        input$Fare == "Høy" ~ 31,
        input$Fare == "Middels" ~ 14.5,
        input$Fare == "Lav" ~ 7.5),
      
      Embarked = as.factor(input$Embarked),
      
      Person_per_ticket = input$family_size,
      
      Minor = as.factor(ifelse(input$Age < 18, 1, 0)),
      
      Family_size = input$family_size,
      
      Alone = as.factor(ifelse(input$family_size == 1, 1, 0))
      )
    
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

