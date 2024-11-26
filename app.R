library(shiny)

source("app_data.r") # henter en lasso-modell

ui <- pageWithSidebar(
  headerPanel("Titanic overlevelseskalkulator"),
  sidebarPanel(
                numericInput("Age", label = "Alder", value = 10),
                selectInput("Pclass", label = "Klasse", c("1", "2", "3")),
                selectInput("Sex", label = "Kjønn", c("male", "female")),
                selectInput("Fare", label = "Bilettpris", c("Lav", "Middels", "Høy")),
                selectInput("Embarked", label = "Gikk om bord", 
                            c("Southampton", "Cherbourg", "Queenstown")),
                numericInput("family_size", label = "Familiemedlemmer ombord", value = 1),
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
      
      Fare = case_when(
        input$Fare == "Høy" ~ 31,
        input$Fare == "Middels" ~ 14.5,
        input$Fare == "Lav" ~ 7.5),
      
      Embarked = case_when(
        input$Embarked == "Southampton" ~ as.factor("S"),
        input$Embarked == "Queenstown" ~ as.factor("Q"),
        input$Embarked == "Cherbourg" ~ as.factor("C")
        ),
      
      Person_per_ticket = input$family_size,
      
      Minor = as.factor(ifelse(input$Age < 18, 1, 0)),
      
      Family_size = input$family_size,
      
      Alone = as.factor(ifelse(input$family_size == 1, 1, 0))
      )
    
    output <- predict(lasso_fit, ny_tibble, type = "prob")
  })
  output$prediction_output <- renderText({
    prob <- datasetInput()
    paste0("Sannsynligheten for overlevelse: ", round(prob$.pred_1 * 100, 2), "%")
  })
  
  output$prediction_details <- renderPrint({
    prediction <- datasetInput()
    print(prediction)
  })
  
}

shinyApp(ui, server)

