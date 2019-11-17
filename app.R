library(shiny)
library(MASS)
library(randomForest)

ui <- fluidPage(
     
      numericInput(inputId = 'OverallQual', label = "Overall Quality (1-10)", value = 5 , min =1 , max =10000, step = 1),
      numericInput(inputId = 'GrLivArea', label = "Total Square Foot", value = 1000 , min =1 , max =10000, step = 1),
      numericInput(inputId = 'GarageCars', label = "Number of Cars For Garage", value = 2 , min =1 , max =10, step = 1),
      numericInput(inputId = 'GarageArea', label = "Square SqFt of garage", value = 200 , min =1 , max =10000, step = 1),
      numericInput(inputId = 'TotalBsmtSF', label = "Total SqFt of Basement", value = 500 , min =1 , max =10000, step = 1),
      numericInput(inputId = 'FirstFlrSF', label = "First Floor SqFt", value = 1000 , min =1 , max =10000, step = 1),
      numericInput(inputId = 'FullBath', label = "Number Of Full Baths", value = 2 , min =1 , max =10, step = 1),
      numericInput(inputId = 'TotRmsAbvGrd', label = "Number Of Rooms", value = 6 , min =1 , max =20, step = 1),
      numericInput(inputId = 'YearBuilt', label = "Year built", value = 1990 , min =1 , max =3000, step = 1),

      helpText("The output below will relect the price of your home"),
#      verbatimTextOutput("value")
      # Main panel for displaying outputs ----
      mainPanel(
  # Output: Price of house ----
       tableOutput("values")
      )
  
)

server <- function(input, output) {
  optimal.model <- readRDS("./final_model.rds")
 
      # submit buttons do not have a value of their own,
      # they control when the app accesses values of other widgets.
      # input$num is the value of the number widget.
   predictedVal <- reactive({   
      DF = data.frame(OverallQual=input$OverallQual,
                      GrLivArea=input$GrLivArea,
                      GarageCars=input$GarageCars,
                      GarageArea=input$GarageArea,
                      TotalBsmtSF=input$TotalBsmtSF,
                      FirstFlrSF=input$FirstFlrSF,
                      FullBath=input$FullBath,
                      TotRmsAbvGrd=input$TotRmsAbvGrd,
                      YearBuilt=input$YearBuilt)
#Random Forest fit    
      predictedoutput <- predict(optimal.model, newdata=DF) 
      data.frame(
        Price = predictedoutput)
    })
   
   output$values <- renderTable({
     predictedVal()
     
   })

}
shinyApp(ui, server)
