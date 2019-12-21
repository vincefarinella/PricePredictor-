library(shiny)
library(MASS)
library(randomForest)

ui <- fluidPage(
  helpText("Please Input the follow specifications of the home."),
  numericInput(inputId = 'OverallQual', label = "Overall Quality (1-10)", value = 5 , min =1 , max =10000, step = 1),
  numericInput(inputId = 'GrLivArea', label = "Total Square Foot", value = 1000 , min =1 , max =10000, step = 1),
  numericInput(inputId = 'GarageCars', label = "Number of Cars For Garage", value = 2 , min =1 , max =10, step = 1),
  numericInput(inputId = 'GarageArea', label = "Square SqFt of garage", value = 200 , min =1 , max =10000, step = 1),
  numericInput(inputId = 'TotalBsmtSF', label = "Total SqFt of Basement", value = 500 , min =1 , max =10000, step = 1),
  numericInput(inputId = 'FirstFlrSF', label = "First Floor SqFt", value = 1000 , min =1 , max =10000, step = 1),
  numericInput(inputId = 'FullBath', label = "Number Of Full Baths", value = 2 , min =1 , max =10, step = 1),
  numericInput(inputId = 'TotRmsAbvGrd', label = "Number Of Rooms", value = 6 , min =1 , max =20, step = 1),
  numericInput(inputId = 'YearBuilt', label = "Year built", value = 1990 , min =1 , max =3000, step = 1),
  
  helpText("The output below will reflect the price of your home."),
  
  # Output: Price of house - RandomForest
  mainPanel(
    tableOutput("values")),
  # Output: Price of house - LinearModel   
  mainPanel(
    tableOutput("value_lm")),
  # Output: Price of house - AveragePrice 
  mainPanel(
    tableOutput("value_AVG"))
)

server <- function(input, output) {
  optimal.model <- readRDS("./final_model.rds")
  lm.fit <- readRDS("./LM_fit.rds")
  
  
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
    
    
    #new data frame with predicted values      
    predictedoutput_LM <- predict(lm.fit, newdata=DF) 
    predictedoutput_RF <- predict(optimal.model, newdata=DF) 
    data.frame(Price_Linear_Model = predictedoutput_LM,
               Price_Random_Forest = predictedoutput_RF,
               Average_Price = (predictedoutput_LM + predictedoutput_RF)/ 2)  
  })
  
  #making output   
  output$values <- renderTable({
    predictedVal()  })
  
  
}

shinyApp(ui, server)

