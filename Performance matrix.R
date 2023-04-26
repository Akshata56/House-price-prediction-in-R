library(caret)
library(randomForest)

data <- read.csv("new house data.csv")
set.seed(123)
trainIndex <- createDataPartition(data$price, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

price1<-na.omit(data$price)
price1


model <- randomForest(price1 ~ ., data = train)


predictions <- predict(model, newdata = test)


accuracy <- postResample(predictions, test$SalePrice)


library(shiny)

ui <- fluidPage(
  titlePanel("House Price Prediction Model Accuracy"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      h4("Accuracy:"),
      verbatimTextOutput("accuracy_output")
    )
  )
)

server <- function(input, output) {
  output$accuracy_output <- renderPrint({
    paste(round(accuracy * 100, 2), "%")
  })
}

shinyApp(ui = ui, server = server)
