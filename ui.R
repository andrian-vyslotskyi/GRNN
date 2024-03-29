library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("GRNN"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("n",
                   "Number of records:",
                   min = 1,
                   max = 100000,
                   value = 10),
      numericInput("train_coef",
                   "Train coeficient (train/records ration)",
                   min = 0,
                   max = 1,
                   value = 0.75,
                   step = 0.1),
      numericInput("sigma",
                   "Sigma",
                   value = 0.1,
                   step = 0.1),
      numericInput("seed",
                   "Pseudo random generator seed",
                   value = 101)
    ),
    
    mainPanel(
      h3("Test data with neural network results:"),
      tableOutput("table"),
      h6("Mean square error for test data:"),
      textOutput("mse")
    )
  )
))
