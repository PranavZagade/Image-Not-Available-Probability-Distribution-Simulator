library(shiny)

ui <- fluidPage(
  titlePanel("Probability Distribution Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose Distribution:", 
                  choices = c("Normal", "Uniform", "Binomial")),
      
      sliderInput("n", "Number of Observations:", 
                  min = 10, max = 1000, value = 100),
      
      conditionalPanel(
        condition = "input.dist == 'Normal'",
        sliderInput("mean", "Mean:", min = -10, max = 10, value = 0),
        sliderInput("sd", "Standard Deviation:", min = 0.1, max = 5, value = 1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Uniform'",
        sliderInput("min", "Minimum:", min = 0, max = 10, value = 0),
        sliderInput("max", "Maximum:", min = 1, max = 10, value = 5)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Binomial'",
        sliderInput("size", "Number of Trials:", min = 1, max = 100, value = 10),
        sliderInput("prob", "Probability of Success:", min = 0, max = 1, value = 0.5)
      )
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)



server <- function(input, output) {
  output$distPlot <- renderPlot({
    if (input$dist == "Normal") {
      data <- rnorm(input$n, mean = input$mean, sd = input$sd)
    } else if (input$dist == "Uniform") {
      if (input$min >= input$max) {
        showNotification("Error: Minimum value must be less than Maximum value", type = "error")
        return(NULL)
      }
      data <- runif(input$n, min = input$min, max = input$max)
    } else if (input$dist == "Binomial") {
      if (input$size <= 0 || input$prob < 0 || input$prob > 1) {
        showNotification("Error: Invalid parameters for Binomial distribution", type = "error")
        return(NULL)
      }
      data <- rbinom(input$n, size = input$size, prob = input$prob)
    }
    
    if (length(data) == 0) {
      showNotification("No data to plot. Check input parameters.", type = "error")
      return(NULL)
    }
    
    hist(data, main = paste("Histogram of", input$dist, "Distribution"), 
         xlab = "Values", col = "lightblue", border = "black")
  })
}


shinyApp(ui = ui, server = server)


