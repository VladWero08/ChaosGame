library(shiny)

# Define the UI
ui <- fluidPage(
  
  # Add CSS for the app
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href="style.css")
  ),
  
  h1("Chaos Game"),
  
  # In the left will be the input for the user
  # In the right will be generating fractals
  sidebarLayout(
    div(class = "side-bar",
        sidebarPanel(
          # Choose the shape of the fractal
          selectInput("SelectShape", h4("Select shape:"), 
                      choices = list("None" = "none", "Triangle" = "triangle", "Square" = "square", "Pentagon" = "pentagon", "Hexagon" = "hexagon")),
          
          sliderInput("ChaosRatio", h4("Select chaos ratio:"),
                      min = 0, max=1, value=0.05, round=FALSE, step=0.05),
          
          # Choose the speed of the drawing process
          uiOutput("SpeedPoints"),
          
          # Choose number of points to be drawn per step
          uiOutput("NumbersPerStep"),
          
          # Choose the number of points slider
          # It will render only if the shape of the fractal has been chosen
          conditionalPanel(
            condition = "input.SelectShape != 'none'",
            uiOutput("SliderPoints")
          )
          
        )    
    ),
    div(class = "col-sm-8 main-bar",
        plotOutput("mainPlot", width = "750px", height = "750px")
    )
  )
)

# Functions to generate fractals

generateTriangle <- function(ratio){
  numPoints <- 20000
  initPoints <- matrix(NA, ncol = 2, nrow = 3)
  allPoints <- matrix(NA, ncol = 2, nrow = numPoints)
  
  # In the triangle, in the XY axis, the initial points are:
  # (0,0), (1,0), (0.5, 1)
  initPoints[1, ] <- c(0, 0)
  initPoints[2, ] <- c(1, 0)
  initPoints[3, ] <- c(0.5, 1)
  
  # Generate a random point inside the triangle
  # -> this are some boundaries so the random point won't exceed the triangle area
  randX <- runif(1, min = 0.25 ,max = 0.75) 
  randY <- runif(1, min = 0, max = 0.4)
  
  allPoints[1, ] <- c(randX, randY)
  # For every iteration generate a random vertex of the triangle
  initPointsRand <- sample(1:3, numPoints, replace = TRUE)
  
  for(i in 1:(numPoints-1)){
    # Calculate the position of the next point:
    # ( allPoints[i,1], allPoints[i,2]) --> (x,y) coordinates from the previous point
    # (initPoints[initPointsRand[i], 1], initPoints[initPointsRand[i], 2]) --> random generated coordinates 
    # towards one vertex of the triangle
    randX <- (initPoints[initPointsRand[i], 1] - allPoints[i, 1]) * ratio + allPoints[i, 1]
    randY <- (initPoints[initPointsRand[i], 2] - allPoints[i, 2]) * ratio + allPoints[i, 2]
    
    allPoints[i + 1, ] <- c(randX, randY)
  }
  
  return (list(initPoints, allPoints))
}

# Define the server and logic
server <- function(input, output){
  
  # Slider which will animate depending on the value of chosen speed
  # by the user
  output$SliderPoints <- renderUI({
    sliderInput("SliderNrPoints", h4("Select number of points to be drawn:"), 
                min = 1, max=10000, value=5, step = input$NumbersOnStep, animate=animationOptions(interval = input$SelectSpeed))
  })  
  
  # Speed input 
  output$SpeedPoints <- renderUI({
    selectInput("SelectSpeed", h4("Select speed:"), 
                choices = list("Very slow" = 500, "Slow" = 400, "Medium" = 300, "Fast" = 100, "Very fast" = 50))
  })
  
  # Number of points to be drawn in a step
  output$NumbersPerStep <- renderUI({
    sliderInput("NumbersOnStep", h4("Select number of points / step:"),
                min = 1, max = 100, value=5)
  })

  chosenShape <- reactive({
    if(input$SelectShape == "triangle"){ return (generateTriangle(input$ChaosRatio))} 
  })
  
  output$mainPlot <- renderPlot({
    par(bg = "#1B2430")
    tips <- chosenShape()[[1]]
    allPoints <- chosenShape()[[2]]
    
    plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), col = 0, yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
    points(allPoints[1 : input$SliderNrPoints, 1], allPoints[1 : input$SliderNrPoints, 2], pch = 46, col = "#D6D5A8")  
    points(tips[, 1], tips[, 2], pch = 20, cex = 3, col = "white")
  })
  
  
}


shinyApp(ui = ui, server = server)

