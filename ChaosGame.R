library(shiny)
library(logger)
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
                      choices = list("None" = "none", "Triangle" = "triangle", "Square" = "square", "Pentagon" = "pentagon", "Hexagon" = "hexagon", "Leaf" = "leaf", "Flower" = "flower")),
          
          conditionalPanel(
            condition = "input.SelectShape != 'leaf'",
            sliderInput("ChaosRatio", h4("Select chaos ratio:"),
                        min = 0, max=1, value=0.05, round=FALSE, step=0.05)
          ),
          
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
  numPoints <- 15000
  initPoints <- matrix(NA, ncol = 2, nrow = 3)
  allPoints <- matrix(NA, ncol = 2, nrow = numPoints)
  
  # In the triangle, in the XY axis, the initial points are:
  # (0,0), (4,0), (2, 4)
  initPoints[1, ] <- c(0, 0)
  initPoints[2, ] <- c(4, 0)
  initPoints[3, ] <- c(2, 4)
  
  # Generate a random point inside the triangle
  # -> this are some boundaries so the random point won't exceed the triangle area
  randX <- runif(1, min = 0.25 ,max = 0.75) 
  randY <- runif(1, min = 0, max = 0.5)
  
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

generateHexagon <- function(ratio){
  numPoints <- 15000
  initPoints <- matrix(NA, ncol = 2, nrow = 6)
  allPoints <- matrix(NA, ncol = 2, nrow = numPoints)
  
  # In the hexagon, in the XY axis, the initial points are:
  initPoints[1, ] <- c(-1 + 2, (-1) * sqrt(3) + 2)
  initPoints[2, ] <- c(1 + 2, (-1) * sqrt(3) + 2)
  initPoints[3, ] <- c(-2 + 2, 0 + 2)
  initPoints[4, ] <- c(2 + 2, 0 + 2)
  initPoints[5, ] <- c(1 + 2, sqrt(3) + 2)
  initPoints[6, ] <- c(-1 + 2, sqrt(3) + 2)
  
  randX <- runif(1, min = 1.05 ,max = 1.95) 
  randY <- runif(1, min = 0.05, max = 1.95)
  allPoints[1, ] <- c(randX, randY)
  
  # For every iteration generate a random vertex of the triangle
  initPointsRand <- sample(1:6, numPoints, replace = TRUE)
  
  for(i in 1:(numPoints-1)){
    randX <- (initPoints[initPointsRand[i], 1] - allPoints[i, 1]) * ratio + allPoints[i, 1]
    randY <- (initPoints[initPointsRand[i], 2] - allPoints[i, 2]) * ratio + allPoints[i, 2]
    
    allPoints[i + 1, ] <- c(randX, randY)
  }
  
  return (list(initPoints, allPoints))
}

generateSquare <-function(ratio){
  numPoints <- 15000
  initPoints <- matrix(NA, ncol = 2, nrow = 4)
  allPoints <- matrix(NA, ncol = 2, nrow = numPoints)
  
  # In the square, in the XY axis, the initial points are:
  # (0,0), (1,0), (0, 1) , (1,1)
  initPoints[1, ] <- c(0, 0)
  initPoints[2, ] <- c(4, 0)
  initPoints[3, ] <- c(0, 4)
  initPoints[4, ] <- c(4, 4)
  
  
  # Generate a random point inside the square
  # -> this are some boundaries so the random point won't exceed the square area
  randX <- runif(1, min = 0.05 ,max = 0.95) 
  randY <- runif(1, min = 0.05, max = 0.95)
  
  allPoints[1, ] <- c(randX, randY)
  # generate first random Vertex
  lastVertexIndex <- sample(1:4, 1)
  currentVertexIndex <- lastVertexIndex
  
  log_info('Loading data')
  for(i in 1:(numPoints-1)){
    # Calculate the position of the next point:
    # ( allPoints[i,1], allPoints[i,2]) --> (x,y) coordinates from the previous point
    # (initPoints[initPointsRand[i], 1], initPoints[initPointsRand[i], 2]) --> random generated (x,y) coordinates 
    # towards one vertex of the square
     
   
    #making sure the same vertex isn't picked two consecutive times 
    while (lastVertexIndex == currentVertexIndex){
      currentVertexIndex = sample(1:4, 1, replace = TRUE)
     #log_info('vertex index picked is {currentVertexIndex }')
     #log_info('last vertex index picked is {lastVertexIndex}')
    }
    #log_info('!!out of while loop!!')
    #getting the X and Y coordinates of the picked Vertex
    choosenVertexX = initPoints[currentVertexIndex, 1]
    choosenVertexY = initPoints[currentVertexIndex, 2]
    
    #updating lastVertexIndex value
    lastVertexIndex = currentVertexIndex
    
    #building the next point (prevPoint + (Vertex-prevpoint)/ratio)
    prevPointX = allPoints[i, 1]
    prevPointY = allPoints[i, 2]
    
    randX <- (choosenVertexX - prevPointX) * ratio + prevPointX
    randY <- (choosenVertexY - prevPointY) * ratio + prevPointY
    #log_info("new point is {randX}")
    #the next point picked
    allPoints[i + 1, ] <- c(randX, randY)
  
  }
  
  #return the vertexes of the square + the points choosen inside the square
  return (list(initPoints, allPoints))
}

valid <-function (value){
  result <- TRUE
  if (value<0.051 * 1.5 && value> 1.287 * 1.5) result<- FALSE
  if (value<1.127 * 1.5 && value> 2.072 * 1.5) result<- FALSE
  if (value<2.206 * 1.5 && value> 1.292 * 1.5) rresult<- FALSE
  if (value<1.798 * 1.5 && value> 0.024 * 1.5) result<- FALSE
  if (value<0.466 * 1.5 && value> 0.021 * 1.5) result<- FALSE

  return (result)
}

generatePentagon <-function(ratio){
  numPoints <- 15000
  initPoints <- matrix(NA, ncol = 2, nrow = 5)
  allPoints <- matrix(NA, ncol = 2, nrow = numPoints)
  
  # setting the values of vertexes
  initPoints[1, ] <- c(0.051 * 1.5, 1.287 * 1.5)
  initPoints[2, ] <- c(1.127 * 1.5, 2.072 * 1.5)
  initPoints[3, ] <- c(2.206 * 1.5, 1.292 * 1.5)
  initPoints[4, ] <- c(1.798 * 1.5, 0.024 * 1.5)
  initPoints[5, ] <- c(0.466 * 1.5, 0.021 * 1.5)
  
  
  # Generate a random point inside the pentagon
  # -> this are some boundaries so the random point won't exceed the area
  randX <- runif(1, min = 0.01 ,max = 1.99) 
  randY <- runif(1, min = 0.01, max = 3.99)
  while(valid(randX) == FALSE){
    randX <- runif(1, min = 0.01 ,max = 1.99) 
  }
  while(valid(randY) == FALSE){
    randY <- runif(1, min = 0.01, max = 3.99)
  }

  allPoints[1, ] <- c(randX, randY)
  
  #  log_info('Loading data')
  for(i in 1:(numPoints-1)){
    # Calculate the position of the next point:
    # ( allPoints[i,1], allPoints[i,2]) --> (x,y) coordinates from the previous point
    # (initPoints[initPointsRand[i], 1], initPoints[initPointsRand[i], 2]) --> random generated (x,y) coordinates 
    # towards one vertex of the square
    vertexIndex = sample(1:5, 1, replace = TRUE)
  
    choosenVertexX = initPoints[vertexIndex, 1]
    choosenVertexY = initPoints[vertexIndex, 2]
  
    #building the next point (prevPoint + (Vertex-prevpoint)/ratio)
    prevPointX = allPoints[i, 1]
    prevPointY = allPoints[i, 2]
    
    randX <- (choosenVertexX - prevPointX) * ratio + prevPointX
    randY <- (choosenVertexY - prevPointY) * ratio + prevPointY
      
    #the next point picked
    allPoints[i + 1, ] <- c(randX, randY)
  }
  
  #return the vertexes of the pentagon + the points choosen inside it
  return (list(initPoints, allPoints))
}

generateLeaf <- function(){
  numPoints <- 15000
  # Still return initpoints, to keep the same logic for the program
  initPoints <- matrix(NA, ncol = 2, nrow = 1)
  allPoints <- matrix(NA, ncol = 2, nrow = numPoints)
  allPointsReversed <- matrix(NA, ncol = 2, nrow = numPoints)
  
  # Generate a random point inside the triangle
  # -> this are some boundaries so the random point won't exceed the triangle area
  initPoints[1, ] <- c(0, 0)
  randX <- 0
  randY <- 0
  
  # Using the Barnsley fern algorithm, for each of 15000 steps, need
  initPointsRand <- sample(1:4, numPoints, replace = TRUE, prob=c(0.01, 0.85, 0.07, 0.07))
  
  for(i in 1:(numPoints-1)){
    
    if(initPointsRand[i] == 1){
      randX <- 0 
      randY <- 0.16*randY
    } else if(initPointsRand[i] == 2){
      randX <- 0.85*randX + 0.04*randY  
      randY <- -0.04*randX + 0.85*randY + 1.6
    } else if(initPointsRand[i] == 3){
      randX <- 0.2*randX - 0.26*randY
      randY <- 0.23*randX + 0.22*randY + 1.6
    } else{
      randX <- -0.15*randX + 0.28*randY
      randY <- 0.26*randX + 0.24*randY + 0.44
    }
    
    # Rotate the fractal generate 45 degrees to the right of the (0,0)
    allPoints[i + 1, ] <- c((-0.45) * (randX * cos(45) - randY * sin(45)), 0.45 * (randY * cos(45) + randX * sin(45)))
  }
  
  return (list(initPoints, allPoints))
}

generateFlower <- function(){
  numPoints <- 15000
  # Still return initpoints, to keep the same logic for the program
  initPoints <- matrix(NA, ncol = 2, nrow = 1)
  allPoints <- matrix(NA, ncol = 2, nrow = numPoints * 2)
  allPointsReversed <- matrix(NA, ncol = 2, nrow = numPoints)
  
  # Generate a random point inside the triangle
  # -> this are some boundaries so the random point won't exceed the triangle area
  initPoints[1, ] <- c(0, 0)
  randX <- 0
  randY <- 0
  
  # Using the Barnsley fern algorithm, for each of 15000 steps, need
  initPointsRand <- sample(1:4, numPoints, replace = TRUE, prob=c(0.01, 0.85, 0.07, 0.07))
  
  i <- 1
  while(i < numPoints - 1){
    
    if(initPointsRand[i] == 1){
      randX <- 0 
      randY <- 0.16*randY
    } else if(initPointsRand[i] == 2){
      randX <- 0.85*randX + 0.04*randY  
      randY <- -0.04*randX + 0.85*randY + 1.6
    } else if(initPointsRand[i] == 3){
      randX <- 0.2*randX - 0.26*randY
      randY <- 0.23*randX + 0.22*randY + 1.6
    } else{
      randX <- -0.15*randX + 0.28*randY
      randY <- 0.26*randX + 0.24*randY + 0.44
    }
    
    # Rotate the fractal generate 45 degrees to the right of the (0,0)
    allPoints[i, ] <- c((-0.45) * (randX * cos(45) - randY * sin(45)), 0.45 * (randY * cos(45) + randX * sin(45)))
    allPoints[i + 1, ] <- c((0.45) * (randX * cos(45) - randY * sin(45)), 0.45 * (randY * cos(45) + randX * sin(45)))  
    
    i <- i + 2
  }
  
  return (list(initPoints, allPoints))
}

# Define the server and logic
server <- function(input, output){
  
  # Slider which will animate depending on the value of chosen speed
  # by the user
  output$SliderPoints <- renderUI({
    sliderInput("SliderNrPoints", h4("Select number of points to be drawn:"), 
                min = 1, max=15000, value=5, step = input$NumbersOnStep, animate=animationOptions(interval = input$SelectSpeed))
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
    if(input$SelectShape == "triangle"){ 
      return (generateTriangle(input$ChaosRatio))} 
    if(input$SelectShape == "square"){ 
      return (generateSquare(input$ChaosRatio))} 
    if(input$SelectShape == "pentagon"){ 
      return (generatePentagon(input$ChaosRatio))} 
    if(input$SelectShape == "hexagon"){ 
      return (generateHexagon(input$ChaosRatio))} 
    if(input$SelectShape == "leaf"){ 
      return (generateLeaf())} 
    if(input$SelectShape == "flower"){ 
      return (generateFlower())} 
  })
  
  output$mainPlot <- renderPlot({
    par(bg = "#1B2430")
    tips <- chosenShape()[[1]]
    allPoints <- chosenShape()[[2]]
    
    plot(0, 0, xlim = c(-8, 8), ylim = c(-8, 8), col = 0, yaxt = "n", xaxt = "n", xlab = "", ylab = "", bty = "n")
    points(allPoints[1 : input$SliderNrPoints * 2, 1], allPoints[1 : input$SliderNrPoints * 2, 2], pch = 46, col = "#D6D5A8")  
    points(tips[, 1], tips[, 2], pch = 20, cex = 3, col = "white")
  })
}

shinyApp(ui = ui, server = server)

