library(shiny)
library(matrixcalc)
library(plotly)

getLikelihood <- function(point, expectation, covMatrix) {
  pointMinusMean <- t(as.matrix(point - expectation))
  
  covMatrixInverse <- solve(covMatrix)
  nom <- exp(-(pointMinusMean %*% covMatrixInverse %*% t(pointMinusMean)) / 2)
  denom <- sqrt((2 * pi) ^ nrow(covMatrix) * det(covMatrix))
  
  return(nom / denom)
}

getPlugInProbability <- function(point, lambda, prior, expectations, covMatrix) {
  likelihood <- getLikelihood(point, expectations, covMatrix)
  
  probability <- lambda * prior * likelihood;
  
  return (probability)
}

function(input, output, session, ...) {
  output$err <- function() {
    # E <- matrix(c(input$E1, input$E3, input$E2, input$E4), nrow = 2)
    # if (!matrixcalc::is.positive.definite(E)) {
    #   return("Error: covariance matrix has to be positive definite.")
    # }
    return("")
  }
  
  output$p1 <- renderPlotly({
    lambdas <- c(input$la1, input$la2)
    prior <- c(input$ratio / 100, 1 - (input$ratio / 100))
    expectations <- list(
      c(input$mu11, input$mu12),
      c(input$mu21, input$mu22)
    )
    
    covMatrix <- list(
      matrix(c(input$E11, input$E12, input$E13, input$E14), nrow = 2, ncol = 2, byrow = TRUE),
      matrix(c(input$E21, input$E22, input$E23, input$E24), nrow = 2, ncol = 2, byrow = TRUE)
    )
    
    center <- c((input$mu11 + input$mu21) / 2, (input$mu12 + input$mu22) / 2)
    
    
    xs <- seq(from = center[1] - 5, to = center[1] + 5, by = 0.1)
    ys <- seq(from = center[2] - 5, to = center[2] + 5, by = 0.1)
    z <- c()
    
    
    for (y in ys) {
      curr <- c()
      for (x in xs) {
        arg <- c(x, y)
        value <- (getPlugInProbability(c(x, y), lambdas[1], prior[1], expectations[[1]], covMatrix[[1]]) - getPlugInProbability(c(x, y), lambdas[2], prior[2], expectations[[2]], covMatrix[[2]]))
        
        curr <- c(curr, value)
      }
      z <- rbind(z, curr)
    }
    
    
    fig <- plot_ly(
      type = 'contour',
      x = ~xs,
      y = ~ys,
      z = ~z)
    fig
  })
  
  output$p2 <- renderPlotly({
    lambdas <- c(input$la1, input$la2)
    prior <- c(input$ratio / 100, 1 - (input$ratio / 100))
    expectations <- list(
      c(input$mu11, input$mu12),
      c(input$mu21, input$mu22)
    )
    
    covMatrix <- list(
      matrix(c(input$E11, input$E12, input$E13, input$E14), nrow = 2, ncol = 2, byrow = TRUE),
      matrix(c(input$E21, input$E22, input$E23, input$E24), nrow = 2, ncol = 2, byrow = TRUE)
    )
    
    center <- c((input$mu11 + input$mu21) / 2, (input$mu12 + input$mu22) / 2)
    
    
    xs <- seq(from = center[1] - 5, to = center[1] + 5, by = 0.1)
    ys <- seq(from = center[2] - 5, to = center[2] + 5, by = 0.1)
    z <- c()
    
    
    for (y in ys) {
      curr <- c()
      for (x in xs) {
        arg <- c(x, y)
        value <- (getPlugInProbability(c(x, y), lambdas[1], prior[1], expectations[[1]], covMatrix[[1]]) - getPlugInProbability(c(x, y), lambdas[2], prior[2], expectations[[2]], covMatrix[[2]]))
        
        curr <- c(curr, value)
      }
      z <- rbind(z, curr)
    }
    
    
    fig <- plot_ly(
      type = 'surface',
      x = ~xs,
      y = ~ys,
      z = ~z)
    fig
  })
}