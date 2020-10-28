library(shiny)
library(matrixcalc)
library(plotly)

gaussPdf <- function (x, mu, E) {
  k <- length(x)
  ans <- exp(-0.5 * t(x - mu) %*% solve(E) %*% (x - mu)) /
    sqrt((2 * pi) ^ k * det(E))
  return (ans)
}

function(input, output, session, ...) {
  output$err <- function() {
    E <- matrix(c(input$E1, input$E3, input$E2, input$E4), nrow = 2)
    if (!matrixcalc::is.positive.definite(E)) {
      return("Error: covariance matrix has to be positive definite.")
    }
    return("")
  }
  
  output$p <- renderPlotly({
    mu = c(input$mu1, input$mu2)
    E <- matrix(c(input$E1, input$E3, input$E2, input$E4), nrow = 2)
    
    if (!matrixcalc::is.positive.definite(E)) {
      plot_ly()
      return()
    }
    
    xs <- seq(from = mu[1] - 5, to = mu[1] + 5, by = 0.1)
    ys <- seq(from = mu[2] - 5, to = mu[2] + 5, by = 0.1)
    z <- c()
    
    
    for (y in ys) {
      curr <- c()
      for (x in xs) {
        arg <- c(x, y)
        value <- gaussPdf(arg, mu, E)
        
        curr <- c(curr, value)
      }
      z <- rbind(z, curr)
    }
    
    
    fig <- plot_ly(
      type = 'surface',
      x = ~xs,
      y = ~ys,
      z = ~z) %>% add_surface(
        contours = list(
          z = list(
            show=TRUE,
            usecolormap=TRUE,
            highlightcolor="#ff0000",
            project=list(z=TRUE)
          )
        )
      )
    fig
  })
}