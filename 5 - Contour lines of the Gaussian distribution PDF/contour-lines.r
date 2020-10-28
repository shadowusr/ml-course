library(plotly)
library(htmlwidgets)

gaussPdf <- function (x, mu, E) {
  k <- length(x)
  ans <- exp(-0.5 * t(x - mu) %*% solve(E) %*% (x - mu)) /
    sqrt((2 * pi) ^ k * det(E))
  return (ans)
}


drawPlot <- function () {
  mu = c(0, 0)
  # E <- matrix(c(1, 3/5, 3/5, 2), nrow = 2)
  # E <- matrix(c(0.5, 0, 0, 2), nrow = 2)
  E <- matrix(c(2, 0.9, 0.9, 2), nrow = 2)
  
  xs <- seq(from = -5, to = 5, by = 0.1)
  ys <- seq(from = -5, to = 5, by = 0.1)
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
  fig <- fig %>% layout(
    scene = list(
      camera=list(
        eye = list(x=1.87, y=0.88, z=-0.64)
      )
    )
  )
  #saveWidget(fig, file="widget.html")
  return (fig)
}

drawHeatmap <- function() {
  mu = c(0, 0)
  # E <- matrix(c(1, 3/5, 3/5, 2), nrow = 2)
  # E <- matrix(c(0.5, 0, 0, 2), nrow = 2)
  E <- matrix(c(2, 0.9, 0.9, 2), nrow = 2)
  
  xs <- seq(from = -5, to = 5, by = 0.1)
  ys <- seq(from = -5, to = 5, by = 0.1)
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
    type = 'heatmap',
    x = ~xs,
    y = ~ys,
    z = ~z)
  #saveWidget(fig, file="widget.html")
  return (fig)
}

# drawPlot()
drawHeatmap()

# plotly 3D plotting tests:
# x = c(1,2,3,4,5)
# y = c(1,2,3,4,5)
# z <- c()
# z = rbind(z,
#   c(2, 1, 2.2, 1, 0),
#   c(1, 0, 1, 0, 1))
# 
# z <- rbind(z,   c(0, 1, 0, 1, 0),
#            c(1, 0, 1, 0, 1),
#            c(0, 1, 0, 1, 0))
# 
# fig <- plot_ly(
#   type = 'surface',
#   x = ~x,
#   y = ~y,
#   z = ~z)
# 
# fig