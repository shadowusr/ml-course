library(shiny)
library(matrixcalc)
library(plotly)

ui <- fluidPage(
  titlePanel("Gaussian PDF 3D demo"),
  sidebarLayout(
    sidebarPanel(
      h6("Expected value, μ"),
      fluidRow(
        column(6,
               numericInput("mu1", value = 0, label = "μ1")
        ),
        column(6,
               numericInput("mu2", value = 0, label = "μ2")
        )
      ),
      h6("Covariance matrix, Σ"),
      fluidRow(
        column(6,
               numericInput("E1", value = 2, label = "Σ (1,1)")
        ),
        column(6,
               numericInput("E2", value = 0.9, label = "Σ (1,2)")
        )
      ),
      fluidRow(
        column(6,
               numericInput("E3", value = 0.9, label = "Σ (2,1)")
        ),
        column(6,
               numericInput("E4", value = 2, label = "Σ (2,2)")
        )
      ),
      textOutput("err")
    ),
    mainPanel(plotlyOutput(outputId = "p"))
  )
)