library(shiny)
library(matrixcalc)
library(plotly)

ui <- fluidPage(
  titlePanel("SVM demo"),
  sidebarLayout(
    sidebarPanel(
      h3("The first class specs"),
      h6("Expected value, μ"),
      fluidRow(
        column(6,
               numericInput("mu11", value = 1.825, label = "μ1")
        ),
        column(6,
               numericInput("mu12", value = 1.825, label = "μ2")
        )
      ),
      h6("Covariance matrix, Σ"),
      fluidRow(
        column(6,
               numericInput("E11", value = 1.5, label = "Σ (1,1)")
        ),
        column(6,
               numericInput("E12", value = 0.25, label = "Σ (1,2)")
        )
      ),
      fluidRow(
        column(6,
               numericInput("E13", value = 0.25, label = "Σ (2,1)")
        ),
        column(6,
               numericInput("E14", value = 1.5, label = "Σ (2,2)")
        )
      ),
      fluidRow(
        column(12,
               numericInput("count1", value = 50, label = "Samples count")
        )
      ),
      textOutput("err"),
      
      h3("The second class specs"),
      h6("Expected value, μ"),
      fluidRow(
        column(6,
               numericInput("mu21", value = 4.57, label = "μ1")
        ),
        column(6,
               numericInput("mu22", value = 4.57, label = "μ2")
        )
      ),
      h6("Covariance matrix, Σ"),
      fluidRow(
        column(6,
               numericInput("E21", value = 2.36, label = "Σ (1,1)")
        ),
        column(6,
               numericInput("E22", value = 0.11, label = "Σ (1,2)")
        )
      ),
      fluidRow(
        column(6,
               numericInput("E23", value = 0.11, label = "Σ (2,1)")
        ),
        column(6,
               numericInput("E24", value = 2.36, label = "Σ (2,2)")
        )
      ),
      fluidRow(
        column(12,
               numericInput("count2", value = 50, label = "Samples count")
        )
      ),
      fluidRow(
        column(12,
               numericInput("C", value = 1, label = "C parameter")
        )
      )
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput(outputId = "p1")),
        column(6, plotOutput(outputId = "p2"))
      )
    )
  )
)