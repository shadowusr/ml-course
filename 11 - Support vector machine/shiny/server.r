library(shiny)
library(matrixcalc)
library(plotly)
library(MASS)
library(kernlab)

dset <- c()
backup <- rep(0, 14)

drawLine = function(w, color, width = 0.5) {
  abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = width, col = color, asp = 1)
}

function(input, output, session, ...) {
  output$err <- function() {
    # E <- matrix(c(input$E1, input$E3, input$E2, input$E4), nrow = 2)
    # if (!matrixcalc::is.positive.definite(E)) {
    #   return("Error: covariance matrix has to be positive definite.")
    # }
    return("")
  }
  
  output$p1 <- renderPlot({
    mu1 <- c(input$mu11, input$mu12)
    mu2 <- c(input$mu21, input$mu22)
    sig1 <- matrix(c(input$E11, input$E12, input$E13, input$E14), nrow = 2, ncol = 2, byrow = TRUE)
    sig2 <- matrix(c(input$E21, input$E22, input$E23, input$E24), nrow = 2, ncol = 2, byrow = TRUE)
    dset1 <- mvrnorm(input$count1, mu1, sig1)
    dset2 <- mvrnorm(input$count2, mu2, sig2)
    newBackup <- c(input$count1, input$count2, input$mu11, input$mu12, input$mu21, input$mu22, input$E11, input$E12, input$E13, input$E14, input$E21, input$E22, input$E23, input$E24)
    if (any(newBackup != backup)) {
      dset <<- rbind(cbind(dset1, 1),cbind(dset2, -1))
      backup <<- newBackup
    }
    
    C <- input$C
    
    svp <- ksvm(as.matrix(dset[, 1:2]), c(dset[,3]),type="C-svc", kernel = "vanilladot", C = C, scaled = c())
    
    supportVectorIndices <- SVindex(svp)
    w <- colSums(coef(svp)[[1]] * dset[supportVectorIndices, 1:2])
    w[3] <- b(svp)
    
    plot(dset[-supportVectorIndices ,1], dset[ -supportVectorIndices,2], pch=21, bg=c("1" = "green","-1" = "blue")[paste(dset[-supportVectorIndices ,3])], xlab="x", ylab="y", main = "SVM")
    points(dset[supportVectorIndices, 1], dset[supportVectorIndices, 2], pch = 24, bg="red", col=c("1" = "green","-1" = "blue")[paste(dset[supportVectorIndices ,3])])
    drawLine(w, "red", width = 3)
  })
  
  output$p2 <- renderPlot({
    mu1 <- c(input$mu11, input$mu12)
    mu2 <- c(input$mu21, input$mu22)
    sig1 <- matrix(c(input$E11, input$E12, input$E13, input$E14), nrow = 2, ncol = 2, byrow = TRUE)
    sig2 <- matrix(c(input$E21, input$E22, input$E23, input$E24), nrow = 2, ncol = 2, byrow = TRUE)
    dset1 <- mvrnorm(input$count1, mu1, sig1)
    dset2 <- mvrnorm(input$count2, mu2, sig2)
    dset <- rbind(cbind(dset1, 1),cbind(dset2, -1))
    
    C <- input$C
    
    svp <- ksvm(as.matrix(dset[, 1:2]), c(dset[,3]),type="C-svc", kernel = "vanilladot", C = C, scaled = c())
    
    supportVectorIndices <- SVindex(svp)
    w <- colSums(coef(svp)[[1]] * dset[supportVectorIndices, 1:2])
    w[3] <- b(svp)
    
    dsetLength <- dim(dset)[1]
    
    d <- matrix(NA, dsetLength, 2)
    
    for (i in 1:dsetLength) {
      d[i, ] <- c(i, sum(w * c(dset[i, 1:2], -1)))
    }
    
    sorted <- dset[order(d[ ,2], decreasing = TRUE), 3]
    
    truePositive <- 0
    falsePositive <- 0
    
    result <- matrix(0, dsetLength+1, 2)
    
    for (i in 1:dsetLength) {
      ans <- sorted[i]
      if (ans == 1) {
        truePositive <- truePositive + 1
      } else {
        falsePositive <- falsePositive + 1
      }
      result[i+1,] <- c(falsePositive, truePositive)
    }
    plot(result[, 1], result[, 2], type="l", xlab="False positive rate", ylab="True positive rate", main = "ROC")
  })
}