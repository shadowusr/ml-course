library(kernlab)
library(MASS)

drawLine = function(w, color, width = 0.5) {
  abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = width, col = color, asp = 1)
}

plotROCCurve <- function(dset, w) {
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
  plot(result[, 1], result[, 2], type="l", xlab="False positive rate", ylab="True positive rate")
}

mu1 <- c(0, 0)
mu2 <- c(2, 2)
sig1 <- matrix(c(2, 0.9, 0.9, 2), 2, 2)
sig2 <- matrix(c(0.5, 0, 0, 2), 2, 2)
dset1 <- mvrnorm(50, mu1, sig1)
dset2 <- mvrnorm(50, mu2, sig2)
dset <- rbind(cbind(dset1, 1),cbind(dset2, -1))
# dset <- cbind(dset, -1)

# m <- matrix(data = c(
#   0, 0,
#   0.2, 0.1,
#   -0.7, 0.2,
#   -0.5, -0.3,
#   0.4, -0.2,
#   
#   2, 2,
#   -2, 2,
#   2, -2,
#   -2, -2,
#   -3, 0,
#   -5, 0,
#   4, 1,
#   3, -4
# ), ncol = 2, byrow = TRUE)
# cl <- c(rep(1, 5), rep(-1, 8))



C <- 0.1

svp <- ksvm(as.matrix(dset[, 1:2]), c(dset[,3]),type="C-svc", kernel = "vanilladot", C = C, scaled = c())

supportVectorIndices <- SVindex(svp)
w <- colSums(coef(svp)[[1]] * dset[supportVectorIndices, 1:2])
w[3] <- b(svp)

plot(dset[-supportVectorIndices ,1], dset[ -supportVectorIndices,2], pch=21, bg=c("1" = "green","-1" = "blue")[paste(dset[-supportVectorIndices ,3])])
# plot(dset[-supportVectorIndices, 1], dset[-supportVectorIndices, 2], pch = 21, bg = "grey", main = "C = 0.1")
points(dset[supportVectorIndices, 1], dset[supportVectorIndices, 2], pch = 24, bg="red", col=c("1" = "green","-1" = "blue")[paste(dset[-supportVectorIndices ,3])])
drawLine(w, "red", width = 3)

# plotROCCurve(dset, w)

# svp <- ksvm(m,cl,type="C-svc")
# plot(svp, data=as.matrix(dset[, 1:2]))
# drawLine(w, "red")


# svp




