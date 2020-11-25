library(MASS)

computeWeightsWithStochasticGradient <- function(dset, classes, lossFunction, updateRule) {
 
  dsetLength <- dim(dset)[1]
  featuresCount <- dim(dset)[2]
  
  w <- runif(featuresCount, -1 / (2 * featuresCount), 1 / (2 * featuresCount))
  lambda <- 1 / dsetLength
  
  currentLoss <- 0
  for (i in 1:dsetLength) {
    margin <- sum(w * dset[i, ]) * classes[i]
    currentLoss <- currentLoss + lossFunction(margin)
  }
  prevLoss <- currentLoss
  
  xs <- c()
  ys <- c()
  
  step <- 1
  repeat {
    margins <- array(0, dim = dsetLength)
    for (i in 1:dsetLength) {
      margins[i] <- sum(w * dset[i, ]) * classes[i]
    }
    errorIndices <- which(margins <= 0)
    
    if (length(errorIndices) == 0) break
    
    randomErrorSampleIndex <- sample(errorIndices, 1)
    x <- dset[randomErrorSampleIndex, ]
    class <- classes[randomErrorSampleIndex]
    
    margin <- sum(w * x) * class
    loss <- lossFunction(margin)
    xs <- c(xs, step)
    ys <- c(ys, currentLoss)
    
    learningRate <- 1 / step
    w <- updateRule(w, learningRate, x, class)
    
    currentLoss <- (1 - lambda) * currentLoss + lambda * loss
    
    if (abs(prevLoss - currentLoss) / abs(max(prevLoss, currentLoss)) < 1e-5) break
    
    if (step > 25000) break
    
    prevLoss <- currentLoss
    step <- step + 1
    
    if (step == 10 || step == 50 || step == 100 || step == 500) {
      png(sprintf("sgd/astep-%03d.png", step), width = 700, height = 700, unit = "px", pointsize = 20)
      # cat("Kernel:", kernelName, index, "of", length(kernels), "\n")
      #index <- index + 1
      
      #loo <- parzen_loo(iris[3:4], iris$Species, kernel, hs)
      #plot(hs, loo, type="l", xlab="h", ylab="LOO")
      #points(hs[which.min(loo)], loo[which.min(loo)], pch = 19, col = "red")
      #text(hs[which.min(loo)], loo[which.min(loo)] + 0.05, paste("best:", hs[which.min(loo)]))
      
      plot(dset[ ,1], dset[ ,2], pch=21, bg=c("1" = "green","-1" = "blue")[paste(gdset[ ,3])])
      drawLine(w, "darkgreen")
      #cat("\rDone.                      \n")
      dev.off()
    }
  }
  gsteps <<- gsteps+step
  # print(step)
  # Thanks, T. Orlova!
  plot(xs, ys, type="l", col="red", xlab="step", ylab="loss")
  return(w)
}

adalineLossFunction <- function(margin) (margin - 1) ^ 2

adalineUpdateRule <- function(w, learningRate, sample, class) w - learningRate * (sum(w * sample) - class) * sample 

hebbLossFunction <- function(margin) max(-margin, 0)

hebbUpdateRule <- function(w, learningRate, sample, class) w + learningRate * sample * class


drawLine = function(w, color) {
  abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 2, col = color)
}

# plot(c(0,2,3,4), c(0,2,2,1), type="l", xlab="h", ylab="Loss over steps", col="red", bg="black")
# points(c(1,2,3,4,5), c(2,2,2,2,2), pch=21, col='red', bg='black')

mu1 <- c(0, 0)
mu2 <- c(4, 4)
sig1 <- matrix(c(2, 0.9, 0.9, 2), 2, 2)
sig2 <- matrix(c(0.5, 0, 0, 2), 2, 2)
dset1 <- mvrnorm(250, mu1, sig1)
dset2 <- mvrnorm(250, mu2, sig2)
dset <- rbind(cbind(dset1, 1),cbind(dset2, -1))
dset <- cbind(dset, -1)
gdset <- dset

w1 = computeWeightsWithStochasticGradient(dset[, c(1, 2, 4)], dset[, 3], hebbLossFunction, hebbUpdateRule)

for (i in 1:dim(dset)[1]) {
  if (sum(c(dset[i, 1:2], -1) * w1) * dset[i, 3] < 0) {
    errorsCount <- errorsCount + 1
  }
}

# print(errorsCount)

# plot(dset[ ,1], dset[ ,2], pch=21, bg=c("1" = "green","-1" = "blue")[paste(dset[ ,3])])

# drawLine(w1, "darkgreen")

# iterationsCount <- 100
# 
# gsteps <- 0
# errorsCount <- 0
# 
# for (iter in 1:iterationsCount) {
#   cat("\rIteration: ", iter)
#   mu1 <- c(0, 0)
#   mu2 <- c(4, 4)
#   sig1 <- matrix(c(2, 0.9, 0.9, 2), 2, 2)
#   sig2 <- matrix(c(0.5, 0, 0, 2), 2, 2)
#   dset1 <- mvrnorm(250, mu1, sig1)
#   dset2 <- mvrnorm(250, mu2, sig2)
#   dset <- rbind(cbind(dset1, 1),cbind(dset2, -1))
#   dset <- cbind(dset, -1)
#   gdset <- dset
#   
#   w1 = computeWeightsWithStochasticGradient(dset[, c(1, 2, 4)], dset[, 3], adalineLossFunction, adalineUpdateRule)
#   
#   for (i in 1:dim(dset)[1]) {
#     if (sum(c(dset[i, 1:2], -1) * w1) * dset[i, 3] < 0) {
#       errorsCount <- errorsCount + 1
#     }
#   }
#   # print(errorsCount)
#   
#   plot(dset[ ,1], dset[ ,2], pch=21, bg=c("1" = "green","-1" = "blue")[paste(dset[ ,3])])
#   
#   drawLine(w1, "darkgreen")
# }
# 
# print(errorsCount / iterationsCount)
# print(gsteps / iterationsCount)




# > w1
# [1] -1.0600585 -0.2050553 -3.6053360
# > w[3]  / w[2]
# Error: object 'w' not found
# > w1[3]  / w1[2]
# [1] 17.58226
# > ?abline
# > -w1[1] / w1[2]
# [1] -5.169623
# > 17.58226 -5.169623
# [1] 12.41264
# > 17.58226 -2*5.169623
# [1] 7.243014
# > c(2, 7.243, 0) * w1
# [1] -2.120117 -1.485215  0.000000
# > c(-1, 2, 7.243) * w1
# [1]   1.0600585  -0.4101105 -26.1134488
# > c(2, 7.243, -1) * w1
# [1] -2.120117 -1.485215  3.605336
# > sum(c(2, 7.243, -1) * w1)
# [1] 3.612683e-06
# > paste(sum(c(2, 7.243, -1) * w1))
# [1] "3.61268315218766e-06"



