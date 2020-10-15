euclidean <- function(a, b) sqrt(sum((a - b) ^ 2))

rectKernel <- function(dist, h) if (dist <= h) 0.5 else 0

trKernel <- function(dist, h) if (dist <= h) (h - dist) / h else 0

epKernel <- function(dist, h) if (dist <= h) (1 - dist ^ 2) * 0.75 else 0

gaussKernel <- function(dist, h) exp(sqrt(h / pi * 0.5) * -0.5 * (dist * h) ^ 2)

kvKernel <-function(dist, h) if (dist <= h) 15 / 16 * (1 - (dist / h) ^ 2) ^ 2 else 0

parzen <- function(dset, labels, point, h, kernel, dist = euclidean) {
  dsetLength <- length(dset[,1])
  distances <- array(dim = c(dsetLength))
  
  uLabels <- unique(labels)
  # print("LABBB")
  # print(apply(uLabels, 1, paste))
  classScores <- array(0L, dim = c(length(uLabels)))
  names(classScores) = uLabels;
  classScores["undefined"] <- 0.000001
  
  for (i in 1:dsetLength) {
    classScores[labels[i]] <- classScores[labels[i]] + kernel(dist(point, dset[i,]), h)
  }
  
  return(names(which.max(classScores)))
}

generateMaps <- function() {
  xs <- seq(from = 0.5, to = 7.5, by = 0.1)
  ys <- seq(from = -1, to = 3.5, by = 0.1)
  
  colors <- c("setosa" = "blue", "virginica" = "red", "versicolor" = "green", "undefined" = "grey")
  
  kernels <- c("rect" = rectKernel, "tr" = trKernel, "ep" = epKernel, "gauss" = gaussKernel, "kv" = kvKernel)
  
  # par(mfrow = c(length(kernels) / 2, 2), pty="s", mar=c(1,1,1,1))
  
  index <- 1
  for (kernelName in names(kernels)) {
    kernel <- kernels[[kernelName]]
    png(sprintf("plot-%03d.png", index), width = 1024, height = 700, unit = "px", pointsize = 20)
    cat("Kernel:", kernelName, index, "of", length(kernels), "\n")
    index <- index + 1
    plot(iris[, 3:4], col = colors[paste(iris$Species)], pch=16, asp=1)
    
    all <- length(xs) * length(ys)
    progress = 1
    for (i in xs) {
      for (j in ys) {
        cat("\rProcessing point", progress, "of", all)
        progress <- progress + 1
        result <- parzen(iris[3:4], paste(iris$Species), c(i, j), 0.4, kernel)
        points(i, j, col = colors[result], pch = 21)
      }
    }
    cat("\rDone.                      \n")
    dev.off()
  }
}


parzen_loo <- function (dset, labels, kernel, hs) {
  dsetLen <- dim(dset)[1]
  hLen <- length(hs)
  ans <- array(0L, dim = c(hLen))
  
  for (i in 1:hLen) {
    cat("h:", hs[i], "index:", i, "of", hLen, "\n")
    for (j in 1:dsetLen) {
      cat("\rProcessing sample", j, "of", dsetLen)
      tmpRes <- parzen(dset[-j,], labels[-j], dset[j,], hs[i], kernel)
      if (tmpRes != paste(labels[j])) {
        ans[i] <- ans[i] + 1
      }
    }
    print(ans)
  }
  
  ans <- ans / dsetLen
  return (ans)
}

generateLoos <- function() {
  kernels <- c("tr" = trKernel, "ep" = epKernel, "gauss" = gaussKernel, "kv" = kvKernel)
  hs <- seq(from = 0, to = 6, by = 0.2)
  
  index <- 1
  for (kernelName in names(kernels)) {
    kernel <- kernels[[kernelName]]
    png(sprintf("loo-%03d.png", index), width = 1324, height = 700, unit = "px", pointsize = 20)
    cat("Kernel:", kernelName, index, "of", length(kernels), "\n")
    index <- index + 1
    
    loo <- parzen_loo(iris[3:4], iris$Species, kernel, hs)
    plot(hs, loo, type="l", xlab="h", ylab="LOO")
    points(hs[which.min(loo)], loo[which.min(loo)], pch = 19, col = "red")
    text(hs[which.min(loo)], loo[which.min(loo)] + 0.05, paste("best:", hs[which.min(loo)]))
    
    cat("\rDone.                      \n")
    dev.off()
  }
}


# generateLoos()
generateMaps()

#parzen(iris[3:4], paste(iris$Species), c(4, 1), 0.4, rectKernel)

